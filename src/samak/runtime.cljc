(ns samak.runtime
  #?@
  (:clj
   [(:require
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put!]]
     [samak.runtime.stores  :as stores]
     [samak.runtime.servers :as servers]
     [samak.helpers :as helpers]
     [samak.tools :refer [fail log]]
     [samak.trace :as trace]
     [samak.pipes :as pipes]
     [samak.transduction-tools :as tt]
     [samak.nodes :as n]
     [samak.api :as api]
     [samak.code-db :as db])]
   :cljs
   [(:require
     [clojure.core.async :as a :refer [<! >! chan close! put!]]
     [samak.runtime.stores  :as stores]
     [samak.runtime.servers :as servers]
     [samak.helpers :as helpers]
     [samak.tools :refer [fail log]]
     [samak.trace :as trace]
     [samak.pipes :as pipes]
     [samak.transduction-tools :as tt]
     [samak.nodes :as n]
     [samak.api :as api]
     [samak.code-db :as db])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))


(def resolver (atom {}))
(def cancel-conditions (atom {}))

(defn cancel?
  ""
  [paket]
  (let [cancel-id (:samak.pipes/cancel (:samak.pipes/meta paket))
        cancel-cond (get @cancel-conditions cancel-id)
        timeout (:timeout cancel-cond)
        cancel (and timeout (helpers/past? timeout))]
    cancel))

(defn set-cancellation-condition
  ""
  [id condition]
  (swap! cancel-conditions update id merge condition))

(defn eval-all [server forms]
  (reduce (fn [server form]
            (swap! resolver #(assoc % :server server))
            (servers/eval-ast server form))
          server forms))

(defn resolve-fn
  ""
  [id]
  (let [defs (servers/get-defined (:server @resolver))
        fn (get defs id)]
    ;; (println "found: " id fn)
    (if fn
      fn
      (println "not found: " id))))

(defn wrap-out
  ""
  [target]
  (fn [paket]
    {::type ::paket ::target (:named target) ::content paket}))

(defn wrap-in
  ""
  [wrapped]
  (fn [paket]
    (let [type (::type paket)
          target (subs (name (::target paket)) 3)]
      (if (and (= type ::paket) (= target (name (:named wrapped))))
        (do
          ;; (println "wrap-in" wrapped target)
          (::content paket))
        ::ignore)))) ;;FIXME


(defn replace-piped
  ""
  [{target :target :as pipe} dir]
  (if (not= target :pipe)
    pipe
    (do
      (println "replacing " pipe)
      (let [from-scheduler (:scheduler @resolver)
            trans-in (pipes/transduction-pipe (comp (map (wrap-in pipe)) (remove #(= % ::ignore))))
            to-world (:broadcast @resolver)
            trans-out (pipes/transduction-pipe (map (wrap-out pipe)))
            in-mapped (pipes/link! from-scheduler trans-in)
            out-mapped (pipes/link! trans-out to-world)]
        (pipes/composite-pipe out-mapped in-mapped)))))


(defn link-fn
  ""
  [from to xf]
  (let [a (replace-piped from "from")
        c (replace-piped to "to")]
    (when (not a)
      (fail "cant link from " from))
    (when (not c)
      (fail "cant link to " to))
    (if xf
      (pipes/link! (pipes/link! a xf) c)
      (pipes/link! a c))))

(defn make-runtime-internal
  ""
  [scheduler]
  (let [c (pipes/pipe (chan))]
     {:id (str "rt-" (helpers/uuid))
      :store  (stores/make-local-store)
      :server (servers/make-local-server {:resolve resolve-fn :link link-fn :cancel? cancel?})
      :broadcast c
      :scheduler (when scheduler (scheduler c))}))

(defn make-runtime
  ([]
   (make-runtime nil nil))
  ([builtins]
   (make-runtime builtins nil))
  ([builtins scheduler]
   (let [runtime (-> (make-runtime-internal scheduler)
                     (update :store stores/load-builtins! (keys builtins))
                     (update :server servers/load-builtins! builtins))
         rt2 (->> (keys builtins)
                  (map (partial stores/resolve-name (:store runtime)))
                  (map (partial stores/load-by-id (:store runtime)))
                  (update runtime :server eval-all))]
     (reset! resolver rt2)
     rt2)))

(defn link-storage
  ""
  [rt storage]
  (assoc rt :store storage))


;; Toplevel code transformation

;; This covers the ::n/def case as well
(defn ast->tx-records [ast]
  [(assoc ast :db/id -1)])

;; Applying the transformation and wrap network

;; (defn wrap-network [network-name forms]
;;   (map (partial api/network network-name) forms))

(defn rewrite-expression [network-name form]
  (->> form
       ast->tx-records
       #_(wrap-network network-name)))

;; Evaluation - Dumb and without dependency resolution for now

(defn persist-to-ids! [store tx-records]
  (-> (stores/persist-tree! store tx-records)
      :tempids
      (dissoc :db/current-tx)
      vals))

(defn store!
  [store tx-records]
  (->> tx-records
       (persist-to-ids! store)
       (map (partial stores/load-by-id store))))

(defn store-and-eval!
  [{store :store server :server :as rt} tx-records]
  (reset! resolver rt)
  (->> tx-records
       (store! store)
       (eval-all server)))

(defn load-by-id
  ""
  [{store :store} id]
  (stores/load-by-id store id))

(defn load-by-sym
  ""
  [{store :store} sym]
  (when-let [ref (stores/resolve-name store sym)]
    (stores/load-by-id store ref)))

(defn load-network
  "loads the given network from storage"
  [{store :store :as rt} id]
  (stores/load-network store id))

(defn load-bundle
  "loads the definition of a bundle"
  [{store :store :as rt} sym]
  (let [defns (load-by-sym rt sym)
        kv (get-in defns [:samak.nodes/rhs :samak.nodes/mapkv-pairs])
        sources (:samak.nodes/mapkv-pairs (:samak.nodes/mapvalue (first kv))) ;; FIXME, move to db?
        value (map #(get-in %1 [:samak.nodes/mapvalue :samak.nodes/fn :db/id]) sources)]
    value))


(defn eval-expression! [{:keys [store server] :as rt} form]
  (let [new-server (store-and-eval! rt (rewrite-expression "user" form))]
    (assoc rt :server new-server)))

(defn get-definition-by-name [runtime sym]
  (let [id (-> runtime :store (stores/resolve-name sym))]
    (-> runtime :server servers/get-defined (get id))))

(defn fire-into-named-pipe
  ""
  [rt pipe-name data timeout]
  (let [pipe (get-definition-by-name rt pipe-name)]
    (if (pipes/pipe? pipe)
      (let [paket (pipes/make-paket data ::fire)
            cancel-id (:samak.pipes/cancel (:samak.pipes/meta paket))]
        (when (> timeout 0)
          (set-cancellation-condition cancel-id {:timeout (helpers/future-ms timeout)}))
        (trace/trace ::fire 0 paket)
        (pipes/fire-raw! pipe paket))
      {:error (str "could not find pipe " pipe-name)})))
