(ns samak.runtime
  #?@
  (:clj
   [(:require
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put!]]
     [clojure.walk :as w]
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
     [clojure.walk :as w]
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

(defn eval-all [server forms]
  (reduce (fn [server form]
            (swap! resolver #(assoc % :server server))
            ;; (println "form" (:db/id form) "->" form)
            (servers/eval-ast server form))
          server forms))


(defn load-by-id
  ""
  [{store :store} id]
  (stores/load-by-id store id))

(defn load-ast
  "loads an ast given by its entity id from the database"
  [rt id]
  (w/postwalk (fn [form]
                  (if-let [sub-id (when (and (map? form) (= (keys form) [:db/id]))
                                    (:db/id form))]
                    (load-by-id rt sub-id)
                    form))
              (load-by-id rt id)))


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


(defn resolve-fn
  ([id]
   (resolve-fn @resolver id))
  ([rt id]
   (let [defs (servers/get-defined (:server rt))
         fn (get defs id)]
     fn
     ;; (if fn
     ;;   (do
     ;;     ;; (println "resolved " id "-> " fn)
     ;;     fn)
     ;;   (println "not evaluated: " id " -> " (stores/load-by-id (:store rt) id)))
     )))

(defn wrap-out
  ""
  [target id]
  (fn [paket]
    ;; (println id "wrap-out paket" target paket)
    {::type ::paket ::target (:named target) ::content paket}))

(defn wrap-in
  ""
  [wrapped id]
  (fn [paket]
    ;; (println id "wrap-in paket" wrapped paket)
    (let [type (::type paket)
          target (::target paket)]
      (if (and (= type ::paket) (= target (:name (:named wrapped))))
        (do
          ;; (println "wrap-in target" target (::content paket))
          (::content paket))
        ::ignore)))) ;;FIXME


(defn replace-piped
  ""
  [{target :target :as pipe}]
  (if (not= target :pipe)
    pipe
    (do
      (println "replacing " pipe)
      (let [from-scheduler (:scheduler @resolver)
            trans-in (pipes/transduction-pipe (comp (map (wrap-in pipe (:id @resolver))) (remove #(= % ::ignore))))
            to-world (:broadcast @resolver)
            trans-out (pipes/transduction-pipe (map (wrap-out pipe (:id @resolver))))
            in-mapped (pipes/link! from-scheduler trans-in)
            out-mapped (pipes/link! trans-out to-world)]
        (pipes/composite-pipe out-mapped in-mapped)))))

(defn link-fn
  ""
  [from to xf]
  (println "linking" from to)
  (let [a (replace-piped from)
        c (replace-piped to)]
    (when (not (pipes/pipe? a))
      (fail "cant link from " from))
    (when (not (pipes/pipe? c))
      (fail "cant link to " to))
    (if xf
      (pipes/link! (pipes/link! a xf) c)
      (pipes/link! a c))))

(defn instanciate-module
  ""
  [{:keys [:samak.nodes/definition] :as module} man]
  (let [n (str (:samak.nodes/name module))
        c (get (:config man) n)]
    (if c
      (fn []
        (println "return stub for" n "[" (:db/id module) "] -> " c)
        c)
      (fn []
        ;; FIXME
        ;; needs to prep resolve magic when instanciating pipes, to select same runtime
        ;; maybe simply do so explicitly
        ;; (if (:config man))
        (println  (str "about to eval module: " module))
        (let [evaled (n/eval-env man nil definition (:db/id module))]
          (println (str "used module: " module "->" evaled))
          evaled)))))

(defn make-runtime-internal
  ""
  [scheduler conf]
  (let [c (pipes/pipe (chan))]
     {:id (str "rt-" (helpers/uuid))
      :store  (stores/make-local-store)
      :server (servers/make-local-server {:config conf
                                          :resolve resolve-fn
                                          :link link-fn
                                          :cancel? cancel?
                                          :module instanciate-module})
      :broadcast c
      :scheduler (when scheduler (scheduler c))}))

(defn make-runtime
  ([]
   (make-runtime nil nil))
  ([builtins]
   (make-runtime builtins nil))
  ([builtins scheduler]
   (make-runtime builtins scheduler {}))
  ([builtins scheduler conf]
   (let [runtime (-> (make-runtime-internal scheduler conf)
                     (update :store stores/load-builtins! (keys builtins))
                     (update :server servers/load-builtins! builtins))
         rt2 (->> (keys builtins)
                  (map (partial stores/resolve-name (:store runtime)))
                  (map (partial load-by-id runtime))
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

(defn load-by-sym
  ""
  [{store :store :as rt} sym]
  (when-let [ref (stores/resolve-name store sym)]
    (load-by-id rt ref)))

(defn load-network
  "loads the given network from storage"
  [{store :store :as rt} id]
  (stores/load-network store id))

(defn get-id-from-source-val
  [entry]
  (let [val (:samak.nodes/mapvalue entry)
        fn (if (:samak.nodes/fn-expression val) (:samak.nodes/fn-expression val) val)]
    (get-in fn [:samak.nodes/fn :db/id])))


(defn get-ids-from-source-def
  [def type-set]
  (let [deps (filter #(type-set (:samak.nodes/value (:samak.nodes/mapkey %))) def)
        ;; _ (println "deps" deps)
        sources (mapcat #(:samak.nodes/mapkv-pairs (:samak.nodes/mapvalue %)) deps)] ;; FIXME, move to db?
    ;; (println "sources" sources)
    sources))


(defn load-roots-from-bundle
  ""
  [rt id defns]
  (let [defs (if (= (:samak.nodes/type defns) :samak.nodes/def)
                (:samak.nodes/rhs defns)
                (:samak.nodes/definition defns))
        ;; _ (println "id" id "- defns" defns)
        kvs (:samak.nodes/mapkv-pairs defs)
        ;; _ (println "kvs" kvs)
        sources (get-ids-from-source-def kvs #{:sources})
        deps (get-ids-from-source-def kvs #{:depends})
        source-ids (apply sorted-set (map get-id-from-source-val sources))
        _ (println "source-ids:" source-ids)
        dep-ids (mapv get-id-from-source-val deps)
        _ (println "dep-ids" dep-ids)
        deps-source-ids (mapv (fn [dep]
                                (println "dep" dep)
                                (load-roots-from-bundle rt dep (load-by-id rt dep)))
                              dep-ids)
        _ (println "dep-s-id" deps-source-ids)
        roots {id {:depends dep-ids
                   :dependencies deps-source-ids
                   :roots source-ids}}]
    (println "roots: " roots)
    roots))


(defn load-bundle
  "loads the definition of a bundle by the given id"
  [rt id]
  (let [defns (load-by-id rt id)]
    (load-roots-from-bundle rt id defns)))


(defn eval-expression! [{:keys [store server] :as rt} form]
  (let [new-server (store-and-eval! rt (rewrite-expression "user" form))]
    (assoc rt :server new-server)))

(defn resolve-name [runtime sym]
  (-> runtime :store (stores/resolve-name sym)))

(defn get-definition-by-id [runtime id]
  (when id
    (-> runtime :server servers/get-defined (get id))))

(defn get-definition-by-name [runtime sym]
  (let [id (-> runtime :store (stores/resolve-name sym))]
    (get-definition-by-id runtime id)))


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
