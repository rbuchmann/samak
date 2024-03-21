(ns samak.runtime
  #?@
  (:clj
   [(:require
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put!]]
     [promesa.core :as p]
     [samak.runtime.stores  :as stores]
     [samak.runtime.servers :as servers]
     [samak.helpers :as helpers]
     [samak.tools :refer [fail log]]
     [samak.trace :as trace]
     [samak.pipes :as pipes]
     [samak.conveyor :as conv]
     [samak.transduction-tools :as tt]
     [samak.nodes :as n]
     [samak.api :as api]
     [samak.code-db :as db])]
   :cljs
   [(:require
     [clojure.core.async :as a :refer [<! >! chan close! put!]]
     [promesa.core :as p]
     [samak.runtime.stores  :as stores]
     [samak.runtime.servers :as servers]
     [samak.helpers :as helpers]
     [samak.tools :refer [fail log]]
     [samak.trace :as trace]
     [samak.pipes :as pipes]
     [samak.conveyor :as conv]
     [samak.transduction-tools :as tt]
     [samak.nodes :as n]
     [samak.api :as api]
     [samak.code-db :as db])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))


(def cancel-conditions (atom {}))
(def pipe-links (atom {}))

(defn eval-all
  [server forms ctx]
  (reduce (fn [server form]
            ;; (println "form" (:db/id form) "->" form)
            (servers/eval-ast server form ctx))
          server forms))

(defn resolve-name [runtime sym]
  (-> runtime :store (stores/resolve-name sym)))

(defn load-by-id
  ""
  [{store :store :as rt} id]
  (if (nil? id)
    (fail "tried to load nil")
    (stores/load-by-id store id)))

(defn load-ast
  "loads an ast given by its entity id from the database"
  [rt id]
  (helpers/ppostwalk (fn [form]
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
  ([rt id]
   (println "resolve" (:id rt) id)
   (get (servers/get-defined (:server rt)) id)))

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
  [{target :target :as pipe} id broadcast inbound]
  (if (not= target :pipe)
    pipe
    (do
      (println "### replacing " pipe)
      (let [trans-in (pipes/transduction-pipe (comp (map (wrap-in pipe id)) (remove #(= % ::ignore))) (str "trans-in-" pipe))
            trans-out (pipes/transduction-pipe (map (wrap-out pipe id)) (str "trans-out-" pipe))
            in-mapped (pipes/link! inbound trans-in)
            out-mapped (pipes/link! trans-out broadcast)]
        (pipes/composite-pipe out-mapped in-mapped)))))

(defn link-fn
  ""
  [id broadcast inbound]
  (fn [from to xf db-id]
    (println "[" id "]" "link" from to xf db-id)
    (let [a (replace-piped from id broadcast inbound)
          c (replace-piped to id broadcast inbound)
          _ (when (and (not (pipes/pipe? a)) (not (conv/station? a)))
              (fail "cant link from " from))
          _ (when (and (not (pipes/pipe? c)) (not (conv/station? c)))
              (fail "cant link to " to))
          ;; xi (when xf (pipes/instrument db-id cancel? xf))
          x (when xf (conv/transduction-pipe xf (str (pipes/uuid a) "-" db-id "-" (pipes/uuid c)) cancel?))
          l (if x
              (conv/link! (conv/link! a x) c)
              (conv/link! a c))
          from-uuid (if (pipes/pipe? from) (pipes/uuid from) (or (:named from) from))
          to-uuid (if (pipes/pipe? to) (pipes/uuid to) (or (:named to) to))]
      (println "[" id "]" "### linking" from-uuid to-uuid)
      (swap! pipe-links assoc (str from-uuid "|" to-uuid) l)
      l)))

(defn instanciate-module
  ""
  [{:keys [:samak.nodes/definition] :as module} man ctx]
  (let [n (str (:samak.nodes/name module))
        c (get (:config man) n)
        id (str n "-" (helpers/uuid))]
    (println "inst mod ->" id ctx)
    (if c
      (fn []
        (println "return stub for" n "[" (:db/id module) "] -> " c)
        c)
      (do (println  (str "### about to eval module: " id module))
          (let [ns-reg #(do (println "ns reg ->" id %1 %2) ((:register man) (str id "/" %1) %2))
                ns-res #(do (println "ns res ->" id %1) (or ((:resolve man) (str id "/" %1)) ((:resolve man) %1)))
                ns-man (merge man {:register ns-reg :resolve ns-res})
                evaled (n/eval-env ns-man nil definition {:db-id (:db/id module) :ctx id})] ;;;FIXME should be server/eval-ast?
            (fn [a]
              ;; FIXME
              ;; needs to prep resolve magic when instanciating pipes, to select same runtime
              ;; maybe simply do so explicitly
              ;; (if (:config man))
              (println "### used module: " ctx "/" n "/" a "---" module "->" evaled)
              evaled))))))

(defn make-store-internal
  ""
  [conf inbound broadcast builtins]
  (if (:store conf)
    (stores/make-piped-store (:id conf) inbound broadcast)
    (let [store (stores/make-local-store (:id conf))]
      (stores/load-builtins! store (keys builtins))
      (stores/serve-store store inbound broadcast))))


(defn make-runtime-internal
  ""
  [scheduler conf builtins]
  (let [[inbound broadcast] (scheduler)
        id (or (:id conf) (str "rt-" (helpers/uuid)))
        manager {:config (:modules conf)
                 :link (link-fn id broadcast inbound)
                 :cancel? cancel?
                 :module instanciate-module}]
    (reset! conv/id id)
    {:id id
     :store (make-store-internal conf inbound broadcast builtins)
     :manager manager
     :server (servers/make-local-server manager)
     :broadcast broadcast
     :scheduler inbound}))

(defn make-runtime
  ([]
   (make-runtime nil))
  ([builtins]
   (make-runtime builtins (fn [] [(pipes/pipe (chan) identity)
                                  (pipes/pipe (chan) println)])))
  ([builtins scheduler]
   (make-runtime builtins scheduler {}))
  ([builtins scheduler conf]
   (p/let [prep (make-runtime-internal scheduler conf builtins)
           runtime (update prep :server servers/load-builtins! builtins)
           build-in-names (p/all (map #(p/do! %1) (map (partial resolve-name runtime) (keys builtins))))
           asts (p/all (map (partial load-by-id runtime) build-in-names))]
     (update runtime :server eval-all asts ""))))

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

(defn persist-to-ids!
  ""
  [store tx-records]
  (stores/persist-tree! store tx-records))


(defn store!
  [store tx-records]
  (p/let [ids (persist-to-ids! store tx-records)]
    (p/all (map (partial stores/load-by-id store) ids))))

(defn store-and-eval!
  [{store :store server :server :as rt} tx-records ctx]
  (p/let [asts (store! store tx-records)]
    (eval-all server asts ctx)))

(defn load-by-sym
  ""
  [rt sym]
  (p/let [ref (resolve-name rt sym)]
    (when ref
      (load-by-id rt ref))))

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


(defn load-def-from-bundle
  ""
  [rt id defns]
  (p/let [defs (if (= (:samak.nodes/type defns) :samak.nodes/def)
                 (:samak.nodes/rhs defns)
                 (:samak.nodes/definition defns))
          kvs (:samak.nodes/mapkv-pairs defs)
          sources (get-ids-from-source-def kvs #{:sources})
          source-ids (apply sorted-set (map get-id-from-source-val sources))
          _ (println "### source-ids:" source-ids)
          sinks (get-ids-from-source-def kvs #{:sinks})
          sink-ids (apply sorted-set (map get-id-from-source-val sinks))
          _ (println "### sink-ids:" sink-ids)
          deps (get-ids-from-source-def kvs #{:depends})
          dep-ids (mapv get-id-from-source-val deps)
          _ (println "### dep-ids" dep-ids)
          deps-source-ids (p/all (map (fn [dep]
                                        (println "### dep" dep)
                                        (p/let [ast (load-by-id rt dep)]
                                          (load-def-from-bundle rt dep ast)))
                                       dep-ids))
          _ (println "### dep-s-id" deps-source-ids)
          def {id {:depends dep-ids
                   :dependencies deps-source-ids
                   :sinks sink-ids
                   :roots source-ids}}]
    (println "### def: " def)
    def))


(defn load-bundle
  "loads the definition of a bundle by the given id"
  [rt id]
  (js-debugger)

  (p/let [defns (load-by-id rt id)]
    (println "defns:" defns)
    (load-def-from-bundle rt id defns)))


(defn eval-expression! [{:keys [store server] :as rt} form ctx]
  (when form
      (do
        ;; (println "eval exp" form ctx rt)
        (p/let [new-server (store-and-eval! rt (rewrite-expression "user" form) ctx)]
    (assoc rt :server new-server)))))

(defn get-definition-by-id [runtime id]
  (when id
    (let [defs (-> runtime :server servers/get-defined)]
      (get defs id))))

(defn get-definition-by-name [runtime ctx sym]
  (p/let [id (resolve-name runtime sym)]
    (println "id" id)
    (get-definition-by-id runtime (str ctx "/" id))))


(defn fire-into-named-pipe
  ""
  [rt ctx pipe-name data timeout]
  (println "§§§§§§ fire" (:id rt) ctx pipe-name data)
  (p/let [pipe (get-definition-by-name rt ctx pipe-name)]
    (do
      (println "pipes" @pipe-links)
      (println "firing" pipe-name)
      (println (:id rt) "pipeis" pipe)
      (if (conv/fire? pipe)
       (let [paket (conv/fire! pipe data nil)
             cancel-id (:samak.pipes/cancel (:samak.pipes/meta paket))]
         (when (> timeout 0)
           (set-cancellation-condition cancel-id {:timeout (helpers/future-ms timeout)}))
         (trace/trace ::fire 0 paket)
         (println "fired"))
       (ex-info "could not find pipe" {:pipe-name pipe-name})))))

(defn links [rt]
  @conv/links)
