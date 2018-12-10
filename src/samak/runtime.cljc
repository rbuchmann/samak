(ns samak.runtime
  (:require [samak.runtime.stores  :as stores]
            [samak.runtime.servers :as servers]
            [samak.nodes           :as n]
            [samak.api             :as api]
            [samak.code-db         :as db]))

(defn eval-all [server forms]
  (reduce servers/eval-ast server forms))

(defn make-runtime
  ([]
   {:store  (stores/make-local-store)
    :server (servers/make-local-server)})
  ([builtins]
   (let [runtime (-> (make-runtime)
                     (update :store stores/load-builtins! (keys builtins))
                     (update :server servers/load-builtins! builtins))]
     (->> (keys builtins)
          (map (partial stores/resolve-name (:store runtime)))
          (map (partial stores/load-by-id (:store runtime)))
          (update runtime :server eval-all)))))

;; Toplevel code transformation

(defmulti transform-expression ::n/type)

(defmethod transform-expression ::n/pipe [ast]
  (->> ast ::n/arguments (sort-by :order) (map ::n/node)
       (partition 2 1)
       (map-indexed (fn [i [a b]]
                      {::n/type ::n/link
                       ::n/from a
                       ::n/to   b
                       :db/id   (- (inc i))}))))

;; This covers the ::n/def case as well
(defmethod transform-expression :default [ast]
  [(assoc ast :db/id -1)])

;; Applying the transformation and wrap network

(defn wrap-network [network-name forms]
  (map (partial api/network network-name) forms))

(defn rewrite-expression [network-name form]
  (->> form
       transform-expression
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
  ([{store :store server :server} tx-records]
   (store-and-eval! store server tx-records))
  ([store server tx-records]
   (->> tx-records
        (store! store)
        (reduce servers/eval-ast server))))

(defn load-by-id
  ""
  [{store :store} id]
  (stores/load-by-id store id))

(defn eval-expression! [{:keys [store server] :as rt} form]
  (let [new-server (store-and-eval! store server (rewrite-expression "user" form))]
    (assoc rt :server new-server)))

(defn get-definition-by-name [runtime sym]
  (let [id (-> runtime :store (stores/resolve-name sym))]
    (-> runtime :server servers/get-defined (get id))))
