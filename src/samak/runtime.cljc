(ns samak.runtime
  (:require [samak.runtime.stores  :as stores]
            [samak.runtime.servers :as servers]
            [samak.nodes           :as n]
            [samak.api             :as api]))

(defn make-runtime
  ([]
   {:store  (stores/make-local-store)
    :server (servers/make-local-server)})
  ([builtins]
   (update (make-runtime) :store stores/load-builtins! builtins)))

;; Toplevel code transformation

(defmulti transform-expression ::n/type)

(defmethod transform-expression ::n/pipe [ast]
  (let [arguments (->> ast ::n/arguments (sort-by :order))]
    (map-indexed
     (for [[a b] (partition 2 1 arguments)]
       {::n/type ::n/link
        ::n/from (assoc a :db/id -1)
        ::n/to   (assoc b :db/id -2)}))))

(defmethod transform-expression ::n/def [ast]
  [(assoc ast :db/id -1)])

;; Applying the transformation and apply network

(defn wrap-network [network-name forms]
  (map (partial api/network network-name) forms))

(defn rewrite-expression [network-name form]
  (->> form
       transform-expression
       (wrap-network network-name)))

;; Evaluation - Dumb and without dependency resolution for now

(defn store-and-eval! [store server tx-records]
  (let [new-ids (-> (stores/persist-tree! store tx-records)
                    :tempids
                    (dissoc :db/current-tx)
                    vals)]
    (doseq [id new-ids]
      (servers/eval-ast! store (stores/load-by-id store id)))))

(defn eval-expression! [{:keys [store server]} form]
  (reduce (partial store-and-eval! store)
   server
   (rewrite-expression "user" form)))
