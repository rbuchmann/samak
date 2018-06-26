(ns samak.runtime
  (:require [datascript.core :as d]
            [samak.code-db   :as db]
            [samak.nodes     :as n]
            [samak.pipes     :as pipes]
            [clojure.set     :as set]))

(defprotocol SamakRuntime
  (eval-expression! [this ast])
  (get-defined-expressions [this])
  (query [this query]))

(defn maybe-wrap-instrumentation [evaluated]
  (if (pipes/pipe? evaluated)
    evaluated
    (pipes/instrument evaluated)))

(defn eval-all-new! [db tx-records]
  (println "About to transact these records:" tx-records)
  (let [new-ids (-> (db/parse-tree->db! db tx-records)
                    :tempids
                    (dissoc :db/current-tx)
                    vals)]
    (into {}
          (for [id new-ids]
            [id (n/eval-node (db/load-by-id db id))]))))

(defn eval-toplevel-ast! [db ast]
  (condp (fn [k form] (= (::n/type form) k)) ast
    ::n/def  (eval-all-new! db [(update ast ::n/rhs assoc :db/id -1)])
    ::n/pipe (let [arguments  (->> ast
                                   ::n/arguments
                                   (sort-by :order)
                                   (map ::n/node)
                                   (map-indexed (fn [i arg]
                                                  (if (map? arg)
                                                    (assoc arg :db/id (- (inc i)))
                                                    arg))))
                   links      (for [[a b] (partition 2 1 arguments)]
                                {::n/type ::n/link
                                 ::n/from a
                                 ::n/to   b})
                   tx-records (filter map? (concat arguments links))]
               (eval-all-new! db tx-records))
    (n/eval-node ast)))

(defn link-all-pipes! [db defined-ids linked-pipes]
  (let [pipe-pairs     (->> (db/retrieve-links db)
                            (map (juxt ::n/from ::n/to))
                            (map #(mapv :db/id %))
                            set)
        already-linked (-> linked-pipes keys set)
        to-link        (set/difference pipe-pairs already-linked)
        to-unlink      (set/difference already-linked pipe-pairs)]
    (doseq [edge to-unlink]
      (pipes/disconnect (linked-pipes edge)))
    (into {} (for [edge to-link]
               [edge (->> edge
                          (map (comp maybe-wrap-instrumentation defined-ids))
                          (apply pipes/link!))]))))

(defrecord BasicRuntime [db defined-ids linked-pipes]
  SamakRuntime
  (eval-expression! [_ ast]
    (let [new-defines (eval-toplevel-ast! db ast)]
      (swap! defined-ids  merge new-defines)
      (swap! linked-pipes merge (link-all-pipes! db @defined-ids @linked-pipes))))
  (get-defined-expressions [_]
    @defined-ids)
  (query [_ q]
    (d/q q @db)))

(defn make-runtime []
  (BasicRuntime. (db/create-empty-db) (atom {}) (atom {})))

(def ep
  "(def in (pipes/debug))
  (def out (pipes/log))
  (def f inc)
  (| in f out)")

(def p (-> (samak.lisparser/parse-all ep) :value ))
