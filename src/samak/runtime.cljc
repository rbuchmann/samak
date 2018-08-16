(ns samak.runtime
  (:require [datascript.core :as d]
            [samak.code-db   :as db]
            [samak.nodes     :as n]
            [samak.pipes     :as pipes]
            [samak.api       :as api]
            [clojure.set     :as set]))

(defn eval-all-new! [db tx-records]
  (let [new-ids (-> (db/parse-tree->db! db tx-records)
                    :tempids
                    (dissoc :db/current-tx)
                    vals)]
    (into {}
          (for [id new-ids]
            [id (n/eval-node (db/load-by-id db id))]))))

;; If it's a map, take out the tempid,
;; otherwise assume that it's a lookup ref
(defn to-db-id [record]
  (if (map? record)
    (:db/id record)
    record))

(defn eval-toplevel-ast! [db ast]
  (condp (fn [k form] (= (::n/type form) k)) ast
    ::n/def  (eval-all-new! db [(assoc ast :db/id -1)])
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
                                 ::n/from (to-db-id a)
                                 ::n/to   (to-db-id b)})
                   tx-records (filter map? (concat arguments links))]
               (eval-all-new! db tx-records))
    {:latest (n/eval-node ast)}))

(defn link-all-pipes! [linked-pipes db defined-ids]
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
                          (map defined-ids)
                          (apply pipes/link!))]))))

(declare load-builtins!)

(defn make-runtime
  ([]
   {:db           (db/create-empty-db)
    :defined-ids  (atom {})
    :linked-pipes (atom {})})
  ([builtins]
   (load-builtins! (make-runtime) builtins)))

(defn eval-expression! [{:keys [db defined-ids linked-pipes] :as state} ast]
  (let [new-defines (eval-toplevel-ast! db ast)]
    (swap! defined-ids (fn [ids] (-> ids
                                    (dissoc :latest)
                                    (merge new-defines))))
    (swap! linked-pipes link-all-pipes! db @defined-ids)
    state))

(defn load-builtins! [rt builtin-symbols]
  (->> builtin-symbols
       (map (fn [s] (api/defexp s (api/builtin s))))
       (reduce eval-expression! rt)))

(def get-defined-ids  (comp deref :defined-ids))
(def get-linked-pipes (comp deref :linked-pipes))

(defn get-definition-by-name [state sym]
  (if-let [id (:db/id (db/load-ast (:db state) sym))]
    (-> state get-defined-ids (get id))
    (println "Couldn't find \"" sym "\" in database")))
