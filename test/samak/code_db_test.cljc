(ns samak.code-db-test
  (:require [samak.testing-tools  :as tt]
            [samak.api            :as api]
            [samak.code-db        :as db]
            [datascript.core      :as d]
            [datascript.core      :as d]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test   :as t :include-macros true])))

(def tdb (db/create-empty-db))

(deftest should-persist-maps
  (let [ast (api/map {(api/keyword :test) (api/integer 1)
                      (api/keyword :foo) (api/integer 42)})]
    (is (some? (db/parse-tree->db! tdb [ast])))))


(def builtins ['inc 'dec 'pipes/debug '|>])

(deftest db-save-test
  (let [ast           (:value tt/parsed-example)
        builtin (db/parse-tree->db! tdb (map #(api/defexp % (api/builtin %)) builtins))
        basic-tree-db (do (db/parse-tree->db! tdb ast) tdb)
        result (d/q '[:find [(pull ?e [*])]
                      :in $ ?name
                      :where [?e :samak.nodes/type :samak.nodes/def]
                      [?e :samak.nodes/name ?name]]
                    @basic-tree-db
                    'foo)]
    (is (= 'foo (:samak.nodes/name (first result))))))

(deftest should-load-ast
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))
        parsed (:value tt/parsed-example)
        base (do (db/parse-tree->db! db parsed) db)
        result (db/load-ast base 'foo)]
    (is (= 'foo (get-in result [:samak.nodes/name])))
    (is (= 1 (get-in result [:samak.nodes/rhs :db/id])))))

(deftest should-load-by-id
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))
        parsed (:value tt/parsed-example)
        base (do (db/parse-tree->db! db parsed) db)
        result (db/load-by-id base 9)]
    (is (= 'foo (get-in result [:samak.nodes/name])))
    (is (= 1 (get-in result [:samak.nodes/rhs :db/id])))))

(deftest should-walk-maps-on-load-by-id
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))
        parsed [(assoc (api/map {(api/keyword :key) (api/symbol 'inc)})
                       :db/id -1)]
        map-id (get-in (db/parse-tree->db! db parsed) [:tempids -1])
        result (db/load-by-id db map-id)
        kv-pair (first (:samak.nodes/mapkv-pairs result))]
    (is (= :samak.nodes/map (:samak.nodes/type result)))
    (is (map? kv-pair))
    (contains? kv-pair :samak.nodes/mapkey)
    (contains? kv-pair :samak.nodes/mapvalue)))

(deftest should-refer-fn-by-id
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))
        src [(assoc (api/defexp 'foo (api/symbol 'inc)) :db/id -1)]
        foo-id (get-in (db/parse-tree->db! db src) [:tempids -1])
        used [(assoc (api/defexp 'bar (api/fn-call {:db/id foo-id} [])) :db/id -1)]
        result-id (get-in (db/parse-tree->db! db used) [:tempids -1])
        result (db/load-by-id db result-id)]
    (is (= 'bar (get-in result [:samak.nodes/name])))
    (is (= foo-id (get-in result [:samak.nodes/rhs :samak.nodes/fn :db/id])))))
