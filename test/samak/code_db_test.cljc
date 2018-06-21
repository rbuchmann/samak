(ns samak.code-db-test
  (:require [samak.testing-tools  :as tt]
            [samak.api            :as api]
            [samak.code-db        :as db]
            [datascript.core :as d]
            [datascript.core      :as d]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test   :as t :include-macros true])))

(def tdb (db/create-empty-db))

(deftest should-persist-maps
  (let [ast (api/map {(api/keyword :test) (api/integer 1)
                      (api/keyword :foo) (api/integer 42)})]
    (is (some? (db/parse-tree->db tdb [ast])))))


(deftest db-load-test
  (let [ast           (:value tt/parsed-example)
        basic-tree-db (db/parse-tree->db tdb ast)
        result (d/q '[:find [(pull ?e [*])]
                      :in $ ?name
                      :where [?e :samak.nodes/type :samak.nodes/def]
                      [?e :samak.nodes/name ?name]]
                    @basic-tree-db
                    'foo)]
    (is (= 'foo (:samak.nodes/name (first result))))))


(deftest should-load-ast
  (let [db (db/create-empty-db)
        parsed (:value tt/parsed-example)
        base (db/parse-tree->db db parsed)
        result (db/load-ast base 'foo)]
    (println result)
    (is (= 'foo (get-in result [:samak.nodes/name ])))
    (is (= 'inc (get-in result [:samak.nodes/rhs :samak.nodes/value])))))
