(ns samak.code-db-test
  (:require [samak.testing-tools  :as tt]
            [samak.api            :as api]
            [samak.code-db        :as db]
            [datascript.core :as d]
            [datascript.core      :as d]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test   :as t :include-macros true])))

(def tdb (db/create-empty-db))

(deftest db-load-test
  (let [ast           (:value tt/parsed-example)
        basic-tree-db (db/parse-tree->db tdb ast)
        result (d/q '[:find [(pull ?symbol [*]) ...]
                      :in $ ?name
                      :where [?e :samak.nodes/type :samak.nodes/def]
                             [?e :samak.nodes/name ?symbol]
                             [?symbol :samak.nodes/value ?name]]
                    @basic-tree-db
                    'foo)]
    (is (= ['foo] (mapv :samak.nodes/value result)))))


(deftest should-load-ast
  (let [db (db/create-empty-db)
        parsed (:value tt/parsed-example)
        base (db/parse-tree->db db parsed)
        result (db/load-ast base 'foo)]
    (is (= 'foo (get-in result [:samak.nodes/name :samak.nodes/value])))
    (is (= 'inc (get-in result [:samak.nodes/rhs :samak.nodes/value])))))
