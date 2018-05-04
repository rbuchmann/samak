(ns samak.code-db-test
  (:require [samak.testing-tools  :as tt]
            [samak.api            :as api]
            [samak.code-db        :as db]
            [datascript.core      :as d]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test   :as t :include-macros true])))

(def tdb (db/create-empty-db))

(deftest db-load-test
  (let [ast           (:value tt/parsed-example)
        basic-tree-db (db/parse-tree->db tdb ast)]
    (is (= #{'bar 'baz 'foo}
             (set (map #(:samak.nodes/value (first %))
                       (d/q '[:find ?symbol
                              :where [?e :samak.nodes/type :samak.nodes/def]
                              [?e :samak.nodes/name ?symbol]]
                            @basic-tree-db)))))))

(deftest should-load-ast
  (let [db (db/create-empty-db)
        base (db/parse-tree->db db (:value tt/parsed-example))]
    (is (= :foo
           (db/load-ast @base 'foo)))))
