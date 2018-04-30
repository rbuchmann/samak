(ns samak.code-db-test
  (:require [samak.testing-tools   :as tt]
            [samak.code-db         :as db]
            [datascript.core       :as d]
            #?(:clj [clojure.test  :as t]
                  :cljs [cljs.test :as t :include-macros true])))

(def tdb (db/create-empty-db))

(t/deftest db-load-test
  (let [basic-tree-db (db/parse-tree->db tdb (:value tt/parsed-example))]
    (t/is (= (d/q '[:find ?x
                    :where [?c :samak.nodes/type :samak.nodes/def]
                           [?c :samak.nodes/name ?x]]
                  @basic-tree-db)
             '#{[bar] [baz] [foo]}))))
