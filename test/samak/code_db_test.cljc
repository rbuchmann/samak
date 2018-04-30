(ns samak.code-db-test
  (:require [samak.testing-tools   :as tt]
            [samak.api             :as api]
            [samak.code-db         :as db]
            [datascript.core       :as d]
            #?(:clj [clojure.test  :as t]
               :cljs [cljs.test :as t :include-macros true])))

(def tdb (db/create-empty-db))

(t/deftest db-load-test
  (let [ast (:value tt/parsed-example)
        basic-tree-db (db/parse-tree->db tdb ast)]
    (t/is (= #{(api/symbol 'bar) (api/symbol 'baz) (api/symbol 'foo)}
             (set (map #(:samak.nodes/value (first %))
                       (d/q '[:find ?symbol
                              :where [?e :samak.nodes/type :samak.nodes/def]
                              [?e :samak.nodes/name ?symbol]]
                            @basic-tree-db)))))))
