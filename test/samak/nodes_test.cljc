(ns samak.nodes-test
  (:require [samak.nodes     :as n]
            [clojure.test    :refer [deftest is]]
            [samak.lisparser :as lp]
            [samak.core      :as core]))


(def eval-string (comp n/eval-node #(do (println "FOOO:" %) %) :value lp/parse))

(deftest eval-literals
  (let [i (eval-string "1")]
    (is (= 1 i)))
  (let [k (eval-string ":foo")]
    (is (= :foo k)))
  (let [k (eval-string ":foo")]
    (is (= 5 (k {:foo 5})))))

(deftest eval-datastructures
  (let [m (eval-string "{:a 1}")]
    (is (= {:a 1} m))))

(deftest eval-program-structures
  (binding [n/*symbol-map* core/samak-symbols]
    (is (= 1 (eval-string "(1 :bar)")))
    (let [m (eval-string "(:foo ({:foo 1} !))")]
      (is (= m 1)))
    (is (= [1 :bar] (eval-string "([1 :foo] {:foo :bar})")))))
