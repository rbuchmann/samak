(ns samak.nodes-test
  (:require [clojure.test    :refer [deftest is]]
            [samak.api       :as api]
            [samak.core      :as core]
            [samak.lisparser :as lp]
            [samak.nodes     :as n]))

(def eval-string (comp n/eval-node first :value lp/parse-all))

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
  (is (= 1 (eval-string "(1 :bar)")))
  (let [m (eval-string "(:-foo {:foo 1})")]
    (is (= m 1)))
  (is (= [1 :foo :bar] (eval-string "([1 :foo :-foo] {:foo :bar})"))))

(deftest should-evaluate-map
  (is (= {:test 1} (n/eval-node (api/map {(api/keyword :test) (api/integer 1)})))))

(deftest should-eval-builtins
  (is (= 2 ((n/eval-node (api/defexp 'inc (api/builtin 'inc))) 1)))
  )
