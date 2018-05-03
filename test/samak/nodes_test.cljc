(ns samak.nodes-test
  (:require [samak.nodes       :as n]
            [clojure.test      :refer [deftest is]]
            [samak.combiparser :as cp]
            [blancas.kern.core :as p]
            [samak.core        :as core]))


(defn eval-string [parser s]
  (->> s
       (p/value parser)
       n/eval-node))

(def eval-literal (partial eval-string cp/p-literal))

(def eval-exp (partial eval-string cp/p-simple-expression))

(deftest eval-literals
  (let [i (eval-literal "1")]
    (is (= 1 (i :foo))))
  (let [k (eval-literal ":foo")]
    (is (= :foo (k 5))))
  (let [k (eval-literal ":-foo")]
    (is (= 5 (k {:foo 5})))))

(deftest eval-datastructures
  (let [m (eval-literal "{:a 1}")]
    (is (= {:a 1} (m :foo)))))

(deftest eval-program-structures
  (binding [n/*symbol-map* core/samak-symbols]
    (is (= {:foo 1} (eval-exp "({:foo 1} !)")))
    (let [m (eval-exp "(:-foo ({:foo 1} !))")]
      (is (= m 1)))
    (is (= [1 :foo] (eval-exp "([1 :foo] !)")))))
