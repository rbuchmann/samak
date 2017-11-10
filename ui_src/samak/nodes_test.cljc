(ns samak.nodes-test
  (:require [samak.nodes       :as n]
            [clojure.test      :refer [deftest is]]
            [samak.combiparser :as cp]
            [blancas.kern.core :as p]))


(defn eval-string [parser s]
  (->> s
       (p/value parser)
       n/eval-node))

(def eval-literal (partial eval-string cp/p-literal))

(deftest eval-literals
  (let [i (eval-literal "1")]
    (is (= 1 (i :foo))))
  (let [k (eval-literal ":foo")]
    (is (= :foo (k 5)))))


(deftest eval-datastructures
  (let [m (eval-literal "{:a 1}")]
    (is (= {:a 1} (m :foo)))))
