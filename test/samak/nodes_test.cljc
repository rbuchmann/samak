(ns samak.nodes-test
  (:require [samak.nodes     :as n]
            [clojure.test    :refer [deftest is]]
            [samak.lisparser :as lp]
            [samak.core      :as core]
            [samak.api       :as api]
            [samak.code-db :as db]))


(def eval-string (comp n/eval-node :value lp/parse))

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
    (let [m (eval-string "(:-foo ({:foo 1} !))")]
      (is (= m 1)))
    (is (= [1 :foo :bar] (eval-string "([1 :foo :-foo] {:foo :bar})")))))

(deftest should-resolve-plain-symbol
  (binding [n/*symbol-map* core/samak-symbols]
    (is (= inc (n/resolve-symbol 'inc)))))

(deftest should-resolve-from-db
  (with-redefs-fn {#'db/load-ast (fn [db s] (is (= s 'foo)) (api/keyword :foo))}
    #(binding [n/*symbol-map* core/samak-symbols]
       (is (= :foo (n/resolve-symbol 'foo))))))

(deftest should-resolve-multi-exp
  (with-redefs-fn {#'db/load-ast (fn [db s] (is (= s 'foo)) (api/integer 1))}
    #(binding [n/*symbol-map* core/samak-symbols]
       (is (= 2 (n/eval-node (api/fn-call (api/symbol 'sum)
                                             [(api/vector [(api/integer 1)
                                                           (api/symbol 'foo)])])))))))



(deftest should-evaluate-map
  (is (= {:test 1} (n/eval-node (api/map {(api/keyword :test) (api/integer 1)})))))
