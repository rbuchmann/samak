(ns samak.repl-test
  (:require [samak.repl :as sut]
            [clojure.test :as t :refer [deftest is]]
            [clojure.set :as set]))

(def def-node #:samak.nodes{:type :samak.nodes/def
                            :name {:samak.nodes/value 'quux}
                            :rhs  #:samak.nodes{:type  :samak.nodes/string
                                                :value "quux"}})

(def pipe-env {'bar (constantly "bar")
               'baz (constantly "baz")})

(def pipe-node #:samak.nodes{:op :samak.nodes/pipe,
                             :type :samak.nodes/binop,
                             :arguments
                             [{:samak.nodes/value 'bar,
                               :samak.nodes/type :samak.nodes/symbol,
                               :order 0}
                              {:samak.nodes/value 'baz,
                               :samak.nodes/type :samak.nodes/symbol,
                               :order 1}]})

;; eval-pipe-op

(deftest should-resolve-pipes
  (binding [samak.nodes/*symbol-map* pipe-env]
    (is (every? samak.pipes/pipe? (first (sut/eval-pipe-op pipe-node))))))


;; eval-pipes

(deftest should-resolve-pipes-from-ast
  (is (every? samak.pipes/pipe? (first (sut/eval-pipes pipe-env [pipe-node])))))

;; eval-toplevel-defs

(deftest should-evaluate-defs
  (let [exp [#:samak.nodes{:type :samak.nodes/def
                           :name {:samak.nodes/value 'foo}
                           :rhs  #:samak.nodes{:type  :samak.nodes/string
                                               :value "bar"}}]
        result (sut/eval-toplevel-defs {} exp)]
    (is (contains? result {:samak.nodes/value 'foo}))
    (let [rhs (get result {:samak.nodes/value 'foo})]
      (is (ifn? rhs))
      (is (= "bar" (rhs))))))


;; eval-exp

(deftest should-evaluate-expression
  (let [exp [#:samak.nodes{:type :samak.nodes/def
                           :name {:samak.nodes/value 'foo}
                           :rhs  #:samak.nodes{:type  :samak.nodes/string
                                               :value "bar"}}]]
    (let [result (sut/eval-exp {} exp)]
      (is (contains? result {:samak.nodes/value 'foo})))))

(deftest should-keep-existing-symbols
  (let [exp [#:samak.nodes{:type :samak.nodes/def
                           :name {:samak.nodes/value 'foo}
                           :rhs  #:samak.nodes{:type  :samak.nodes/string
                                               :value "bar"}}]
        symbols {{:samak.nodes/value 'quux} (constantly "quux")}]
  (let [result (sut/eval-exp symbols exp)]
    (is (contains? result {:samak.nodes/value 'foo}))
    (is (contains? result {:samak.nodes/value 'quux})))))

(deftest should-refer-to-env
  (let [exp [#:samak.nodes{:type :samak.nodes/def
                           :name {:samak.nodes/value 'foo}
                           :rhs  #:samak.nodes{:type  :samak.nodes/symbol
                                               :value {:samak.nodes/value 'bar}}}]
        symbols {{:samak.nodes/value 'bar} (constantly "bar")}
        result (sut/eval-exp symbols exp)]
    (is (contains? result {:samak.nodes/value 'foo}))
    (is (contains? result {:samak.nodes/value 'bar}))))

(deftest should-relink-pipes
  (with-redefs-fn {#'samak.pipes/link-all! (fn [pipes] (is (every? samak.pipes/pipe? (apply concat pipes))))}
    #(let [result (sut/eval-exp pipe-env [pipe-node])]
       (is (every? result #{'bar 'baz})))))
