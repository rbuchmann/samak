(ns samak.oasis-test
  (:require [samak.oasis :as sut]
            [samak.repl :as repl]
            [samak.nodes :as nodes]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test   :as t :refer [is deftest] :include-macros true])))

(deftest should-define
  (let [code (repl/eval-multi-exp repl/base-symbols [sut/get-val])
        func (get code 'get-val)]
    (is (< (count repl/base-symbols) (count code)))
    (is (= true (ifn? func)))
    (is (= :bar (apply func [{:samak.nodes/rhs {:samak.nodes/value :bar}}])))))
