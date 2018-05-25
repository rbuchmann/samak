(ns samak.api-test
  (:require [samak.api            :as sut]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test   :as t :refer [is deftest] :include-macros true])))

(def valid-pipe #:samak.nodes{:type :samak.nodes/pipe,
                              :arguments
                              [{:samak.nodes/type :samak.nodes/symbol,
                                :samak.nodes/value 'a
                                :order 0}
                               {:samak.nodes/type :samak.nodes/symbol,
                                :samak.nodes/value 'b,
                                :order 1}]})


(deftest should-detect-symbol-ast
  (is (sut/symbol-node? #:samak.nodes{:type  :samak.nodes/symbol
                                      :value 'foo})))

(deftest should-generate-pipe-ast
  (is (= valid-pipe (sut/pipe [(sut/symbol 'a) (sut/symbol 'b)]))))

;; (deftest should-generate-compose-ast
;;   (is (= valid-pipe (sut/compose [(sut/symbol 'a) (sut/symbol 'b)]))))
