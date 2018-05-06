(ns samak.api-test
  (:require [samak.api :as sut]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test :as t :refer [is deftest] :include-macros true])))

(deftest should-detect-symbol-ast
  (is (sut/symbol? #:samak.nodes{:type :samak.nodes/symbol
                                 :value 'foo})))
