(ns samak.builtins-test
  (:require [samak.builtins :as sut]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test :as t :include-macros true])))

(deftest should-zip-vectors
  (is (= [[:a 1] [:b 2]] (sut/myzip [:a :b] [1 2])))
  )
