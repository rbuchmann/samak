(ns samak.helpers-test
  (:require [samak.helpers :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true])))

(deftest should-subs
  (is (= "foo" (sut/substring "foo" 100)))
  (is (= "...xx" (apply str (take 5 (reverse (sut/substring (apply str (take 10 (repeat "x"))) 5)))))))
