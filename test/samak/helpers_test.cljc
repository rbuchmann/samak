(ns samak.helpers-test
  (:require [samak.helpers :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true])
            [clojure.string :as str]))

(deftest should-subs
  (is (= "foo" (sut/substring "foo" 100)))
  (is (= "...xx" (apply str (take 5 (reverse (sut/substring (apply str (take 10 (repeat "x"))) 5)))))))

(deftest should-hex
  (is (re-matches #"\p{XDigit}" (sut/hex)))
  (is (= 1 (count (sut/hex)))))
