(ns samak.main-test
  (:require [clojure.test      :refer [deftest is testing]]
            [samak.main        :as sut]
            [samak.combiparser :as p]))

(def toplevel-devs "foo = :-bar\nbar = :-baz")

(deftest toplevel-eval
  (is (= (-> toplevel-devs
             p/parse
             :value
             sut/eval-toplevel-defs)
         {'foo :bar
          'bar :baz})))
