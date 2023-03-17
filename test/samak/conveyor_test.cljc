(ns samak.conveyor-test
  (:require [samak.conveyor :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true])))

(deftest should-add-tasks-to-queue
  (sut/wrap))
