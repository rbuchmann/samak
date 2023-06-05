(ns samak.emit-test
  (:require [samak.emit :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true])))


(def simple-data {:db/id 594, :samak.nodes/name "li", :samak.nodes/rhs {:db/id 595, :samak.nodes/fn-expression {:db/id 596, :samak.nodes/fn {:db/id 15, :samak.nodes/name "pipes/station", :samak.nodes/rhs {:db/id 16, :name "pipes/station", :samak.nodes/type :samak.nodes/builtin, :samak.nodes/value "pipes/station"}, :samak.nodes/type :samak.nodes/def}, :samak.nodes/type :samak.nodes/fn-ref}, :samak.nodes/type :samak.nodes/fn-call}, :samak.nodes/type :samak.nodes/def})

(deftest should-emit-reduction
  (is (= "" (sut/emit simple-data))))
