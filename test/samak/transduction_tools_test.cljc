(ns samak.transduction-tools-test
  (:require [samak.transduction-tools :as sut]
            #?(:clj [clojure.test     :as t]
               :cljs [cljs.test       :as t :include-macros true])))

(t/deftest streamables-test
  (t/is (= [3] (into [] (sut/instrumentation-xf #(if (even? %) (sut/ignore %) %)) [2 3 4 ])))
  (t/is (= [1 1 1 2 2 2]
           (into [] (sut/instrumentation-xf (comp sut/many (partial repeat 3))) [1 2]))))

(t/deftest metadata-test
  (t/is (= [{:samak.pipes/meta    "I'm so meta, even this acronym"
             :samak.pipes/content 2}]
           (into [] (sut/instrumentation-xf inc)
                 [{:samak.pipes/meta    "I'm so meta, even this acronym"
                   :samak.pipes/content 1}]))))
