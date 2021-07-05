(ns samak.transduction-tools-test
  (:require [samak.transduction-tools :as sut]
            #?(:clj [clojure.test     :as t]
               :cljs [cljs.test       :as t :include-macros true])))

(t/deftest streamables-test
  (t/is (= [3] (into [] (sut/instrumentation-xf #(if (even? %) (sut/ignore %) %) ::streamables) [2 3 4 ])))
  (t/is (= [1 1 1 2 2 2]
           (into [] (sut/instrumentation-xf (comp sut/many (partial repeat 3)) ::streamables) [1 2]))))

(t/deftest metadata-test
  (let [motd "I'm so meta, even this acronym"
        result (into {} (sut/instrumentation-xf inc ::meta-data)
                     [{:samak.pipes/meta {:motd motd}
                       :samak.pipes/content 1}])]
    (t/is (= 2 (:samak.pipes/content result)))
    (t/is (= motd (:motd (:samak.pipes/meta result))))))
