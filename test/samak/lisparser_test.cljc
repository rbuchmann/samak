(ns samak.lisparser-test
  (:require [samak.lisparser :as sut]
            [samak.api :as api]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true :refer [deftest is]])))

(deftest should-parse-def
  (is (=  {:value (api/defexp (api/symbol 'a) (api/integer 1))} (sut/parse "(def a 1)")))
  )
