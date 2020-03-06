(ns samak.lisparser-test
  (:require [samak.lisparser :as sut]
            [samak.api :as api]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true :refer [deftest is]])))

(deftest should-parse-def
  (is (= [(api/defexp 'a (api/integer 1))] (:value (sut/parse-all "(def a 1)")))))

(deftest should-parse-defmodule
  (is (= [(api/defmodule 'a (api/map {}))] (:value (sut/parse-all "(defmodule a {})")))))
