(ns samak.repl-test
  (:require [samak.repl   :as sut]
            [samak.test-programs :as tests]
            [clojure.test :as t :refer [deftest is]]
            [clojure.string :as str]))

(deftest should-eval-inc-x2
  (let [stdout (with-out-str
                 (sut/eval-lines tests/tl)
                 (Thread/sleep 100))]
    (is (str/includes? stdout ":content 7"))))

(deftest should-eval-local-modules
  (let [stdout (with-out-str
                 (sut/eval-lines tests/test-local-modules)
                 (Thread/sleep 1000))]
    (is (str/includes? stdout "!!!"))))

(deftest should-eval-builtin-modules
  (let [stdout (with-out-str
                 (sut/eval-lines tests/test-builtin-modules)
                 (Thread/sleep 1000))]
    (is (str/includes? stdout "!!!"))))
