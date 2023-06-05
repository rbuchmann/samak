(ns samak.terminal-test
  (:require [samak.terminal :as sut]
            [clojure.test :as t :refer [deftest is]]))

(deftest should-find-next
  (is (= 2 (sut/find-next [1 2 3 4] 1)))
  (is (= 3 (sut/find-next [1 2 3 4] 2)))
  (is (= 4 (sut/find-next [1 2 3 4] 3)))
  (is (= 1 (sut/find-next [1 2 3 4] 4)))
  (is (= 1 (sut/find-next [1 2 3 4] :none))))
