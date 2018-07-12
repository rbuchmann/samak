(ns samak.stdlib-test
  (:require [samak.stdlib               :as sut]
            [samak.pipes                :as pipes]
            #?(:clj [clojure.core.async :as a :refer [go chan take! put!]]
               :cljs [cljs.core.async   :as a :refer [go chan take! put!]])
            #?(:clj [clojure.test       :as t :refer [is deftest]]
               :cljs [cljs.test         :as t :include-macros true :refer [is deftest]])
            [samak.code-db              :as db]))

(deftest should-provide-db
  (is (not (nil? (sut/db-init :a)))))

(deftest should-log
  (let [in (sut/debug)
        logger (sut/log :unused)]
    (pipes/link! in logger)
    (pipes/fire! in :foo))
  )

(deftest should-log
  (let [debug (sut/debug)
        listener (chan 1)]
      (a/tap (.out-port debug) listener)
      (pipes/fire! debug :foo)
      (take! listener (fn [x] (is (= :foo x))))))

(deftest should-call
  (let [out (chan 1)]
    ((sut/query-call (db/create-empty-db) :b) :foo out)
    (take! out #(is (= :not-found %)))))


(deftest should-db
  (let [db (sut/db-init :unused)
        result (sut/db-query db 'foo)
        listener (chan 1)]
    (a/tap (.out-port result) listener)
    (take! listener (fn [x] (is (= [] x))) true)
    (pipes/fire! result :foo)))


(deftest should-query-db
  (let [db (sut/db-init :unused)
        result (sut/db-query db 'foo)
        listener (chan 1)]
    ;; (is (pipes/pipe? result))
    (a/tap (.out-port result) listener)
    (pipes/fire! result :foo)
    (take! listener (fn [x] (is (= [] x))) true)))
