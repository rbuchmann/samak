(ns samak.api-test
  (:require [samak.api            :as sut]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test   :as t :refer [is deftest] :include-macros true])
            [clojure.spec.alpha   :as s]
            [samak.api            :as api]))

(def valid-pipe #:samak.nodes{:type         :samak.nodes/pipe,
                              :from
                              #:samak.nodes {:type :samak.nodes/fn-ref,
                                             :fn   [:samak.nodes/name 'a]},
                              :to
                              #:samak.nodes {:type :samak.nodes/fn-ref,
                                             :fn   [:samak.nodes/name 'b]}})


(deftest should-detect-symbol-ast
  (is (sut/symbol-node? #:samak.nodes{:type  :samak.nodes/symbol
                                      :value 'foo})))

(deftest should-generate-pipe-ast
  (is (= valid-pipe (sut/pipe (sut/symbol 'a) (sut/symbol 'b)))))

;; (deftest should-generate-compose-ast
;;   (is (= valid-pipe (sut/compose [(sut/symbol 'a) (sut/symbol 'b)]))))

#_(deftest should-generate-valid-map
  (is (s/valid? :samak.spec/map (sut/map {(api/keyword :foo) (api/integer 1)})))
  )
