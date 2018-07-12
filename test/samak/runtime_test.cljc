(ns samak.runtime-test
  (:require [samak.runtime        :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is]]

               :cljs [cljs.test   :as t :include-macros true])
            [datascript.core      :as d]))

(def r (sut/make-runtime))


(def def-node
  #:samak.nodes{:type :samak.nodes/def
                :name 'quux
                :rhs  #:samak.nodes {:type  :samak.nodes/string
                                     :value "quux"}})

(deftest should-eval-def-node
  (let [r (sut/make-runtime)
        [k v] (-> r
                  (sut/eval-expression! def-node)
                  sut/get-defined-ids
                  first)]
    (is (number? k))
    (is (= "quux" v))))


(def pipe-node
  #:samak.nodes{:type :samak.nodes/pipe,
                :arguments
                [{:order        0,
                  :samak.nodes/node
                  #:samak.nodes {:type         :samak.nodes/fn-call,
                                 :fn
                                 #:samak.nodes {:type
                                                :samak.nodes/builtin,
                                                :value 'pipes/debug},
                                 :arguments    []}}
                 {:order        1,
                  :samak.nodes/node
                  #:samak.nodes {:type         :samak.nodes/fn-call,
                                 :fn
                                 #:samak.nodes {:type
                                                :samak.nodes/builtin,
                                                :value 'pipes/log},
                                 :arguments    []}}]})

(deftest should-eval-pipe-node
  (let [r         (sut/make-runtime)
        new-state (sut/eval-expression! r pipe-node)
        pipes     (-> new-state sut/get-defined-ids vals)
        pipe-ids  (-> new-state sut/get-defined-ids keys)
        link      (-> new-state sut/get-linked-pipes keys first)]
    (is (every? samak.pipes/pipe? pipes))
    (is (= (set pipe-ids) (set link)))))

(def other-def-node
  #:samak.nodes{:type :samak.nodes/def
                :name 'foo
                :rhs  #:samak.nodes {:type  :samak.nodes/string
                                     :value "foo"}})

(deftest should-keep-existing-symbols
  (let [r  (-> (sut/make-runtime)
               (sut/eval-expression! def-node)
               (sut/eval-expression! other-def-node))
        vs (-> r sut/get-defined-ids vals)
        ]
    (is (= (set vs) #{"quux" "foo"}))))

(def referring-node
  #:samak.nodes{:type :samak.nodes/def
                :name 'bar
                :rhs  [:samak.nodes/name 'quux]})

(deftest should-resolve-symbols
  (let [vs (-> (sut/make-runtime)
                (sut/eval-expression! def-node)
                (sut/eval-expression! referring-node)
                sut/get-defined-ids
                vals)]
    (is (= 2 (count vs)))
    (is (apply = vs))))

(deftest should-retrieve-definitions-by-name
  (let [r (-> (sut/make-runtime)
              (sut/eval-expression! other-def-node))]
    (is (= "foo" (sut/get-definition-by-name r 'foo)))))
