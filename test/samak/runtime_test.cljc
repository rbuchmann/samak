(ns samak.runtime-test
  (:require [samak.runtime         :as sut]
            #?(:clj [clojure.test  :as t :refer [deftest is]]
               :cljs [cljs.test    :as t :include-macros true])
            [samak.code-db         :as db]
            [samak.stdlib          :as std]
            [samak.runtime.stores  :as stores]
            [samak.runtime.servers :as servers]
            [datascript.core       :as d]))


(def def-node
  #:samak.nodes{:type :samak.nodes/def
                :name 'quux
                :rhs  #:samak.nodes {:type  :samak.nodes/string
                                     :value "quux"}})

(deftest should-eval-def-node
  (let [r (sut/make-runtime)
        k (-> r
              (sut/eval-expression! def-node)
              :store
              (stores/resolve-name 'quux))]
    (is (number? k))))


(def pipe-node
  #:samak.nodes {:type :samak.nodes/pipe
                 :from #:samak.nodes {:type         :samak.nodes/fn-call,
                                      :fn-expression
                                      #:samak.nodes {:type
                                                     :samak.nodes/builtin,
                                                     :value 'pipes/debug},
                                      :arguments    []}
                 :to   #:samak.nodes {:type         :samak.nodes/fn-call,
                                      :fn-expression
                                      #:samak.nodes {:type
                                                     :samak.nodes/builtin,
                                                     :value 'pipes/log},
                                      :arguments    []}})

(deftest should-eval-pipe-node
  (let [r              (sut/make-runtime {'pipes/debug std/debug
                                          'pipes/log   std/log})
        new-state      (sut/eval-expression! r pipe-node)
        defined-things (-> new-state :server servers/get-defined vals)]
    (is (some samak.pipes/pipe? defined-things))))

(def other-def-node
  #:samak.nodes{:type :samak.nodes/def
                :name 'foo
                :rhs  #:samak.nodes {:type  :samak.nodes/string
                                     :value "foo"}})

(deftest should-keep-existing-symbols
  (let [r  (-> (sut/make-runtime)
               (sut/eval-expression! def-node)
               (sut/eval-expression! other-def-node))
        vs (-> r :server servers/get-defined vals)]
    (is (= (set vs) #{"quux" "foo"}))))

(def referring-node
  #:samak.nodes{:type :samak.nodes/def
                :name 'bar
                :rhs  [:samak.nodes/name 'quux]})

(deftest should-resolve-symbols
  (let [vs (-> (sut/make-runtime)
                (sut/eval-expression! def-node)
                (sut/eval-expression! referring-node)
                :server
                servers/get-defined
                vals)]
    (is (= 2 (count vs)))
    (is (apply = vs))))

(deftest should-retrieve-definitions-by-name
  (let [r (-> (sut/make-runtime)
              (sut/eval-expression! other-def-node))]
    (is (= 1 (stores/resolve-name (:store r) 'foo)))))


(deftest should-persist-builtins
  (is (= inc (-> (sut/make-runtime {'inc inc 'dec dec})
                 :server
                 servers/get-defined
                 (get 1)))))
