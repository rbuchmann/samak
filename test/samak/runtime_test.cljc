(ns samak.runtime-test
  (:require [samak.runtime         :as sut]
            #?(:clj [clojure.test  :as t :refer [deftest is]]
               :cljs [cljs.test    :as t :include-macros true])
            [samak.code-db         :as db]
            [samak.stdlib          :as std]
            [samak.runtime.stores  :as stores]
            [samak.runtime.servers :as servers]
            [samak.utils           :as utils]
            [promesa.core          :as p]
            [datascript.core       :as d]))


(def def-node
  #:samak.nodes{:type :samak.nodes/def
                :name 'quux
                :rhs  #:samak.nodes {:type  :samak.nodes/string
                                     :value "quux"}})

(deftest should-eval-def-node
  (p/let [init (sut/make-runtime)
          rt (sut/eval-expression! init def-node)
          k (stores/resolve-name (:store rt) 'quux)]
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
  (p/let [r              (sut/make-runtime {'pipes/debug std/debug
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
  (p/let [r  (-> (sut/make-runtime)
                 (p/then #(sut/eval-expression! % def-node))
                 (p/then #(sut/eval-expression! % other-def-node)))
          vs (-> r :server servers/get-defined vals)]
    (is (= (set vs) #{"quux" "foo"}))))

(def referring-node
  #:samak.nodes{:type :samak.nodes/def
                :name 'bar
                :rhs  [:samak.nodes/name 'quux]})

(deftest should-resolve-symbols
(p/let [rt (-> (sut/make-runtime)
               (sut/eval-expression! def-node)
               (sut/eval-expression! referring-node))
        vs (-> rt
               :server
               servers/get-defined
               vals)]
    (is (= 2 (count vs)))
    (is (apply = vs))))

(deftest should-retrieve-definitions-by-name
  (p/let [init (sut/make-runtime)
        rt (sut/eval-expression! init other-def-node)
        n (stores/resolve-name (:store rt) 'foo)]
    (is (= 1 n))))


(deftest should-persist-builtins
  (utils/test-promise (p/then (sut/make-runtime {'inc inc 'dec dec})
                              #(do (println "?")
                                   (is (=
                                    inc
                                    (-> %
                                        :server
                                        servers/get-defined
                                        (get 1))))))))
