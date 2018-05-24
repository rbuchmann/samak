(ns samak.api
  (:refer-clojure :exclude [vector map symbol keyword float])
  (:require [samak.tools :as tools]))

(defn literal [literal-name body]
  #:samak.nodes{:type  (tools/qualify-kw "samak.nodes" literal-name)
                :value body})

(defn symbol [identifier]
  (literal 'symbol identifier))

(defn keyword [identifier]
  (literal 'keyword identifier))

(defn integer [identifier]
  (literal 'integer identifier))

(defn float [identifier]
  (literal 'float identifier))

(defn string [identifier]
  (literal 'string identifier))

(defn pipe [args]
  #:samak.nodes {:type      :samak.nodes/pipe
                 :arguments (tools/ordered args)})

(defn map [kvs]
  #:samak.nodes {:type     :samak.nodes/map
                 :kv-pairs (vec kvs)})
(defn vector [items]
  #:samak.nodes {:type     :samak.nodes/vector
                 :children (tools/ordered items)})

(defn fn-call [fn-expression args]
  #:samak.nodes {:type      :samak.nodes/fn-call
                 :fn        fn-expression
                 :arguments (tools/ordered args)})

(defn defexp [expression-name rhs]
  #:samak.nodes{:type :samak.nodes/def
                :name expression-name
                :rhs  rhs})

(defn symbol? [s]
  (and (= :samak.nodes/symbol (:samak.nodes/type s))
       (contains? s :samak.nodes/value)))
