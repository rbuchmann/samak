(ns samak.api
  (:refer-clojure :exclude [vector map symbol accessor keyword float])
  (:require [samak.tools :as tools]))

(defn literal
  [literal-name body]
  #:samak.nodes{:type (tools/qualify-kw "samak.nodes" literal-name)
                :value body})

(defn symbol
  [identifier]
  (literal 'symbol identifier))

(defn accessor
  [identifier]
  (literal 'accessor identifier))

(defn keyword
  [identifier]
  (literal 'keyword identifier))

(defn integer
  [identifier]
  (literal 'integer identifier))

(defn float
  [identifier]
  (literal 'float identifier))

(defn string
  [identifier]
  (literal 'string identifier))

(defn binop
  [op-name args]
  #:samak.nodes {:type      :samak.nodes/binop
                 :op        (tools/qualify-kw "samak.nodes" op-name)
                 :arguments (tools/ordered args)})

(defn pipe
  [source sink]
  (binop 'pipe [source sink]))

(defn compose
  [args]
  (binop 'compose args)) ;; here be dragons?!?

(defn map
  [kvs]
  #:samak.nodes {:type :samak.nodes/map
                 :kv-pairs (vec kvs)})
(defn vector
  [items]
  #:samak.nodes {:type     :samak.nodes/vector
                 :children (tools/ordered items)})

(defn fn-call [fn-expression expression]
  #:samak.nodes {:type     :samak.nodes/fn-call
                 :fn       fn-expression
                 :argument expression})

(defn defexp
  [expression-name rhs]
  #:samak.nodes{:type :samak.nodes/def
                :name expression-name
                :rhs  rhs})
