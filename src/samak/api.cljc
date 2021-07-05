(ns samak.api
  (:refer-clojure :exclude [vector map symbol keyword float])
  (:require [samak.tools :as tools]))

(defn literal [literal-name body]
  #:samak.nodes{:type  (tools/qualify-kw "samak.nodes" literal-name)
                :value body})

(defn builtin [identifier]
  (assoc (literal 'builtin identifier)
         :name identifier))

(def keyword (partial literal 'keyword))
(def key-fn  (partial literal 'key-fn))
(def integer (partial literal 'integer))
(def float   (partial literal 'float))
(def string  (partial literal 'string))

(defn symbol [identifier]
  #:samak.nodes{:type :samak.nodes/fn-ref
                :fn   [:samak.nodes/name identifier]})

(defn id-ref [identifier]
  #:samak.nodes{:type :samak.nodes/fn-ref
                :fn   {:db/id identifier}})

(defn pipe
  ([from to]
   #:samak.nodes {:type :samak.nodes/pipe
                  :from from
                  :to   to})
  ([from xf to]
   #:samak.nodes {:type :samak.nodes/pipe
                  :from from
                  :to   to
                  :xf   xf}))

(defn map-entry
  ""
  [[k v]]
  {:samak.nodes/mapkey k
   :samak.nodes/mapvalue v})


(defn map [kvs]
  #:samak.nodes {:type        :samak.nodes/map
                 :mapkv-pairs (vec (clojure.core/map map-entry kvs))})

(defn vector [items]
  #:samak.nodes {:type     :samak.nodes/vector
                 :children (tools/ordered items)})

(defn fn-call [fn-expression args]
  #:samak.nodes {:type          :samak.nodes/fn-call
                 :fn-expression fn-expression
                 :arguments     (if args (tools/ordered args) [])})

(defn defexp [expression-name rhs]
  #:samak.nodes{:type :samak.nodes/def
                :name expression-name
                :rhs  rhs})

(defn defmodule [module-name definition]
  #:samak.nodes{:type :samak.nodes/module
                :name module-name
                :definition definition})

(defn symbol-node? [s]
  (and (= :samak.nodes/symbol (:samak.nodes/type s))
       (contains? s :samak.nodes/value)))

(defn is-vector?
  [node]
  (= (:samak.nodes/type node) :samak.nodes/vector))

(defn is-map?
  [node]
  (= (:samak.nodes/type node) :samak.nodes/map))

(defn is-entry?
  [node]
  (contains? node :samak.nodes/mapvalue))

(defn is-fn-call?
  [node]
  (= (:samak.nodes/type node) :samak.nodes/fn-call))

(defn is-def?
  [node]
  (= (:samak.nodes/type node) :samak.nodes/def))

(defn is-pipe?
  [node]
  (= (:samak.nodes/type node) :samak.nodes/pipe))

(defn is-module?
  [node]
  (= (:samak.nodes/type node) :samak.nodes/module))

(defn get-pipe-source-name
  [ast]
  (get-in ast [:samak.nodes/from :samak.nodes/fn :samak.nodes/name]))

(defn get-pipe-sink-name
  [ast]
  (get-in ast [:samak.nodes/to :samak.nodes/fn :samak.nodes/name]))

(defn get-pipe-xf-fn
  [ast]
  (get-in ast [:samak.nodes/xf :samak.nodes/fn]))
