(ns samak.types
  (:require [samak.nodes :as n]
            [samak.tools :as tools]))

(defn make [name & kv-pairs]
  (into {::name name}
        kv-pairs))

(defn follow-refs [node]
  (if (= ::n/fn-ref (::n/type node))
    (follow-refs (::n/fn node))
    node))

(defn union [& subtypes]
  (make ::union
        ::subtypes (tools/ordered subtypes)))

(defn intersection [& subtypes]
  (make ::intersection
        ::subtypes (tools/ordered subtypes)))

(defn kvmap
  "A map with a dynamic number of kv pairs of the same respective types"
  [key-type value-type]
  (make ::kvmap
        ::key-type key-type
        ::value-type value-type))

(defn hmap
  "A struct like map with a fixed set of keys with different types of
  values. Value types are defined by the namespaced key,
  just like in core.spec"
  [required-keys optional-keys]
  (make ::map
        ::required-keys (tools/ordered required-keys)
        ::optional-keys (tools/ordered optional-keys)))

(defn tvector
  "Only boring homogeneous vectors for now, although we need mixed ones
  for at least hiccups"
  [element-type]
  (make ::vector
        ::element-type element-type))

(def tstring  (make ::string))
(def tnumber  (make ::number))
(def tbool    (make ::bool))
(def tkeyword (make ::keyword))
(def tany     (make ::any))

(defmulti subtype? (fn [a b]
                     (print "a: " a ", b: " b)
                     [(::name a) (::name b)]))

(defmethod subtype? :default [a b]
  (= a b))

(defmethod subtype? [::integer ::number] [_ _]
  true)

(defmethod subtype? [::float ::number] [_ _]
  true)

(defmulti node->type-fn ::n/type)

(defmethod node->type-fn ::n/fn-ref [node]
  (node->type-fn (::n/fn node)))

(defmethod node->type-fn ::n/fn-call [{:keys [n/fn-expression]} node]
  (let [called (follow-refs (::n/fn-expression node))]))

(defn type-error [& args]
  (apply tools/fail "Type Error:" args))

(def type-annotation (comp ::n/type-annotation follow-refs))

(defmulti builtin->type-fn (fn [node args] (::n/name node)))

(defmethod builtin->type-fn 'inc [node [lens]]
  (fn [input-type]
    (let [transformed (lens input-type)]
      (if (subtype? transformed tnumber)
        input-type
        (type-error transformed "is not a subtype of number!")))))

(defn typecheck-pipe [pipe-node]
  (let [{:keys [::n/from
                ::n/to
                ::n/xf]} pipe-node
        input-type       (type-annotation from)
        output-type      (type-annotation to)
        type-fn          (if (some? xf)
                           (node->type-fn xf)
                           identity)
        transformed (type-fn input-type)]
    (if (subtype? transformed output-type)
      true
      (tools/fail transformed " is not a subtype of " output-type "!"))))
