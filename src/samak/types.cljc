(ns samak.types
  (:require [samak.nodes :as n]
            [samak.tools :as tools]))

(defn make [name & kv-pairs]
  (into {::name name}
        kv-pairs))

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

(defn type-error [& args]
  (apply tools/fail "Type Error:" args))

(defmulti builtin->type-fn (fn [node args] (::n/name node)))

(defn type-with [node x]
  ((node->type-fn node) x))

(defmethod builtin->type-fn 'inc [_ [lens]]
  (fn [input-type]
    (let [transformed-input-type (check-with lens input-type)]
      (if (subtype? transformed-input-type tnumber)
        transformed-input-type
        (type-error transformed-input-type "is not a number!")))))

(defn produced-type [pipe])
(defn consumed-type [pipe])

(defn typecheck-pipe [pipe-node]
  (let [{:keys [::n/from ::n/to ::n/xf]} pipe-node
        produced (produced-type from)
        consumed (consumed-type to)
        type-fn  (if (some? xf)
                   (node->type-fn xf)
                   identity)
        transformed (type-fn produced)]
    (if (subtype? transformed consumed)
      true
      (tools/fail transformed " is not a subtype of " consumed "!"))))
