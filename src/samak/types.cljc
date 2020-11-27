(ns samak.types
  (:require [samak.nodes     :as n]
            [samak.tools     :as tools]
            [samak.lisparser :as p]
            [samak.api       :refer [builtin]]))

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

(defmethod node->type-fn ::n/fn-call [{:keys [::n/fn-expression ::n/arguments] :as node}]
  (let [called (follow-refs fn-expression)]
    (builtin->type-fn called (map ::n/node arguments))))

(defn type-error [& args]
  (apply tools/fail "Type Error:" args))

(defmethod node->type-fn ::n/builtin [node]
  (if (= (::n/name node) '_)
    identity
    (type-error "Tried taking the type-fn of an uncalled builtin")))

(def type-annotation ::n/type-annotation)

(defmulti builtin->type-fn (fn [node args] (::n/name node)))

(defmethod builtin->type-fn 'inc [node [lens]]
  (fn [input-type]
    (let [transformed ((node->type-fn lens) input-type)]
      (if (subtype? transformed tnumber)
        input-type
        (type-error transformed "is not a subtype of number!")))))

(defmethod builtin->type-fn 'test [node _]
  (print node))

(def tp '((def in (pipes/debug))
          (def out (pipes/log))
          (| in (inc _) out)))

(def pipe-node #:samak.nodes{:type :samak.nodes/pipe,
                             :from
                             #:samak.nodes{:type :samak.nodes/fn-ref,
                                           :fn [:samak.nodes/name 'in]
                                           :type-annotation tnumber},
                             :to
                             #:samak.nodes{:type :samak.nodes/fn-ref,
                                           :fn [:samak.nodes/name 'out]
                                           :type-annotation tnumber},
                             :xf
                             #:samak.nodes{:type :samak.nodes/fn-call,
                                           :fn-expression (builtin 'inc) ,
                                           :arguments
                                           [{:order 0,
                                             :samak.nodes/node (builtin '_)}]}})

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
