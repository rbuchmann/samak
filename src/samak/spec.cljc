(ns samak.spec
  (:require #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])))


(defmulti nested-exp :samak.nodes/type)

(s/def :samak.nodes/mapkv (s/keys :req [:samak.nodes/mapkey :samak.nodes/mapvalue]))
(s/def :samak.nodes/mapkv-pairs (s/coll-of :samak.nodes/mapkv))
(s/def :samak.spec/map (s/keys :req [:samak.nodes/type
                                     :samak.nodes/mapkv-pair]))
(defmethod nested-exp :samak.nodes/map [_] :samak.spec/map)

;; TODO
(defmethod nested-exp :samak.nodes/fn-call [_] any?)
(defmethod nested-exp :samak.nodes/vector [_] any?)
(defmethod nested-exp :samak.nodes/integer [_] any?)
(defmethod nested-exp :samak.nodes/symbol [_] any?)
(defmethod nested-exp :samak.nodes/keyword [_] any?)
(defmethod nested-exp :samak.nodes/float [_] any?)
(defmethod nested-exp :samak.nodes/string [_] any?)
(defmethod nested-exp :samak.nodes/key-fn [_] any?)


(s/def :samak.nodes/rhs nested-exp)

(s/def :samak.spec/eval-node (s/keys :req [:samak.nodes/type
                                           :samak.nodes/name
                                           :samak.nodes/rhs]))

(s/def :samak.spec/pipe (s/keys :req [:samak.nodes/type
                                      :samak.nodes/arguments]))

(defmulti toplevel-exp :samak.nodes/type)
(defmethod toplevel-exp :samak.nodes/def [_] :samak.spec/eval-node)
(defmethod toplevel-exp :samak.nodes/pipe [_] :samak.spec/pipe)


(s/def :samak.spec/toplevel-exp (s/multi-spec toplevel-exp :samak.nodes/type))
