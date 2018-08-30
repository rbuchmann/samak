(ns samak.nodes
  (:require [samak.core      :as core]
            [samak.protocols :as p]))

(defmulti eval-node ::type)

(defn eval-reordered [nodes]
  (->> nodes
       (sort-by :order)
       (mapv (comp eval-node ::node))))

(def eval-vals (partial map (fn [[k v]] [(::value k) (eval-node v)])))

(defmethod eval-node nil [value]
  (if (and (vector? value) (= 2 (count value)) (= ::name (first value)))
    (println (str "unknown function: '" (second value) "'"))
    (println (str "unknown token during evaluation: " (str value)))))

(defmethod eval-node ::map [{:keys [::mapkv-pairs]}]
  (reduce (fn [a {:keys [::mapkey ::mapvalue]}]
            (assoc a (::value mapkey) (eval-node mapvalue)))
          {}
          mapkv-pairs))

(defmethod eval-node ::vector [{:keys [::children]}]
  (-> children eval-reordered vec))

(defmethod eval-node ::integer [{:keys [::value]}] value)
(defmethod eval-node ::keyword [{:keys [::value]}] value)
(defmethod eval-node ::key-fn  [{:keys [::value]}] (fn [x] (value x)))
(defmethod eval-node ::string  [{:keys [::value]}] value)
(defmethod eval-node ::float   [{:keys [::value]}] value)
(defmethod eval-node ::builtin [{:keys [::value]}] (core/samak-symbols value))

(defmethod eval-node ::def [{:keys [::rhs]}] (eval-node rhs))

(defmethod eval-node ::pipe [ast] ast)

(defmethod eval-node ::fn-call [{:keys [::fn ::arguments]}]
  (apply (p/eval-as-fn (eval-node fn)) (eval-reordered arguments)))
