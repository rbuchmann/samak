(ns samak.emit
  (:require [clojure.string :as str]
            [samak.nodes    :as n]))

(defmulti emit :n/type)

(defmethod emit :n/def [node]
  (str (-> node :n/name) " = " (-> node :n/rhs emit)))

(defmethod emit :n/fn-call [node]
  (str (-> node :n/name symbol) " " (->> node
                                         :n/argument
                                         emit)))

(defn in-brackets [s]
  (str "[" s "]"))

(defn in-curlys [s]
  (str "{" s "}"))

(defmethod emit :n/vector [node]
  (->> node :n/children (sort-by :order) (map emit) vec (str/join " ") in-brackets))

(defmethod emit :n/integer [node]
  (str (:n/value node)))

(defmethod emit :n/keyword [node]
  (-> node :n/value keyword str))

(defmethod emit :n/string [node]
  (:n/value node))

(defmethod emit :n/var [node]
  (-> node :n/value pr-str))

(defmethod emit :default [node]
  (str "No emit value for " (:n/type node) ", node was " node ", keyword was" :n/type))

(defmethod emit :n/map [node]
  (->> node
       :n/kv-pairs
       (map #(map emit %))
       (map #(str/join " " %))
       (str/join ", ")
       in-curlys))

(defmethod emit :n/bin-op [{:keys [n/op n/arguments]}]
  (str/join op (map emit arguments)))
