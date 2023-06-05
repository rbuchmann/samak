(ns samak.emit
  (:require [clojure.string :as str]))

(defmulti emit :samak.nodes/type)

(defmethod emit :samak.nodes/def [node]
  (let [rhs (:samak.nodes/rhs node)]
    (if (= (:samak.nodes/type rhs) :samak.nodes/builtin)
      (:samak.nodes/name node) ;; just print builtin directly
      (str "(def '" (:samak.nodes/name node) " " (emit rhs) ")"))))

(defmethod emit :samak.nodes/fn-call [node]
  (let [args (:samak.nodes/arguments node)]
    (str "(" (->> node :samak.nodes/fn-expression emit)
         (when args (str " " (str/join " " (map #(emit (:samak.nodes/node %)) (sort-by :order args)))))
         ")")))

(defn in-brackets [s]
  (str "[" s "]"))

(defn in-curlys [s]
  (str "{" s "}"))

(defmethod emit :samak.nodes/vector [node]
  (->> node :samak.nodes/children (sort-by :order) (map #(emit (:samak.nodes/node %))) vec (str/join " ") in-brackets))

(defmethod emit :samak.nodes/integer [node]
  (str (:samak.nodes/value node)))

(defmethod emit :samak.nodes/keyword [node]
  (-> node :samak.nodes/value keyword str))

(defmethod emit :samak.nodes/key-fn [node]
  (str ":-" (name (:samak.nodes/value node))))

(defmethod emit :samak.nodes/string [node]
  (str "\"" (:samak.nodes/value node) "\""))

(defmethod emit :samak.nodes/var [node]
  (-> node :samak.nodes/value pr-str))

(defmethod emit :default [node]
  (str "No emit value for " (:samak.nodes/type node) ", node was " node ", keyword was" :samak.nodes/type))

(defmethod emit :samak.nodes/map [node]
  (->> node
       :samak.nodes/mapkv-pairs
       (map #(str (emit (:samak.nodes/mapkey %)) " " (emit (:samak.nodes/mapvalue %))))
       (str/join ", ")
       in-curlys))

(defmethod emit :samak.nodes/fn-ref [node]
  (-> node :samak.nodes/fn emit))

(defmethod emit :samak.nodes/builtin [node]
  (str (:samak.nodes/value node)))
