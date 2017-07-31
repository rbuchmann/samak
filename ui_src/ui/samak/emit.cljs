(ns ui.samak.emit
  (:require [clojure.string :as str]
            [cljs.pprint    :as pp]))

(defmulti emit :kind)

(defmethod emit :program [node]
  (mapv emit (:definitions node)))

(defmethod emit :def [node]
  (list 'def (-> node :name symbol) (-> node :rhs emit)))

(defmethod emit :fn-call [node]
  (list* (-> node :name symbol)
         (->> node
              :arguments
              (sort-by :order)
              (map emit))))

(defmethod emit :vector [node]
  (->> node :children (sort-by :order) (map emit) vec))

(defmethod emit :integer [node]
  (:value node))

(defmethod emit :keyword [node]
  (-> node :value keyword))

(defmethod emit :string [node]
  (:value node))

(defmethod emit :var [node]
  (-> node :value symbol))

(defmethod emit :default [node]
  (str "No emit value for: " (:kind node)))

(defmethod emit :map [node]
  (into {} (for [item (:value node)]
             (mapv emit item))))

(defmethod emit :pipe-def [{:keys [from to transducers]}]
  (list* 'std/link (map emit [from (assoc transducers :kind :vector) to])))

(defmethod emit :lambda [{:keys [params rhs]}]
  (list 'fn (->> params (sort-by :order) (mapv emit)) (emit rhs)))

(defmethod emit :handler [{:keys [channel field-id]}]
  (let [{:keys [id field]} field-id]
    `(fn [& args#] (~'std/fire! ~(symbol channel) (aget (js/document.getElementById ~id) ~field)))))

(defmethod emit :chan-declare [{:keys [chans]}]
  `(do
     ~@(for [chan chans]
         `(def ~(symbol chan) (~'std/pipe (~'chan))))))

(defn clj->str [form]
  (with-out-str
    (pp/with-pprint-dispatch pp/code-dispatch
      (pp/pprint form))))

(def header '(ns ui.samak.app (:require [ui.samak.stdlib :as std] [ui.samak.pipes :as pipes] [ui.components :as ui] [cljs.core.async :as a :refer [put! chan <! >! timeout close!]])))

(defn prepend-header [forms]
  (cons header forms))

(defn append-footer [forms]
  (-> forms vec (conj '(std/start))))

(defn emit-clj [program]
  (if (:reason program)
    "Parse error"
    (->> program
         emit
         prepend-header
         append-footer
         (map clj->str)
         (interpose \newline)
         (apply str))))
