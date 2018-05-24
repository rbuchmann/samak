(ns samak.nodes
  (:require [samak.api       :as api]
            [samak.protocols :as p]))

(def ^:dynamic *symbol-map* {})

(defmulti eval-node ::type)

(defn eval-reordered [nodes]
  (->> nodes
       (sort-by :order)
       (mapv eval-node)))

(def eval-vals (partial map (fn [[k v]] [(::value k) (eval-node v)])))

(defn resolve-symbol [s]
  (or (*symbol-map* s)
      (let [msg (str "Unknown variable: " s)]
        (println "symbols" *symbol-map*)
        (println "Variable: " (pr-str s))
        #?(:clj  (throw (Exception. msg))
           :cljs (throw (js/Error.  msg))))))

(defmethod eval-node ::map [{:keys [::kv-pairs]}]
  (->> kv-pairs eval-vals (into {})))

(defmethod eval-node ::vector [{:keys [::children]}]
  (-> children eval-reordered vec))

(defmethod eval-node ::integer [{:keys [::value]}] value)
(defmethod eval-node ::keyword [{:keys [::value]}] value)
(defmethod eval-node ::key-fn  [{:keys [::value]}] (fn [x] (value x)))
(defmethod eval-node ::string  [{:keys [::value]}] value)
(defmethod eval-node ::float   [{:keys [::value]}] value)
(defmethod eval-node ::symbol  [{:keys [::value]}] (resolve-symbol value))

(defmethod eval-node ::def [{:keys [::name ::rhs]}])

(defmethod eval-node ::fn-call [{:keys [::fn ::arguments]}]
  (apply (p/eval-as-fn (eval-node fn)) (eval-reordered arguments)))
