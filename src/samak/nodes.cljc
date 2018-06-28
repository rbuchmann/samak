(ns samak.nodes
  (:require [samak.api       :as api]
            [samak.code-db   :as db]
            [samak.protocols :as p]))

(def ^:dynamic *symbol-map* {})
(def ^:dynamic *db*)

(defmulti eval-node ::type)

(defn eval-reordered [nodes]
  (->> nodes
       (sort-by :order)
       (mapv eval-node)))

(def eval-vals (partial map (fn [[k v]] [(::value k) (eval-node v)])))

(defn resolve-from-db
  ""
  [s]
  (let [node (eval-node (db/load-ast *db* s))]
    ;; here be stateful dragons
    node))


(defn resolve-symbol [s]
  (or (*symbol-map* s)
      (let [msg (str "Unknown variable: " s)]
        (println "symbols" *symbol-map*)
        (println "Variable: " (pr-str s))
        #?(:clj  (throw (Exception. msg))
           :cljs (throw (js/Error.  msg))))))

(defmethod eval-node nil [value] (println (str "eval node broke for " value)))

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
(defmethod eval-node ::symbol  [{:keys [::value]}] (resolve-symbol value))

(defmethod eval-node ::def [{:keys [::name ::rhs]}])

(defmethod eval-node ::pipe [ast] ast)

(defmethod eval-node ::fn-call [{:keys [::fn ::arguments]}]
  (apply (p/eval-as-fn (eval-node fn)) (eval-reordered arguments)))
