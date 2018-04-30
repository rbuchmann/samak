(ns samak.nodes
  #?@
  (:clj
   [(:require [samak.api :as api])]
   :cljs
   [
    (:require-macros [samak.nodes :refer [defnode]]
                     [samak.api :as api])]))

(defn symbol->ns-keyword [sym]
  (->> sym
       str
       (keyword (str *ns*))))

(defmulti eval-node ::type)

(defmulti describe-node ::type)

#?(:clj
   (defmacro defnode [name fields & {:keys [references components eval-fn] :as args}]
     (let [kw (symbol->ns-keyword name)]
       `(do
          (defmethod describe-node ~kw [_#] {:references ~references})
          ~(when eval-fn
             `(defmethod eval-node ~kw [node#] (~eval-fn node#)))))))

(defn eval-reordered [nodes]
  (->> nodes
       (sort-by :order)
       (mapv eval-node)))

(def eval-vals (partial map (fn [[k v]] [(::value k) (eval-node v)])))

(defn to-map-fn [m]
  (fn [x]
    (->> m
         (map (fn [[k f]] [k (f x)]))
         (into {}))))

(defn to-vector-fn [v]
  (apply juxt v))

(def ^:dynamic *symbol-map* {})

(defn resolve-symbol [s]
  (or (*symbol-map* s)
      (*symbol-map* (api/symbol s))
      (let [msg (str "Unknown variable: " s)]
        (println "symbols" *symbol-map*)
        (println "Variable: " (pr-str s))
        #?(:clj  (throw (Exception. msg))
           :cljs (throw (js/Error.  msg)) ))))

(defnode map [::value]
  :eval-fn (comp to-map-fn eval-vals ::kv-pairs))

(defnode vector [::value]
  :eval-fn (comp to-vector-fn eval-reordered ::children))

(defnode integer [::value]
  :eval-fn (comp constantly ::value))

(defnode keyword [::value]
  :eval-fn (comp constantly ::value))

(defnode string [::value]
  :eval-fn (comp constantly ::value))

(defnode float [::value]
  :eval-fn (comp constantly ::value))

(defnode symbol [::value]
  :eval-fn (comp resolve-symbol ::value))

(defnode accessor [::value]
  :eval-fn ::value)

(defnode def [::name ::rhs])

(defnode fn-call [::name ::argument]
  :eval-fn (fn [{:keys [::fn ::argument]}]
             ((eval-node fn) (eval-node argument))))
