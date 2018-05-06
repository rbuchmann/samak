(ns samak.nodes
  #?@
  (:clj
   [(:require [samak.api :as api])]
   :cljs
   [(:require [samak.api :as api])
    (:require-macros [samak.nodes :refer [defnode]])]))

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

(declare eval-as-fn)

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn to-map-fn [m]
  (let [fn-m (map-vals eval-as-fn m)]
    (fn [x]
      (->> fn-m
           (map (fn [[k f]] [k (f x)]))
           (into {})))))

(defn to-vector-fn [v]
  (apply juxt (map eval-as-fn v)))

(def ^:dynamic *symbol-map* {})

(defn matches-symbol
  [s x]
  (and (api/symbol? x)
       (= (str (:samak.nodes/value x)) (str s))))

(defn resolve-binding
  [s [sym rhs]]
  (when (matches-symbol s sym)
    rhs))

(defn resolve-symbol [s]
  (or (*symbol-map* s)
      (some #(resolve-binding s %) *symbol-map*)
      (let [msg (str "Unknown variable: " s)]
        (println "symbols" *symbol-map*)
        (println "Variable: " (pr-str s))
        #?(:clj  (throw (Exception. msg))
           :cljs (throw (js/Error.  msg))))))

(defprotocol SamakCallable
  (to-samak-fn [this]))

(extend-protocol SamakCallable
  #?(:clj  clojure.lang.PersistentArrayMap
     :cljs cljs.core/PersistentArrayMap)
  (to-samak-fn [this] (to-map-fn this))
  #?(:clj  clojure.lang.PersistentVector
     :cljs cljs.core/PersistentVector)
  (to-samak-fn [this] (to-vector-fn this)))

(defn eval-as-fn [f]
  (cond
    (satisfies? SamakCallable f) (to-samak-fn f)
    (ifn? f) f
    :default (constantly f)))

(defnode map [::value]
  :eval-fn (comp #(into {} %) eval-vals ::kv-pairs))

(defnode vector [::value]
  :eval-fn (comp vec eval-reordered ::children))

(defnode integer [::value]
  :eval-fn ::value)

(defnode keyword [::value]
  :eval-fn ::value)

(defnode string [::value]
  :eval-fn ::value)

(defnode float [::value]
  :eval-fn ::value)

(defnode symbol [::value]
  :eval-fn (comp resolve-symbol ::value))

(defnode def [::name ::rhs])

(defnode fn-call [::name ::argument]
  :eval-fn (fn [{:keys [::fn ::argument]}]
             ((eval-as-fn (eval-node fn)) (eval-node argument))))
