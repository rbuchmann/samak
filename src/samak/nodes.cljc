(ns samak.nodes
  #?@
   (:clj
    [(:require [clojure.spec.alpha :as s])]
    :cljs
    [(:require [cljs.spec.alpha :as s])
     (:require-macros [samak.nodes :refer [defnode]])]))

(defn symbol->ns-keyword [sym]
  (->> sym
       str
       (keyword (str *ns*))))

(defmulti eval-node ::type)

(defmulti describe-node ::type)

(s/def ::type keyword?)

(s/def ::node (s/keys :req [::type]))

(s/def ::value (complement nil?))

(def s-merge #?(:clj  'clojure.spec.alpha/merge
                :cljs 'cljs.spec.alpha/merge))
(def s-keys #?(:clj  'clojure.spec.alpha/keys
               :cljs 'cljs.spec.alpha/keys))
(def s-def #?(:clj  'clojure.spec.alpha/def
              :cljs 'cljs.spec.alpha/def))

#?(:clj
   (defmacro defnode [name fields & {:keys [references components eval-fn] :as args}]
     (let [kw (symbol->ns-keyword name)]
       `(let [spec# (~samak.nodes/s-merge ::node (~samak.nodes/s-keys :req ~fields))]
          (do
            (~samak.nodes/s-def ~kw spec#)
            (defmethod describe-node ~kw [_#] {:references ~references
                                               :spec       spec#})
            ~(when eval-fn
               `(defmethod eval-node ~kw [node#] (~eval-fn node#))))))))

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
      (let [msg (str "Unknown variable: " s)]
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
