(ns samak.nodes
  (:require #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha    :as s])))

(defn symbol->ns-keyword [sym]
  (->> sym
       str
       (keyword (str *ns*))))

(defmulti eval-node ::type)

(defmulti describe-node ::type)

(s/def ::type keyword?)

(s/def ::node (s/keys :req [::type]))

(s/def ::value (complement nil?))

(defmacro defnode [name fields & {:keys [references components eval-fn] :as args}]
  (let [kw (symbol->ns-keyword name)]
    `(let [spec# (s/merge ::node (s/keys :req ~fields))]
       (do
         (s/def ~kw spec#)
         (defmethod describe-node ~kw [_#] {:references ~references
                                            :spec       spec#})
         ~(when eval-fn
            `(defmethod eval-node ~kw [node#] (~eval-fn node#)))))))

(defn eval-reordered [nodes]
  (->> nodes
       (sort-by :order)
       (mapv eval-node)))

(def eval-vals (partial map (fn [[k v]] [k (eval-node v)])))


;; TODO: Make fn literals first class?

(defnode map [::value]
  :eval-fn (comp eval-vals ::value))

(defnode vector [::children]
  :eval-fn (comp eval-reordered ::children))

(defnode integer [::value]
  :eval-fn ::value)

(defnode keyword [::value]
  :eval-fn ::value)

(defnode string [::value]
  :eval-fn ::value)

(defnode float [::value]
  :eval-fn ::value)

(defnode var [::value]
  :eval-fn ::value)

#_(defnode )
