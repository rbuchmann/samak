(ns samak.nodes
  (:require [samak.protocols :as p]
            [samak.pipes     :as pipes]
            [samak.tools     :refer [fail log]]
            [samak.trace     :refer [*db-id*]]
            [samak.code-db :as db]))

(def ^:dynamic *environment* {})
(def ^:dynamic *builtins* {})


(defn compile-error
  [& args]
  (fail ["[" *db-id* "]"] args))


(defmulti eval-node ::type)

(defn eval-reordered [nodes]
  (->> nodes
       (sort-by :order)
       (mapv (comp eval-node ::node))))

(def eval-vals (partial map (fn [[k v]] [(::value k) (eval-node v)])))

(defn ref? [m]
  (and (map? m) (= (keys m) [:db/id])))

(defn unresolved-name? [value]
  (and (vector? value)
       (= 2 (count value))
       (= ::name (first value))))

(defmethod eval-node nil [value]
  (cond
    (unresolved-name? value) (compile-error "Tried to eval unresolved name:"
                                   (str  "'" (second value) "'"))
    (ref? value)             (let [id (:db/id value)]
                               (or (get *environment* id)
                                   (compile-error "Referenced id " id " was undefined")))
    :default                 (compile-error "unknown token during evaluation: " (str value))))

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
(defmethod eval-node ::builtin [{:keys [::value]}] (get *builtins* value))

(defmethod eval-node ::def [{:keys [::rhs]}] (eval-node rhs))

(defmethod eval-node ::pipe [{:keys [::from ::to ::xf]}]
  (let [a (eval-node from)
        b (when xf
            (binding [*db-id* (:db/id xf)]
              (eval-node xf)))
        c (eval-node to)]
    (if b
      (pipes/link! (pipes/link! a b) c)
      (pipes/link! a c))))

(defmethod eval-node ::fn-ref [{:keys [::fn]}]
  (or (get *environment* (:db/id fn))
      (when (= (::type fn) ::def) (eval-node fn))
      (compile-error "Undefined reference " fn " in " *environment*)))

(defmethod eval-node ::fn-call [{:keys [::fn-expression ::arguments]}]
  (let [func (eval-node fn-expression)]
    (apply (p/eval-as-fn func) (eval-reordered arguments))))

(defmethod eval-node ::link [{:keys [::from ::to]}]
  (pipes/link! (eval-node from) (eval-node to)))

(defn eval-env [env builtins ast db-id]
  (binding [*environment* env
            *builtins* builtins
            *db-id* db-id]
    (eval-node ast)))
