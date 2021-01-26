(ns samak.nodes
  (:require [samak.protocols :as p]
            [samak.api       :as api]
            [samak.pipes     :as pipes]
            [samak.tools     :refer [fail log]]
            [samak.trace     :refer [*db-id*]]
            [samak.code-db   :as db]))

(def ^:dynamic *manager* nil)
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
                               (or ((:resolve *manager*) id)
                                   (compile-error "Referenced id " id " was undefined")))
    :default                 (compile-error "unknown token during evaluation: " (str "type: " (or (type value) "nil") " with value: " (str value)))))

(defmethod eval-node ::module [module]
  ((:module *manager*) module *manager*))

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

(defmethod eval-node ::def [{:keys [::rhs] :as fn}]
  (let [res (eval-node rhs)
        id (:db/id fn)]
    ((:register *manager*) id res)
    res))

(defmethod eval-node ::pipe [{:keys [::from ::to ::xf] :as p}]
  (let [a (eval-node from)
        c (eval-node to)
        b (when xf
            (let [db-id (:db/id xf)]
              (binding [*db-id* db-id]
                (-> xf
                    eval-node
                    ((partial pipes/instrument db-id (:cancel? *manager*)))
                    (#(pipes/transduction-pipe % (str (pipes/uuid a) "-" db-id "-" (pipes/uuid c))))))))]
    ((:link *manager*) a c b)))

(defmethod eval-node ::fn-ref [{:keys [::fn] :as f}]
  (or ((:resolve *manager*) (:db/id fn))
      (when (api/is-def? fn)
        (let [res (eval-node fn)]
          ;; (println "evaling" (:db/id fn) "->" res "def" fn)
          res))
      ;; (when (api/is-module? fn)
      ;;   (let [res (eval-node fn)]
      ;;     (println "evaling" (:db/id fn) "->" res "mod" fn) res))
      (println "type:" (::type fn) (:db/id fn))
      (compile-error "Undefined reference for evaling " *db-id* " fn " fn)))

(defmethod eval-node ::fn-call [{:keys [::fn-expression ::arguments]}]
  (let [func (eval-node fn-expression)]
    (try (apply (p/eval-as-fn func) (eval-reordered arguments))
         (catch #?(:clj clojure.lang.ArityException :cljs js/Error) ex
           (compile-error "wrong args: " (eval-reordered arguments) " for fn " func " -> " ex)))))

(defmethod eval-node ::link [{:keys [::from ::to]}]
  (pipes/link! (eval-node from) (eval-node to)))

(defn eval-env [manager builtins ast db-id]
  (binding [*manager* manager
            *builtins* builtins
            *db-id* db-id]
    (eval-node ast)))
