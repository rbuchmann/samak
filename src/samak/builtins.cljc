(ns samak.builtins
  (:require [samak.stdlib             :as std]
            [samak.transduction-tools :as tt]
            [samak.protocols          :as p]
            [clojure.string           :as s]
            [samak.tools              :as tools]
            [samak.pipes              :as pipes]
            [samak.helpers            :as helpers]
            [clojure.string           :as str]
            [samak.core-utils         :refer [samakify samakify-all]]))

(defn or* [& args]
  (fn [x]
    (some #(% x) (map p/eval-as-fn args))))

(defn and* [& args]
  (fn [x]
    (every? #(% x) (map p/eval-as-fn args))))

(defn chain [& args]
  (->> args reverse (map p/eval-as-fn) (apply comp)))

(defn wrap-samak-reducer [f]
  (fn [state nxt]
    (f {:next  nxt
        :state state})))

(defn reduce* [f i]
  (fn [x]
    (reduce (-> f p/eval-as-fn wrap-samak-reducer) i x)))

(def if* (samakify if [pred then else]))

(defn incase
  [pred then]
  (if* pred then identity))

(defn unless
  [pred else]
  (if* pred identity else))

(defn spy
  ([] (spy nil))
  ([prefix]
   (fn [x]
     (tools/log "spy " prefix ": " x)
     x)))

(defn index-of* [s x]
  (when x
    (s/index-of x s)))

(defn zip [& args]
  (apply map vec args))

(defn myzip
  [c1 c2]
  (into [] (map vec (partition 2 (interleave c1 c2)))))

(defn lookup [m x d]
  (get m x d))

(defn takev [n col]
  (vec (take n col)))

(def samakified-builtins
  (samakify-all

   ;; Control flow
   (if   [pred then else])
   (when [pred then])

   ;; Predicates
   (nil?    [x])
   (map?    [x])
   (vector? [x])
   (number? [x])
   (list?   [x])
   (set?    [x])
   (true?   [x])
   (false?  [x])
   (string? [x])
   (zero?   [num])
   (every?  [!pred col])
   (some    [!pred col])

   ;; Sequence functions
   (first      [col])
   (second     [col])
   (next       [col])
   (rest       [col])
   (conj       [col x])
   (remove     [!f col])
   (filter     [!f col])
   (map        [!f col])
   (mapv       [!f col])
   (mapcat     [!f col])
   (concat     [& args])
   (flatten    [col])
   (nth        [col index])
   (drop       [n col])
   [take       (takev [n col])]
   (interleave [& args])
   (interpose  [sep col])
   (into       [target source])
   (partition  [n col])
   [part-step  (partition [n step col])]
   (repeat     [n x])
   (cycle      [col])
   (distinct   [col])
   (zip        [& args])
   (myzip      [col1 col2])
   (count      [col])
   (sort       [coll])
   (sort-by    [!comp coll])
   (reverse    [col])

   ;; Map functions
   (vals      [m])
   (keys      [m])
   (assoc     [m k v])
   (dissoc    [m k])
   (update-in [m ks !f & args])
   (assoc-in  [m ks v])
   (zipmap    [ks vs])
   (get       [k m d])

   ;; String functions
   (str [& args])
   [str-join  (str/join  [sep col])]
   [str-split (str/split [s re])]
   [str-index (index-of* [s x])]
   [str-to-int (helpers/str-to-int [s])]

   ;; Boolean functions
   (=  [& args])
   (<  [& args])
   (<= [& args])
   (>= [& args])
   (>  [& args])
   ;; and, or handled separately

   ;; Arithmetic functions
   (+     [& args])
   (-     [& args])
   (*     [& args])
   (/     [& args])
   (inc   [x])
   (dec   [x])
   (max   [& args])
   (min   [& args])
   (odd?  [x])
   (even? [x])

   ;; random functions
   (rand-int [x])
   ))


;; (defn loop*
;;   [test body init]
;;   (let [test* (p/eval-as-fn test)
;;         body* (p/eval-as-fn body)
;;         init* (p/eval-as-fn init)
;;         acc (atom nil)]
;;     (fn [x]
;;       (reset! acc (init* x))
;;       (while (test* @acc)
;;         (reset! acc (body* @acc)))
;;       @acc)))

;; (defn dropv
;;   [n]
;;   (fn [c]
;;     (into [] (drop n c))))

;; (defn eq*
;;   ""
;;   []
;;   (fn [v]
;;     (apply = v)))


;; (defn partitionv
;;   [n]
;;   (fn [c]
;;     (into [] (map vec (partition n c)))))

;; (defn repeat*
;;   ""
;;   []
;;   (fn [[n x]]
;;     (into [] (repeat n x))))

;; (defn index-of*
;;   ""
;;   [s]
;;   (fn [x]
;;     (when x (s/index-of x s))))

;; (defn negate
;;   ""
;;   []
;;   (fn [x]
;;     (- x)))


;; (defn distinct*
;;   ""
;;   []
;;   (fn [x]
;;     (distinct x)))


;; (defn join*
;;   ""
;;   []
;;   (fn [s]
;;     (s/join s)))


;; (defn max*
;;   ""
;;   []
;;   (fn [x]
;;     (apply max x)))

;; (defn min*
;;   ""
;;   []
;;   (fn [x]
;;     (apply min x)))


;; TODO: This happens before evaling-as-samak-fn, so it breaks a ton
;; of stuff, commented it out for now
#_(defn pipify
  ""
  [& args]
  (when (some nil? args)
    (throw (str "trying to connect with nil function")))
  (->> args
       (map (fn [f]
              (fn [x]
                (if (or (nil? f) (nil? x))
                  (println (str "Trying to call nil function or value:" f " on " x))
                  (f x)))))
       (apply chain)
       pipes/instrument
       ))


(def samak-symbols
  (merge
   samakified-builtins
   {'->            chain
    '_             identity
    'reduce        reduce*
    'only          #(if* % identity tt/ignore)
    'except        #(if* % tt/ignore identity)
    'many          tt/many
    'ignore        tt/ignore
    'or            or*
    'and           and*
    'incase        incase
    'unless        unless
    'lookup        (samakify lookup [m x d])
    'spy           spy
    'create-sink   :noop
    'connect       :noop
    'add-cell      :noop
    'edit-cell     :noop
    'indent-cell   :noop
    'swap-cell     :noop
    'cut-cell      :noop
    'pipes/caravan :noop
    '!             '!}))
