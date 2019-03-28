(ns samak.builtins
  (:require [samak.stdlib             :as std]
            [samak.transduction-tools :as tt]
            [samak.protocols          :as p]
            [clojure.string           :as s]
            [samak.tools              :as tools]
            [samak.pipes              :as pipes]
            [clojure.string           :as str]
            [samak.core-utils         :refer [samakify-all]]))

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
   (some?   [!pred col])

   ;; Sequence functions
   (first      [col])
   (second     [col])
   (next       [col])
   (rest       [col])
   (remove     [!f col])
   (filter     [!f col])
   (mapcat     [!f col])
   (concat     [& args])
   (flatten    [col])
   (nth        [col !index])
   (drop       [!n col])
   (take       [!n col])
   (interleave [& args])
   (interpose  [!sep col])
   (into       [!target source])
   (partition  [!n col])
   [part-step  (partition [!n !step col])]
   (repeat     [!n x])
   (cycle      [col])
   (distinct   [col])
   (zip        [& args])
   (count      [col])
   (sort       [coll])
   (sort-by    [!comp coll])

   ;; Map functions
   (vals      [m])
   (keys      [m])
   (assoc     [m !k v])
   (dissoc    [m !k])
   (update-in [m !ks !f & args])
   (assoc-in  [m !ks !k v])
   (zipmap    [!ks vs])

   ;; String functions
   (str [& args])
   [str-join  (str/join  [!sep col])]
   [str-split (str/split [s !re])]
   [str-index (index-of* [s !x])]

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
   ))

(def samak-symbols
  (merge
   samakified-builtins
   {'->            chain
    '|>            (comp pipes/instrument chain)
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
