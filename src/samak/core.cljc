(ns samak.core
  (:require [samak.stdlib             :as std]
            [samak.transduction-tools :as tt]
            [samak.protocols          :as p]
            [net.cgrand.xforms        :as x]))

(defn if* [pred then else]
  (let [pred* (p/eval-as-fn pred)
        then* (p/eval-as-fn then)
        else* (p/eval-as-fn else)]
    (fn [x]
      (if (pred* x)
        (then* x)
        (else* x)))))

(defn or* [& args]
  (fn [x]
    (some #(% x) (map p/eval-as-fn args))))

(defn and* [& args]
  (fn [x]
    (every? #(% x) (map p/eval-as-fn args))))

(defn when* [pred]
  (let [pred* (p/eval-as-fn pred)]
    (fn [x]
      (when (pred* x)
        x))))

(defn chain [& args]
  (->> args reverse (map p/eval-as-fn) (apply comp)))

(defn curry1fn [f]
  (fn [x]
    (let [x* (p/eval-as-fn x)]
      (fn [y]
        (f x* y)))))

(defn curry1 [f]
  (fn [x]
    (fn [y]
      (f x y))))

(defn mapcatv [f]
  (fn [x]
    (vec (mapcat f x))))

(def sum (partial apply +))

(defn filter* [pred]
  (let [pred* (p/eval-as-fn pred)]
    (fn [x]
      (if (pred* x)
        x
        (tt/ignore x)))))

(def sum (partial apply +))

(defn red [f init]
  (let [col (atom (vector))]
    (fn [x]
      (swap! col conj x)
      (reduce f init @col))))

(defn vals*
  ([]
   (vals* '[]))
  ([init]
   (fn [x]
     (into init (vals x)))))

(defn concat*
  [init]
  (fn [x]
    (into init x)))


(def samak-symbols
  {'|>     chain
   'id     identity
   'map    (curry1fn mapv)
   'filter (curry1fn filter)
   'only   #(if* % identity tt/ignore)
   'remove (curry1fn filter)
   'mapcat mapcatv
   'repeat (curry1 repeat)
   'take   (curry1 take)
   'drop   (curry1 drop)
   '=      (curry1 =)
   'many   tt/many
   'ignore tt/ignore
   'merge  merge
   'red    red
   'vals   vals*
   'sort-by (curry1 sort-by)
   'first  first
   'concat concat*
   'inc    inc
   'odd?   odd?
   'even?  even?
   'sum    sum
   'or     or*
   'and    and*
   'if     if*
   'const  constantly
   'when   when*
   '+      +
   '-      -
   '!      '!})
