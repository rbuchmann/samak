(ns samak.core
  (:require [samak.stdlib             :as std]
            [samak.transduction-tools :as tt]
            [samak.protocols          :as p]
            [net.cgrand.xforms        :as x]))

(defn if* [pred then else]
  (fn [x]
    (if (pred x)
      ((p/eval-as-fn then) x)
      ((p/eval-as-fn else) x))))

(defn or* [& args]
  (fn [x]
    (some #(% x) (map p/eval-as-fn args))))

(defn and* [& args]
  (fn [x]
    (every? #(% x) (map p/eval-as-fn args))))

(defn chain [& args]
  (->> args reverse (map p/eval-as-fn) (apply comp)))

(defn curry1 [f]
  (fn [x]
    (partial f x)))

(defn curry2 [f]
  (fn [x y]
    (partial f x y)))

(defn filter* [pred]
  (let [pred* (p/eval-as-fn pred)]
    (fn [x]
      (if (pred* x)
        x
        (tt/ignore x)))))

(defn mapcat* [f]
  (let [f* (p/eval-as-fn f)]
    (comp tt/many f*)))

(def sum (partial apply +))

(def samak-symbols
  {'|>     chain
   'id     identity
   'map    (curry1 map)
   'filter filter*
   'mapcat mapcat*
   'remove (curry1 remove)
   'repeat (curry1 repeat)
   'take   (curry1 take)
   'drop   (curry1 drop)
   '=      (curry1 =)
   'many   tt/many
   'ignore tt/ignore
   'inc    inc
   'odd?   odd?
   'even?  even?
   'sum    sum
   'or     or*
   'and    and*
   'if     if*
   'const  constantly
   '!      '!})
