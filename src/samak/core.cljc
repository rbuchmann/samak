(ns samak.core
  (:require [samak.stdlib             :as std]
            [samak.transduction-tools :as tt]
            [samak.protocols          :as p]
            [net.cgrand.xforms        :as x]))

(defn if* [pred then else]
  (fn [x]
    (if (pred x)
      (then x)
      (else x))))

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

(def samak-symbols
  {'|>     chain
   'id     identity
   'map    (curry1 map)
   'filter (curry1 filter)
   'remove (curry1 remove)
   'take   (curry1 take)
   'drop   (curry1 drop)
   'many   tt/many
   'ignore tt/ignore
   'inc    inc
   'or     or*
   'and    and*
   'if     if*
   'const 'constantly
   '!      '!})
