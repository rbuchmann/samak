(ns samak.core
  (:require [samak.stdlib      :as std]
            [net.cgrand.xforms :as x]))

(defn arity1 [f]
  (fn [x]
    (f x)))

(defn arity2 [f]
  (fn [x]
    (fn [y]
      (f x y))))

(defn arity3 [f]
  (fn [x]
    (fn [y]
      (fn [z]
        (f x y z)))))

(defn if* [pred then else]
  (fn [x]
    (if (pred x)
      (then x)
      (else x))))

(defn or* [a b]
  (fn [x]
    (or (a x) (b x))))

(defn and* [a b]
  (fn [x]
    (and (a x) (b x))))

(def transducer-symbols
  {'map        (arity1 map)
   'mapcat     (arity1 mapcat)
   'filter     (arity1 filter)
   'remove     (arity1 remove)
   'reductions (arity2 x/reductions)
   'take       (arity1 take)
   'drop       (arity1 drop)})


(def samak-symbols
  {'id identity
   'map (arity2 map)
   'or or*
   'and and*
   'if if*})


(defn map* [f]
  (std/transduction-pipe (map f)))

(defn filter* [f]
  (std/transduction-pipe (filter f)))

(defn reductions* [f init]
  (std/transduction-pipe (x/reductions f init)))
