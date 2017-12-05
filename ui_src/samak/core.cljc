(ns samak.core
  (:require [samak.stdlib             :as std]
            [samak.transduction-tools :as tt]
            [net.cgrand.xforms        :as x]))

(defn arity1 [f]
  (fn [x]
    (f x)))

(defn arity2 [f]
  (fn [x]
    (fn [y]
      (f x y))))

(defn arity2 [f]
  (fn [x]
    (if (= x '_)
      (fn [y]
        (fn [x2]
          (f x2 y)))
      (fn [y]
        (f x y)))))

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

(defn filter* [pred]
  (if* pred identity tt/ignore))

(def transducer-symbols
  {'mapcat     (fn [f]
                 (fn [x]
                   (tt/many (f x))))
   'filter     filter*
   'remove     (comp filter* complement)
   ;; 'reductions (arity2 x/reductions) TBD: Stateful helper in tt ns
   ;; 'take       (arity1 take)
   ;; 'drop       (arity1 drop)
   })


(def samak-symbols
  {'id identity
   'map (arity2 map)
   'or or*
   'and and*
   'if if*
   '! '!})
