(ns samak.builtins
  (:require [samak.stdlib    :as std]
            [samak.protocols :as p]
            [samak.nodes     :as n]
            [samak.tools     :as tools]))

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
    (vec (mapcat (p/eval-as-fn f) x))))

(def sum (partial apply +))
(def mult (partial apply *))

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

(defn into*
  []
  (fn [[c x]]
    (println (str "into " c " put " x))
    (into c x)))

(defn flattenv
  ""
  [x]
  (into [] (flatten x)))

(defn str* [& args]
  (let [args* (map p/eval-as-fn args)]
    (fn [x] (apply str (map #(% x) args*)))))

(defn nth*
  ""
  [i]
  (fn [col]
    (nth col i)))

(defn less
  ""
  [c]
  (fn [x] (< x c)))

(defn more
  ""
  [c]
  (fn [x] (> x c)))

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

(defn loop*
  [test body init]
  (let [test* (p/eval-as-fn test)
        body* (p/eval-as-fn body)
        init* (p/eval-as-fn init)
        acc (atom nil)]
    (fn [x]
      (reset! acc (init* x))
      (while (test* @acc)
        (reset! acc (body* @acc)))
      @acc)))

(defn dropv
  [n]
  (fn [c]
    (into [] (drop n c))))


(defn interleave*
  ""
  []
  (fn [[c1 c2]]
    (into [] (map vec (partition 2 (interleave c1 c2))))))

(defn repeat*
  ""
  []
  (fn [[n x]]
    (into [] (repeat n x))))

(defn inc*
  ""
  [f]
  (fn [x]
    (inc ((p/eval-as-fn f) x))))


(def samak-symbols
  {'->      chain
   '|>      (comp pipes/instrument chain)
   'id      identity
   'map     (curry1fn mapv)
   'filter  (curry1fn filterv)
   'only    #(if* % identity tt/ignore)
   'remove  (curry1fn filterv)
   'mapcat  mapcatv
   'repeat  repeat*
   'zip     interleave*
   'take    (curry1 take)
   'drop    dropv
   '=       (curry1 =)
   '<       less
   '>       more
   '+       (curry1 +)
   '*       (curry1 *)
   'nth     nth*
   'count   count
   'many    tt/many
   'ignore  tt/ignore
   'flatten flattenv
   'vals    vals*
   'sort-by (curry1 sort-by)
   'concat  concat*
   'into    into*
   'inc     inc*
   'dec     dec
   'odd?    odd?
   'even?   even?
   'sum     sum
   'mult    mult
   'str     str*
   'or      or*
   'and     and*
   'if      if*
   'const   constantly
   'when    when*
   'incase  incase
   'unless  unless
   'loop    loop*
   'spy     spy
   'net     net
   '!       '!})
