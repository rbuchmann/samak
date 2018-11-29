(ns samak.builtins
  (:require [samak.stdlib             :as std]
            [samak.transduction-tools :as tt]
            [samak.protocols          :as p]
            [samak.caravan            :as c]
            [clojure.string           :as s]
            [samak.tools              :as tools]
            [samak.pipes              :as pipes]))

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


(defn wrap-samak-reducer [f]
  (fn [state nxt]
    (f {:next  nxt
        :state state})))

(defn reduce* [f i]
  (fn [x]
    (reduce (-> f p/eval-as-fn wrap-samak-reducer) i x)))

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
    ;; (println (str "!!! concat: " init " - " x))
    (into init x)))

(defn dissoc*
  ""
  [keys]
  (fn [x]
    (update-in x (drop-last keys) dissoc (last keys))))


(defn into*
  []
  (fn [[c x]]
    ;; (println (str "!!! into: " c " - " x))
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
    (get col i)))

(defn get-by-key
  ""
  []
  (fn [[col i not-found]]
    (get col i not-found)))

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

(defn eq*
  ""
  []
  (fn [v]
    (apply = v)))


(defn interleave*
  ""
  []
  (fn [[c1 c2]]
    (into [] (map vec (partition 2 (interleave c1 c2))))))

(defn partitionv
  [n]
  (fn [c]
    (into [] (map vec (partition n c)))))

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

(defn index-of*
  ""
  [s]
  (fn [x]
    (when x (s/index-of x s))))

(defn negate
  ""
  []
  (fn [x]
    (- x)))


(defn distinct*
  ""
  []
  (fn [x]
    (distinct x)))


(defn join*
  ""
  []
  (fn [s]
    (s/join s)))


(defn max*
  ""
  []
  (fn [x]
    (apply max x)))

(defn min*
  ""
  []
  (fn [x]
    (apply min x)))


(def samak-symbols
  {'->     chain
   '|>     (comp pipes/instrument chain)
   'id     identity
   'map    (curry1fn mapv)
   'reduce reduce*
   'filter (curry1fn filterv)
   'only   #(if* % identity tt/ignore)
   'remove (curry1fn filterv)
   'mapcat mapcatv
   'repeat repeat*
   'zip    interleave*
   'take   (curry1 take)
   'drop   dropv
   '=      (curry1 =)
   'eq     eq*
   '<      less
   '>      more
   '+      (curry1 +)
   '-      (curry1 -)
   '*      (curry1 *)
   'negate negate
   'nth    nth*
   'lookup get-by-key
   'count  count
   'many   tt/many
   'ignore tt/ignore
   'flatten flattenv
   'vals   vals*
   'sort-by (curry1 sort-by)
   'concat concat*
   'dissoc dissoc*
   'into   into*
   'inc    inc
   'dec    dec
   'odd?   odd?
   'even?  even?
   'sum    sum
   'mult   mult
   'max    max*
   'min    min*
   'random rand-int
   'unique distinct*
   'str    str*
   'index-of index-of*
   'join   join*
   'or     or*
   'and    and*
   'if     if*
   'const  constantly
   'when   when*
   'incase incase
   'unless unless
   'loop   loop*
   'spy    spy
   'create-sink c/create-sink
   'connect c/connect
   'add-cell c/add-cell
   '!      '!})
