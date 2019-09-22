(ns samak.helpers
  #?
  (:clj
   (:require
    [clj-time.core :as time]
    [clj-time.format :as time-format]
    [clj-time.coerce :as time-coerce])
   :cljs
   (:require
    [cljs-time.core :as time]
    [cljs-time.format :as time-format]
    [cljs-time.coerce :as time-coerce])))

(defn now
  ""
  []
  (time/now))

(defn duration
  ""
  [from to]
  (time/in-millis (time/interval from to)))


(defn serialize-timestamp
  ""
  [time]
  (time-coerce/to-date time))

(def form (time-format/formatters :date-time))

(defn parse-timestamp
  ""
  [datetime]
  (time-coerce/from-date datetime))


(defn print-timestamp
  ""
  [time]
  (time-format/unparse form time))

(defn compare-timestamp
  ""
  [a b]
  (time/before? a b))


(defn uuid
  "Return a random UUID."
  []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defn make-span
  ""
  []
  (rand-int 1000000))

(defn str-len
  ""
  [s]
  #?(:clj (count s)
     :cljs (.-length s)))


(defn substring
  ""
  [s n]
  (str
   (if (> (str-len s) n)
     (str (subs (str s) 0 (- n 3)) "...")
     s)))
