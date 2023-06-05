(ns samak.helpers
  #?@
  (:clj
   [(:refer-clojure :exclude [uuid])
    (:require
     [promesa.core :as p]
     [clojure.walk :as w]
     [clojure.data.json :as json]
     [clj-time.core :as time]
     [clj-time.format :as time-format]
     [clj-time.coerce :as time-coerce])]
   :cljs
   [(:refer-clojure :exclude [uuid])
    (:require
     [goog.async.nextTick]
     [promesa.core :as p]
     [clojure.walk :as w]
     [cljs-time.core :as time]
     [cljs-time.format :as time-format]
     [cljs-time.coerce :as time-coerce])]))

(defn now
  ""
  []
  (time/now))


(defn past
  ""
  [millis]
  (time/minus (now) (time/millis millis)))

(defn future-ms
  ""
  [amount]
  (time/from-now (time/millis amount)))


(defn duration
  ""
  [from to]
  (time/in-millis (time/interval from to)))

(defn past?
  ""
  [timeout]
  (time/after? (now) timeout))


(defn serialize-timestamp
  ""
  [time]
  (time-coerce/to-date time))

(defn parse-timestamp
  ""
  [datetime]
  (time-coerce/from-date datetime))

(def form (time-format/formatters :date-time))

(defn print-ISO
  ""
  [time]
  (time-format/unparse form time))

(defn to-epoch
  ""
  [time]
  (time-coerce/to-long time))

(defn compare-timestamp
  ""
  [a b]
  (time/before? a b))


(defn uuid
  "Return a random UUID."
  []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defn hex
  []
  #?(:clj (Integer/toHexString (rand-int 16))
     :cljs (.toString (rand-int 16) 16)))


(defn make-span
  ""
  []
  (str (hex) (hex) (hex) (hex)
       (hex) (hex) (hex) (hex)))

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
     (str (subs (str s) 0 (max 0 (- n 3))) "...")
     s)))

(defn fixstring [s n]
  (apply str (concat (take n (concat (substring s n) (repeat " "))))))

(defn to-json [x]
  #?(:cljs (clj->js x)
     :clj (json/write-str x)))

(defn debounce
  ""
  [f]
  #?(:cljs (goog.async.nextTick f)
     :clj (f)))

(defn str-to-int [s]
  #?(:clj (try (Integer/parseInt s) (catch Exception e nil))
     :cljs (js/parseInt s)))

(defn pwalk
  ""
  [inner outer form]
  (cond
    (p/promise? form) (p/then form #(w/walk inner outer form))
    (list? form) (outer (apply list (p/do! (map inner form))))
    (seq? form) (outer (p/do! (map inner form)))
    (coll? form) (outer (into (empty form) (p/do! (map inner form))))
    :else (w/walk inner outer form)))

(defn ppostwalk
  ""
  [f form]
  (pwalk (partial ppostwalk f) f form))

(defn make-meta
  ""
  [specific]
  (merge {:samak.pipes/created (now)
          :samak.pipes/span (make-span)
          :samak.pipes/parent (make-span)
          :samak.pipes/cancel (uuid)
          :samak.pipes/uuid (uuid)} specific))

(defn make-paket
  ""
  ([event source]
   {:samak.pipes/meta (make-meta {:samak.pipes/source source})
    :samak.pipes/content event})
  ([event source uuid]
   {:samak.pipes/meta (make-meta {:samak.pipes/uuid uuid :samak.pipes/source source})
    :samak.pipes/content event}))
