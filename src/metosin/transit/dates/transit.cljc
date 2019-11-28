(ns metosin.transit.dates
  "Transit readers and writers for JodaTime and goog.date.
  Supports two types:
  - DateTime (org.joda.time.DateTime, goog.date.UtcDateTime)
  - LocalDate (org.joda.time.LocalDate, goog.date.Date)
  Represents DateTimes in RFC 3339 format: yyyy-mm-ddTHH:MM:SS.sssZ.
  RFC 3339 format is an specific profile of ISO 8601 DateTime format.
  Some consideration has been made to provide performant read
  implemenation for ClojureScript."
  (:require [cognitect.transit :as transit]
            [clojure.string :as string]
            #?@(:cljs [[goog.string :as gs]
                       goog.date.UtcDateTime
                       goog.date.Date]))
  #?(:clj (:import [org.joda.time])))

#?(:clj (set! *warn-on-reflection* true))

(def DateTime #?(:clj org.joda.time.DateTime, :cljs goog.date.UtcDateTime))
(def LocalDate #?(:clj org.joda.time.LocalDate, :cljs goog.date.Date))

(defn write-date-time
  "Represent DateTime in RFC3339 format string."
  [d]
  #?(:clj  (.toString (.withZone ^org.joda.time.DateTime d (org.joda.time.DateTimeZone/forID "UTC")))
     :cljs (str (.getUTCFullYear d)
                "-" (gs/padNumber (inc (.getUTCMonth d)) 2)
                "-" (gs/padNumber (.getUTCDate d) 2)
                "T" (gs/padNumber (.getUTCHours d) 2)
                ":" (gs/padNumber (.getUTCMinutes d) 2)
                ":" (gs/padNumber (.getUTCSeconds d) 2)
                "." (gs/padNumber (.getUTCMilliseconds d) 3)
                "Z")))

(defn read-date-time
  "Read RFC3339 string to DateTime."
  [s]
  #?(:clj  (org.joda.time.DateTime/parse s)
     :cljs (goog.date.UtcDateTime.fromIsoString s)))

(defn write-local-date
  "Represent Date in YYYY-MM-DD format."
  [x]
  #?(:clj  (.toString ^org.joda.time.LocalDate x)
     :cljs (.toIsoString x true false)))

(defn read-local-date
  "Read Date in YYYY-MM-DD format."
  [x]
  #?(:clj  (org.joda.time.LocalDate/parse x)
     :cljs (let [[_ y m d] (re-find #"(\d{4})-(\d{2})-(\d{2})" x)]
             (goog.date.Date. (long y) (dec (long m)) (long d)))))

(def writers
  {DateTime  (transit/write-handler (constantly "DateTime") write-date-time)
   LocalDate (transit/write-handler (constantly "Date") write-local-date)})

(def readers
  {"DateTime" (transit/read-handler read-date-time)
   "Date"     (transit/read-handler read-local-date)})