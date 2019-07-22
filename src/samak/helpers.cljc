(ns samak.helpers
  #?
  (:clj
   (:require
    [clj-time.core :as time])
   :cljs
   (:require
    [cljs-time.core :as time])))

(defn now
  ""
  []
  (time/now))

(defn uuid
  "Return a random UUID."
  []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))
