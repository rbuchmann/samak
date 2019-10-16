(ns samak.utils
  (:require #?(:clj [clojure.core.async :as a :refer [<! <!! chan go go-loop]]
               :cljs [clojure.core.async :as a :refer [<! async chan go go-loop]])
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(defn test-async
  "Asynchronous test awaiting ch to produce a value or close."
  [ch]
  #?(:clj
     (<!! ch)
     :cljs
     (async done
       (take! ch (fn [_] (done))))))
