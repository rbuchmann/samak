(ns ui.samak.stdlib
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as a :refer [put! chan <! >! timeout close!]]))

(def start-chan (a/promise-chan))

(defn link [from transducers to]
  (let [xf (apply comp transducers)
        sink (chan 1 xf)]
    (a/pipe sink to false)
    (a/tap from sink)))


(defn start []
  (put! start-chan :go))
