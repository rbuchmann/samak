(ns ui.samak.pipes
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as a :refer [put! chan <! >! timeout close!]]
            [ui.samak.stdlib :as std]))

(defn from-seq [col]
  (let [ch (chan)]
    (go
      (<! std/start-chan)
      (a/onto-chan ch col))
    (a/mult ch)))

(defn log []
  (let [log-chan (chan)]
    (go-loop []
      (when-let [x (<! log-chan)]
        (.log js/console x)
        (recur)))
    log-chan))
