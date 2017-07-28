(ns ui.samak.pipes
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as a :refer [put! chan <! >! timeout close!]]
            [ui.samak.stdlib :as std]
            [reagent.core    :as r]))

(defn from-seq [col]
  (std/source (a/to-chan col)))

(defn log []
  (let [log-chan (chan)]
    (go-loop []
      (when-let [x (<! log-chan)]
        (.log js/console x)
        (recur)))
    (std/sink log-chan)))

(defn tick []
  (let [tick-chan (chan)
        state (atom 0)]
    (go-loop []
      (when (= @state 0)
        (<! std/start-chan))
      (>! tick-chan @state)
      (swap! state inc)
      (<! (timeout 1000))
      (recur))
    (std/source tick-chan)))

(defn ui []
  (let [ui-chan (chan)]
    (go-loop []
      (when-some [x (<! ui-chan)]
        (when-let [node (js/document.getElementById "generated-app")]
          (r/render x node))
        (recur)))
    (std/sink ui-chan)))
