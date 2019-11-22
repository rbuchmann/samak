(ns dev.core
  (:require [cljsjs.react]
            [clojure.core.async :as a :refer [<! >! chan close!]]
            [samak.repl :as repl])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn update-bar
  ""
  [a]
  (when-let [elem (-> js/document
                      (.getElementById "bar"))]
    (-> elem
        (.-style)
        (.-width)
        (set! (str a "%")))))

(defn handle-update
  ""
  [c]
  (go-loop []
    (let [p (<! c)]
      (update-bar p))
    (recur)))


(defn init
  ""
  []
  (println "start")
  (let [c (chan)]
    (handle-update c)
    (repl/start-oasis c)))

(init)
