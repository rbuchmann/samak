(ns dev.core
  (:require [cljsjs.react]
            [clojure.core.async :as a :refer [<! >! chan close! put!]]
            [cognitect.transit :as t]
            [metosin.transit.dates :as d]
            [dev.render :as render]
            [samak.runtime :as run]
            [samak.helpers :as helpers]
            [samak.builtins :as builtins])
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

(defn handle-send
  ""
  [worker c]
  (let [w (t/writer :json {:handlers d/writers})]
    (go-loop []
      (let [p (<! c)
            before (helpers/now)]
        (.postMessage worker (t/write w p))
        (render/trace ::render-out
                    (helpers/duration before (helpers/now))
                    (:samak.runtime/content p)))
      (recur))))

(def json-reader (t/reader :json {:handlers d/readers}))
(defn make-handler
  ""
  [load in]
  (fn
    [event]
    (let [data (t/read json-reader (.-data event))]
      (condp = (:target data)
        :load (put! load (:data data))
        (put! in data)))))

(defn init
  ""
  []
  (println "start")
  (let [in (chan)
        out (chan)]
    (let [;; w (js/Worker. "/js/oasis-worker.js")
          loading (chan)]
      (render/start-render-runtime loading in out)
      (handle-update loading)
      ;; (aset w "onmessage" (make-handler loading in))
      ;; (handle-send w out)
      ;; (.postMessage w (pr-str {:cmd "init" :args {:name "tl"}}))
      )
    ))

(init)
