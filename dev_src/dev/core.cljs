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

(defn start-main
  [load in out]
  (render/start-render-runtime load in out))


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
  [c done]
  (go-loop []
    (let [p (<! c)]
      (do
        (update-bar p)
        (when (= p 100)
          (println "done core")
          (done))))
    (recur)))

(defn handle-send
  ""
  [worker c]
  (let [w (t/writer :json {:handlers d/writers})]
    (go-loop []
      (let [p (<! c)
            before (helpers/now)]
        ;; (println "to worker" p)
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
      ;; (println "recv from w" data)
      (condp = (:target data)
        :load (put! load (:data data))
        (put! in data)))))

(defn init
  ""
  []
  (println "start")
  (let [in (chan)
        out (chan)]
    (let [w (js/Worker. "/js/oasis-worker.js")
          loading (chan)]
      (handle-update loading #(start-main loading in out))
      (aset w "onmessage" (make-handler loading in))
      (handle-send w out)
      )
    ))

(init)
