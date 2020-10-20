(ns dev.core
  (:require [cljsjs.react]
            [clojure.core.async :as a :refer [<! >! chan close! put!]]
            [cognitect.transit :as t]
            [metosin.transit.dates :as d]
            [promesa.core :as p]
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
  [load in out]
  (fn
    [event]
    (let [data (t/read json-reader (.-data event))]
      ;; (println "recv from w" data)
      (condp = (:target data)
        :bootstrap (put! out :init)
        :load (put! load (:data data))
        (put! in data)))))


(defn init
  ""
  []
  (p/let [in (chan)
          out (chan)
          loading (chan)]
    (render/start-render-runtime loading in out)
    (let [w (js/Worker. "/js/oasis-worker.js")]
      (handle-send w out)
      (aset w "onmessage" (make-handler loading in out))
      (handle-update loading #(render/start-main loading))
      )
    ))

(init)
