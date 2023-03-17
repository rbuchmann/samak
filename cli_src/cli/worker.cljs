(ns cli.worker-core
  (:require [cljs.nodejs :as nodejs]
            [clojure.core.async :as a :refer [<! >! chan close! put!]]
            [cognitect.transit :as t]
            [cljs.reader :as edn]
            [metosin.transit.dates :as d]
            [samak.helpers :as helpers]
            [samak.worker :as worker])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def worker_threads (nodejs/require "worker_threads"))

(defn handle-update
  ""
  [c]
  (let [w (t/writer :json {:handlers d/writers})]
    (go-loop []
      (let [p (<! c)]
        (.postMessage (.-parentPort worker_threads) (t/write w {:target :load :data p})))
      (recur))))


(defn handle-request
  ""
  [c]
  (let [w (t/writer :json {:handlers d/writers})]
    (go-loop []
      (let [p (<! c)
            before (helpers/now)]
        ;; (println "to main" p)
        (.postMessage (.-parentPort worker_threads) (t/write w p)))
      (recur))))

(def json-reader (t/reader :json {:handlers d/readers}))
(defn make-handler
  ""
  [in init]
  (fn
    [event]
    (let [data (t/read json-reader event)]
      ;; (println "worker in" data)
      (if (= (:cmd data) :init)
        (helpers/debounce #(init (:param data)))
        (put! in data)))))

(defn bootstrap
  ""
  []
  (println "bootstrap")
  (let [loading (chan)
        in (chan)
        out (chan)]
    (handle-update loading)
    (handle-request out)
    (.on (.-parentPort worker_threads) "message" (make-handler in #(worker/start-rt loading in out)))
    (put! out {:target :bootstrap})))

(bootstrap)
