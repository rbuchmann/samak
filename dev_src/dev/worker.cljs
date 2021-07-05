(ns dev.worker
  (:require [clojure.core.async :as a :refer [<! >! chan close! put!]]
            [cognitect.transit :as t]
            [cljs.reader :as edn]
            [metosin.transit.dates :as d]
            [samak.helpers :as helpers]
            [samak.worker :as worker])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn handle-update
  ""
  [c]
  (let [w (t/writer :json {:handlers d/writers})]
    (go-loop []
      (let [p (<! c)]
        (.postMessage js/self (t/write w {:target :load :data p})))
      (recur))))


(defn handle-request
  ""
  [c]
  (let [w (t/writer :json {:handlers d/writers})]
    (go-loop []
      (let [p (<! c)
            before (helpers/now)]
        ;; (println "to main" p)
        (.postMessage js/self (t/write w p))
        (worker/trace ::worker-out
                    (helpers/duration before (helpers/now))
                    (:samak.runtime/content p)))
      (recur))))

(def json-reader (t/reader :json {:handlers d/readers}))
(defn make-handler
  ""
  [in init]
  (fn
    [event]
    (let [before (helpers/now)
          data (t/read json-reader (.-data event))]
      ;; (println "worker in" data)
      (if (= data :init)
        (helpers/debounce init)
        (put! in data))
      (worker/trace ::worker-in
                    (helpers/duration before (helpers/now))
                    (:samak.runtime/content data)))))

(defn bootstrap
  ""
  []
  (println "bootstrap")
  (let [loading (chan)
        in (chan)
        out (chan)]
    (handle-update loading)
    (handle-request out)
    (aset js/self "onmessage" (make-handler in #(worker/start-rt loading in out)))
    (put! out {:target :bootstrap})
    ))

(bootstrap)
