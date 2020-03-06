(ns samak.opentracing
  ;; (:require [cljsjs.opentracing :as opentracing])
  )

(defn init
  ""
  []
  (let [Tracer (.-Tracer js/opentracing)]
    (Tracer.))
  )

(defn startSpan
  ""
  [tracer]
  (let [span (.startSpan tracer)]
    (js/console.error span)
    (.end span)))
