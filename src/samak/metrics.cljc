(ns samak.metrics
  #?@(:clj
     [(:require
      [clojure.core.async :as a :refer [chan put! <! go-loop mult]]
      [promesa.core         :as prom]
      [samak.zipkin         :as tracing]
      [samak.trace-db       :as db]
      [samak.helpers        :as helper]
      [samak.tools          :as tools]
      [samak.api            :as api])
      (:import
       (java.util.concurrent TimeUnit)
       (io.opentelemetry.sdk OpenTelemetrySdk)
       (io.opentelemetry.sdk.common CompletableResultCode)
       (io.opentelemetry.sdk.metrics.export PeriodicMetricReader MetricExporter AggregationTemporalitySelector)
       (io.opentelemetry.exporter.logging LoggingMetricExporter)
       (io.opentelemetry.api OpenTelemetry)
       (io.opentelemetry.sdk.metrics SdkMeterProvider)
       (io.opentelemetry.sdk.metrics.data AggregationTemporality))]
     :cljs
     [(:require
      [cljs.core.async :as a :refer [chan put! <!]]
      [cljs.spec.alpha :as s]
      ["@opentelemetry/api" :as otel]
      [clojure.walk :as w]
      [promesa.core :as prom]
      [samak.zipkin :as tracing]
      [samak.trace-db :as db]
      [samak.helpers :as helper]
      [samak.api :as api]
      [samak.tools :as tools])]))

(def otel (atom nil))
(def metrics-chan (atom (chan)))

(defn reify-chan-exporter [c]
  (reify
    AggregationTemporalitySelector
    (getAggregationTemporality [_ _] AggregationTemporality/CUMULATIVE)
    MetricExporter
    (flush [_] (CompletableResultCode/ofSuccess))
    (shutdown [_] (CompletableResultCode/ofSuccess))
    (export [_ m]
      (put! c m)
      (CompletableResultCode/ofSuccess))))

(defn init-metrics []
  #?(:clj (try (let [c (chan)
                     m (mult c)
                     r (-> (reify-chan-exporter c)
                           (PeriodicMetricReader/builder)
                           (.setInterval 10 TimeUnit/SECONDS)
                           (.build))
                     meter (-> (SdkMeterProvider/builder)
                               (.registerMetricReader r)
                               (.build))
                     sdkOtel (-> (OpenTelemetrySdk/builder)
                                 (.setMeterProvider meter)
                                 (.buildAndRegisterGlobal))]
                 (reset! otel sdkOtel)
                 m)
               (catch RuntimeException ex (println (ex-info "error on metrics" ex))))
     :cljs (println "unimplemented")))

(defn get-builder [m t n]
  (condp = t
    :counter (.counterBuilder m n)
    :updown (.upDownCounterBuilder m n)
    :gauge (.gaugeBuilder m n)
    :histogram (.histogramBuilder m n)))

(defn get-meter [s n t]
  #?(:clj (try (-> (.getMeter @otel s)
                   (get-builder t n)
                   (.build))
            (catch RuntimeException ex (println (ex-info "error on metrics" ex))))))
