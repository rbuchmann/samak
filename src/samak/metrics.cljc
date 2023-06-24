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
       (java.util.concurrent ForkJoinPool)
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

(defn get-builder [m t n]
  (condp = t
    :counter (.counterBuilder m n)
    :updown (.upDownCounterBuilder m n)
    :gauge (.gaugeBuilder m n)
    :histogram (.histogramBuilder m n)))

(defn get-meter [s n t & c]
  #?(:clj (try (let [builder (-> (.getMeter @otel s)
                                 (get-builder t n))]
               (if (= t :gauge)
                 (.buildWithCallback builder (first c))
                 (.build builder)))
               (catch RuntimeException ex
                 (println ex)
                 (println ex (ex-info "error on getting metrics" ex))))))

(defn get-gauge [s n f]
  (get-meter s n :gauge
             (reify java.util.function.Consumer
               (accept [this gauge] (.record gauge (double (f)))))))

(defn init-metrics []
  #?(:clj (try (let [c (chan)
                     m (mult c)
                     r (-> (reify-chan-exporter c)
                           (PeriodicMetricReader/builder)
                           (.setInterval 1000 TimeUnit/MILLISECONDS)
                           (.build))
                     meter (-> (SdkMeterProvider/builder)
                               (.registerMetricReader r)
                               (.build))
                     sdkOtel (-> (OpenTelemetrySdk/builder)
                                 (.setMeterProvider meter)
                                 (.buildAndRegisterGlobal))]
                 (reset! otel sdkOtel)
                 (get-gauge "samak-runtime-jvm"
                            (name :cp_size)
                            #(.getParallelism (ForkJoinPool/commonPool)))
                 (get-gauge "samak-runtime-jvm"
                            (name :cp_running)
                            #(.getRunningThreadCount (ForkJoinPool/commonPool)))
                 (get-gauge "samak-runtime-jvm"
                            (name :cp_active)
                            #(.getActiveThreadCount (ForkJoinPool/commonPool)))
                 (get-gauge "samak-runtime-jvm"
                            (name :cp_submitted)
                            #(.getQueuedSubmissionCount (ForkJoinPool/commonPool)))
                 (get-gauge "samak-runtime-jvm"
                            (name :cp_tasks)
                            #(.getQueuedTaskCount (ForkJoinPool/commonPool)))
                 m)
               (catch RuntimeException ex
                 (println ex)
                 (println (ex-info "error on metrics" ex))))
     :cljs (println "unimplemented")))
