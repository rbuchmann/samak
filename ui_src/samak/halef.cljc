(ns samak.halef
  #?@(:clj
     [(:require
      [clojure.core.async :as a :refer [chan put! <! go-loop]]
      [promesa.core         :as prom]
      [samak.metrics        :as metrics]
      [samak.helpers        :as helpers]
      [samak.pipes          :as pipes]
      [samak.trace          :as trace]
      [samak.tools          :as tools]
      [samak.api            :as api])
      (:import
       (io.opentelemetry.sdk.metrics.data MetricDataType))]
     :cljs
     [(:require
      [cljs.core.async :as a :refer [chan put! <!]]
      [cljs.spec.alpha :as s]
      [samak.trace :as trace]
      [samak.pipes :as pipes]
      [samak.helpers :as helpers]
      [samak.api :as api]
      [samak.tools :as tools])]))

(def tracepipe (chan 100))
(def metpipe (chan 100))
(def trace-source (atom nil))
(def metrics-source (atom nil))

(defn trace-stream [inst out-pipe]
  (go-loop []
    (put! out-pipe :init-trace)
        (when-let [x (<! tracepipe)]
          (put! out-pipe x)
          (recur))))

(defn metric-name [data]
  (str (.getName (.getInstrumentationScopeInfo data)) "/" (.getName data)))

(defn show-metric [data]
  (case = (.getType data)
        MetricDataType/LONG_SUM (str (-> (.getLongSumData data)
                                         (.getPoints)
                                         (first)
                                         (.getValue)))))

(defn convert-metrics [v]
  (let [data (into {} (map (fn [x] [(metric-name x) x]) v))
        disp (into {} (map (fn [[k v]] [k (show-metric v)]) data))]
    disp))

(defn print-metrics [v]
  (println "- metrics ------------------------")
  (run! (fn [[n v]] (println "metrics" n v)) (convert-metrics v))
  (println "- /metrics -----------------------"))

(defn init
  [trac met]
  (reset! trace-source trac)
  (reset! metrics-source met)
  (a/tap trac tracepipe)
  (a/tap met metpipe))

(defn halef-module
  ""
  [prefix]
  (println "def halef" prefix)
  (fn []
    (let [inst (str prefix "-" (helpers/uuid))
          halef-in (pipes/pipe-chan ::in nil)
          halef-in-pipe (pipes/sink halef-in)
          halef-trace (pipes/pipe-chan ::trace nil)
          halef-trace-pipe (pipes/pipe halef-trace)
          halef-metrics (chan 100 (map convert-metrics))
          halef-metrics-pipe (pipes/pipe halef-metrics)]
      (a/pipe tracepipe (pipes/in-port halef-trace-pipe))
      (a/pipe metpipe halef-metrics)
      (println "init halef" inst)
      (go-loop []
        (when-let [x (<! halef-in)]
          (tools/log "halef-in: " inst x)
          (when-let [call (:call (:samak.pipes/content x))]
            (do
              (tools/log "halef-call: " inst call)
              (case (:action call)
                (tools/log "actions unknown: " call))))
          (recur)))
      (let [foo {:sources {:trace halef-trace-pipe
                           :metrics halef-metrics-pipe}
                 :sinks {}}]
        (println "halef is" inst "->" foo)
        foo))))

(def samak-symbols {'modules/halef halef-module})
