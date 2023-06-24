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
(def rt (atom nil))

(defn trace-stream [inst out-pipe]
  (go-loop []
    (put! out-pipe :init-trace)
        (when-let [x (<! tracepipe)]
          (put! out-pipe x)
          (recur))))

(defn metric-name [data]
  (str (.getName (.getInstrumentationScopeInfo data)) "/" (.getName data)))

(defn show-metric [data]
  (condp = (.getType data)
    MetricDataType/HISTOGRAM (str (-> (.getHistogramData data)
                                         (.getPoints)
                                         (first)
                                         (#(when (some? %)
                                             (str
                                              (.getCount %) ": "
                                              (.getMin %)
                                              "/"
                                              (format "%.2f" (/ (.getSum %) (.getCount %)))
                                              "/"
                                              (.getMax %))))))
    MetricDataType/DOUBLE_GAUGE (str (-> (.getDoubleGaugeData data)
                                         (.getPoints)
                                         (first)
                                         (#(when (some? %) (.getValue %)))))
    MetricDataType/LONG_SUM (str (-> (.getLongSumData data)
                                     (.getPoints)
                                     (first)
                                     (#(when (some? %) (.getValue %)))))
    (str "unknown metrics type: " (.getType data))))

(defn convert-metrics [v]
  (let [data (into {} (map (fn [x] [(metric-name x) x]) v))
        disp (into {} (map (fn [[k v]] [k (show-metric v)]) data))]
    (sort-by first disp)))

(defn print-metrics [v]
  (println "- metrics ------------------------")
  (run! (fn [[n v]] (println "metrics" n v)) (convert-metrics v))
  (println "- /metrics -----------------------"))

(defn node-as-str
  ""
  [node]
  ;; (if (number? node)  ;; split into own ns
  ;;   (prom/let [ast (store/load-by-id (:store @rt) node)]
  ;;     (if (api/is-def? ast)
  ;;       (str "(" node ") " (:samak.nodes/name ast))
  ;;       (if (api/is-def? (:samak.nodes/fn ast))
  ;;         (str "(" node ") " (:samak.nodes/name (:samak.nodes/fn ast)))
  ;;         (str ast))))
    node)


(defn convert-traces [t]
  {:event (:samak.trace/event (:samak.pipes/content t))
   :timestamp (helpers/print-ISO (:samak.trace/timestamp (:samak.pipes/content t)))
   :duration (:samak.trace/duration (:samak.pipes/content t))
   :parent (:samak.pipes/parent (:samak.pipes/meta t))
   :span (:samak.pipes/span (:samak.pipes/meta t))
   :node (:samak.trace/node (:samak.pipes/content t))
   :runtime (:samak.trace/runtime (:samak.pipes/content t))})

(defn init
  [trac met]
  ;; (reset! rt runtime)
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
          halef-trace (chan (a/sliding-buffer 100) (map #(helpers/make-paket (convert-traces %) ::traces)))
          halef-trace-pipe (pipes/pipe halef-trace)
          halef-metrics (chan 100 (map #(helpers/make-paket (convert-metrics %) ::metrics)))
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
