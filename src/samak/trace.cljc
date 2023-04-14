(ns samak.trace
  #?(:clj
     (:require
      [clojure.core.async :as a :refer [chan put! <! go-loop]]
      [clojure.spec.alpha   :as s]
      [clojure.walk         :as w]
      [promesa.core         :as prom]
      [samak.zipkin         :as tracing]
      [samak.trace-db       :as db]
      [samak.helpers        :as helper]
      [samak.tools          :as tools]
      [samak.metrics        :as metrics]
      [samak.api            :as api]
      ;; [samak.runtime.stores :as store]
      )
     :cljs
     (:require
      [cljs.core.async :as a :refer [chan put! <!]]
      [cljs.spec.alpha :as s]
      [clojure.walk :as w]
      [promesa.core :as prom]
      [samak.zipkin :as tracing]
      [samak.trace-db :as db]
      [samak.helpers :as helper]
      ;; [samak.runtime.stores :as store]
      [samak.api :as api]
      [samak.metrics :as metrics]
      [samak.tools :as tools])))

(def ^:dynamic *db-id* nil)
(def ^:dynamic *code* nil)

(def ^:dynamic *db* (db/create-empty-db))
(def rt (atom {}))

(def tracer (atom nil))

(def meters (atom {}))

(defn get-meter [s n t]
  (let [k (str s "-" n)]
    (or (get meters k)
        (let [m (swap! meters #(if (contains? % k) % (assoc % k (metrics/get-meter s n t))))]
          (get m k)))))


(defn init-tracer
  ""
  [rt-in config]
  (println "init tracer" config)
  (reset! rt rt-in)
  (when (= :zipkin (:backend config))
    (reset! tracer (tracing/init config)))
  (when (= :samak (:backend config))
    (reset! tracer {:trace-fn (fn [t x] (when (= 0 (rand-int 1)) (put! (:chan config) {:samak.pipes/content x})) t)}))
  (when (= :logging (:backend config))
    (let [pre (or (:prefix config) "TRACE -")]
      (reset! tracer {:trace-fn (fn [t x] (println pre x) t)}))))


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
  ;;   node)
  node
  )

(defn make-trace
  ""
  [db-id duration event]
  (merge {:samak.trace/runtime (:id @rt)
          :samak.trace/node db-id
          :samak.trace/level :trace
          :samak.trace/duration duration
          :samak.trace/timestamp (helper/past duration)}
         (if (map? event) event {:value event})))

(defn to-db
  [data]
  (update data :samak.trace/timestamp helper/serialize-timestamp))

(defn to-tracer
  [data]
  (update data :samak.trace/node node-as-str))

(defn trace
  ""
  [db-id duration event]
  (if-not (:samak.pipes/uuid (:samak.pipes/meta event))
    (when event
      (println "no traceable event:" event)))
  (let [meter (get-meter (str "node-" db-id) "call" :counter)]
      (.add meter 1))
  (when @tracer
    (let [data (make-trace db-id duration event)]
      (reset! tracer ((:trace-fn @tracer) @tracer (to-tracer data)))))
   ;; (tools/log data)
   ;; (db/persist! *db* [(to-db data)])
  event)


(defn trace-to-string
  ""
  [trace]
  (let [id (:samak.pipes/uuid (:samak.pipes/meta trace))]
    (str "*" (:samak.trace/runtime trace) "* [" id "] "
       (helper/print-ISO (:samak.trace/timestamp trace)) " - " (:samak.trace/duration trace) "ms: "
       (node-as-str (:samak.trace/node trace)) " - "
       (let [c (str (:samak.pipes/content trace))]
         (helper/substring c 100)))))


(defn dump
  ""
  []
  (let [traces-db (db/load-traces *db*)
        traces (sort #(helper/compare-timestamp (:samak.trace/timestamp %1) (:samak.trace/timestamp %2))
                     traces-db)]

    (doall (map #(println (trace-to-string %)) traces)))
  (println (str "nodes: "(db/load-nodes *db*))))
