(ns samak.trace
  #?(:clj
     (:require [clojure.spec.alpha   :as s]
               [clojure.walk         :as w]
               [samak.zipkin         :as tracing]
               [samak.trace-db       :as db]
               [samak.helpers        :as helper]
               [samak.tools          :as tools]
               [samak.api            :as api]
               [samak.runtime.stores :as store])
     :cljs
     (:require [cljs.spec.alpha :as s]
               [clojure.walk :as w]
               [samak.zipkin :as tracing]
               [samak.trace-db :as db]
               [samak.helpers :as helper]
               [samak.runtime.stores :as store]
               [samak.api :as api]
               [samak.tools :as tools])))

(def ^:dynamic *db-id* nil)
(def ^:dynamic *code* nil)

(def ^:dynamic *db* (db/create-empty-db))
(def rt (atom {}))

(def tracer (atom nil))


(defn init-tracer
  ""
  [rt-in config]
  (println "init tracer" config)
  (reset! rt rt-in)
  (when (= :zipkin (:backend config))
      (reset! tracer (tracing/init config))))


(defn load-ast
  "loads an ast given by its entity id from the database"
  [rt id]
  (w/postwalk (fn [form]
                (if-let [sub-id (when (and (map? form) (= (keys form) [:db/id]))
                                  (:db/id form))]
                 (store/load-by-id rt sub-id)
                 form))
              (store/load-by-id rt id)))

(defn node-as-str
  ""
  [node]
  (if (number? node)
    (let [ast (load-ast (:store @rt) node)]
      (if (api/is-def? ast)
        (str "(" node ") " (:samak.nodes/name ast))
        (str ast)))
    node))

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
    (println "assert failed:" event))
  (when @tracer
    (let [data (make-trace db-id duration event)]
      (reset! tracer (tracing/trace @tracer (to-tracer data)))))
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
