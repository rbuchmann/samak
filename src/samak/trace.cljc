(ns samak.trace
  #?(:clj
     (:require [clojure.spec.alpha   :as s]
               [clojure.walk         :as w]
               [samak.helpers        :as helper]
               [samak.trace-db       :as db]
               [samak.tools          :as tools]
               [samak.api            :as api]
               [samak.runtime.stores :as store])
     :cljs
     (:require [cljs.spec.alpha :as s]
               [clojure.walk :as w]
               [samak.trace-db :as db]
               [samak.helpers :as helper]
               [samak.runtime.stores :as store]
               [samak.api :as api]
               [samak.tools :as tools])))

(def ^:dynamic *db-id* nil)
(def ^:dynamic *code* nil)

(def ^:dynamic *db* (db/create-empty-db))
(def ^:dynamic *rt* (atom {}))

(defn init-tracer
  ""
  [rt]
  (reset! *rt* rt))


(defn make-trace
  ""
  [db-id duration event]
  (merge {:samak.trace/node db-id
          :samak.trace/level :trace
          :samak.trace/duration duration
          :samak.trace/timestamp (helper/serialize-timestamp (helper/now))}
         (if (map? event) event {:value event})))

(defn trace
  ""
  [db-id duration event]
  (let [data (make-trace db-id duration event)]
    ;; (tools/log data)
    (db/persist! *db* [data])
    event))

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
    (let [ast (load-ast (:store @*rt*) node)]
      (if (api/is-def? ast)
        (str "(" node ") " (:samak.nodes/name ast))
        (str ast)))
    node))


(defn trace-to-string
  ""
  [trace]
  (let [id (:samak.pipes/uuid (:samak.pipes/meta trace))]
    (str "[" id "] "
       (helper/print-timestamp (:samak.trace/timestamp trace)) " - " (:samak.trace/duration trace) "ms: "
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
