(ns samak.trace-db
  (:require [datascript.core :as d]
            [samak.helpers :as helper]))

(def schema #:samak.trace {;; :node
                           ;; :level
                           ;; :timestamp
                           ;; :content
                           ;; :meta {:samak.pipes/created :samak.pipes/uuid}
                           })

(defn create-empty-db []
  (d/create-conn schema))

(defn persist! [db trace]
  (d/transact! db trace))

(defn load-by-id
  "Loads an ast given by its entity id from the database.
   Will not resolve refs automatically."
  [db id]
  (d/pull @db '[*] id))

(defn convert-trace
  ""
  [trace]
  (update-in trace [:samak.trace/timestamp] helper/parse-timestamp))

(defn find-traces
  [db]
  (d/q '[:find [(pull ?traces [*]) ...]
         :in $
         :where
         [?traces :samak.pipes/meta]
         ]
       @db))


(defn load-traces
  ""
  [db]
  (map convert-trace (find-traces db)))

(defn load-nodes
  [db]
  (d/q '[:find ?node
         :in $
         :where
         [?trace :samak.trace/node ?node]]
       @db))
