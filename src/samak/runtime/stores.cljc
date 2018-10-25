(ns samak.runtime.stores
  (:require [samak.code-db :as db]))

(defprotocol SamakStore
  (persist-tree! [this tree])
  (load-by-id [this id])
  (resolve-name [this db-name])
  (load-links [this]))


(defrecord LocalSamakStore [db]
  SamakStore
  (persist-tree! [_ tree]
    (db/parse-tree->db! db tree))
  (load-by-id [_ id]
    (db/load-by-id db id))
  (resolve-name [_ db-name]
    (db/resolve-name db db-name))
  (load-links [_]
    (db/retrieve-links db)))
