(ns samak.runtime.stores
  (:require [samak.code-db :as db]
            [samak.api     :as api]))

(defprotocol SamakStore
  (persist-tree! [this tree])
  (load-by-id [this id])
  (resolve-name [this db-name]))

(defrecord LocalSamakStore [db]
  SamakStore
  (persist-tree! [_ tree]
    (db/parse-tree->db! db tree))
  (load-by-id [_ id]
    (db/load-by-id db id))
  (resolve-name [_ db-name]
    (db/resolve-name db db-name)))

(defn load-builtins! [store builtins]
  (doto store
    (persist-tree! (mapv (fn [s] (api/defexp s (api/builtin s))) builtins))))

(defn make-local-store []
  (LocalSamakStore. (db/create-empty-db)))
