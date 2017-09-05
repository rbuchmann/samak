(ns ui.code-db
  (:require [datascript.core :as d]
            [reagent.core    :as r]))

(def schema {:arguments   {:db/cardinality :db.cardinality/many
                           :db/isComponent true
                           :db/valueType   :db.type/ref}
             :lhs         {:db/isComponent true
                           :db/valueType   :db.type/ref}
             :rhs         {:db/isComponent true
                           :db/valueType   :db.type/ref}
             :kv-pairs    {:db/cardinality :db.cardinality/many
                           :db/isComponent true
                           :db/valueType   :db.type/ref}
             :children    {:db/cardinality :db.cardinality/many
                           :db/isComponent true
                           :db/valueType   :db.type/ref}
             :definitions {:db/cardinality :db.cardinality/many
                           :db/isComponent true
                           :db/valueType   :db.type/ref}
             :chans       {:db/cardinality :db.cardinality/many
                           :db/isComponent true
                           :db/valueType   :db.type/ref}
             :expression  {:db/isComponent true
                           :db/valueType   :db.type/ref}})

(defn create-ratom-db [schema]
  (r/atom (d/empty-db schema) :meta {:listeners (atom {}) }))

(defn parse-tree->db [db tree]
  (doto db
    (d/transact! [tree])))
