(ns samak.code-db
  (:require [datascript.core       :as d]
            #?(:cljs [reagent.core :as r])))

(def schema #:samak.nodes {:argument   {:db/isComponent true
                                        :db/valueType   :db.type/ref}
                           :arguments  {:db/isComponent true
                                        :db/valueType   :db.type/ref
                                        :db/cardinality :db.cardinality/many}
                           :rhs        {:db/isComponent true
                                        :db/valueType   :db.type/ref}
                           :kv-pairs   {:db/cardinality :db.cardinality/many
                                        :db/isComponent true
                                        :db/valueType   :db.type/ref}
                           :children   {:db/cardinality :db.cardinality/many
                                        :db/isComponent true
                                        :db/valueType   :db.type/ref}
                           :expression {:db/isComponent true
                                        :db/valueType   :db.type/ref}})

#?(:cljs (defn create-ratom-db [schema]
           (r/atom (d/empty-db schema) :meta {:listeners (atom {}) })))

(defn create-empty-db []
  (d/create-conn schema))

(defn parse-tree->db [db tree]
  (doto db
    (d/transact! tree)))
