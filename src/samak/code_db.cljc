(ns samak.code-db
  #?@
   (:clj
    [(:require [datascript.core :as d])]
    :cljs
    [(:require [datascript.core :as d] [reagent.core :as r])]))

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

(defn load-ast
  "loads an ast given by SYMBOL from the database"
  [db sym]
  (d/q '[:find ?e
         :where [?e :samak.nodes/type :samak.nodes/def]
         [?e :samak.nodes/name sym]]
       db))
