(ns samak.code-db
  #?@
   (:clj
    [(:require [datascript.core :as d]
               [clojure.walk    :as w])]
    :cljs
    [(:require [datascript.core :as d]
               [reagent.core :as r]
               [clojure.walk    :as w])]))

(def schema #:samak.nodes {:arguments   {:db/isComponent true
                                         :db/valueType   :db.type/ref
                                         :db/cardinality :db.cardinality/many}
                           :fn          {:db/valueType :db.type/ref}
                           :rhs         {:db/valueType :db.type/ref}
                           :mapkv-pairs {:db/cardinality :db.cardinality/many
                                         :db/isComponent true
                                         :db/valueType   :db.type/ref}
                           :mapkey      {:db/valueType :db.type/ref}
                           :mapvalue    {:db/valueType :db.type/ref}
                           :children    {:db/cardinality :db.cardinality/many
                                         :db/isComponent true
                                         :db/valueType   :db.type/ref}
                           :expression  {:db/isComponent true
                                         :db/valueType   :db.type/ref}
                           :from        {:db/isComponent false
                                         :db/valueType   :db.type/ref}
                           :to          {:db/isComponent false
                                         :db/valueType   :db.type/ref}
                           :name        {:db/unique :db.unique/identity}
                           :node        {:db/isComponent true
                                         :db/valueType   :db.type/ref}
                           })

#?(:cljs (defn create-ratom-db [schema]
           (r/atom (d/empty-db schema) :meta {:listeners (atom {}) })))

(defn create-empty-db []
  (d/create-conn schema))

(defn parse-tree->db! [db tree]
  (d/transact! db tree))

(defn load-by-id
  "Loads an ast given by its entity id from the database.
   Will not resolve refs automatically."
  [db id]
  (d/pull @db '[*] id))

(defn resolve-name
  "Returns the db id for a given name"
  [db sym]
  (d/q '[:find ?e .
         :in $ ?sym
         :where
         [?e :samak.nodes/name ?sym]]
       @db
       sym))

(defn retrieve-links [db]
  (d/q '[:find [(pull ?id [*]) ...]
         :in $
         :where
         [?id :samak.nodes/type :samak.nodes/link]]
       @db))

;; TODO: Implement for cljs using node fs stuff
#?(:clj
   (do
     (defn write-to-disk [db filename]
       (->> db d/db pr-str (spit filename)))


     (def read-db (partial clojure.edn/read-string {:readers d/data-readers}))

     (defn load-from-disk [filename]
       (-> filename
           slurp
           read-db
           d/conn-from-db))))
