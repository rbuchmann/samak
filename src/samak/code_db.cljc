(ns samak.code-db
  (:require [datascript.core :as d]
            [clojure.walk    :as w]))

(def schema #:samak.nodes {:arguments     {:db/isComponent true
                                           :db/valueType   :db.type/ref
                                           :db/cardinality :db.cardinality/many}
                           :fn            {:db/valueType   :db.type/ref
                                           :db/isComponent false}
                           :fn-expression {:db/valueType   :db.type/ref
                                           :db/isComponent true}
                           :xf            {:db/valueType   :db.type/ref
                                           :db/isComponent true}
                           :rhs           {:db/valueType   :db.type/ref
                                           :db/isComponent true}
                           :mapkv-pairs   {:db/cardinality :db.cardinality/many
                                           :db/isComponent true
                                           :db/valueType   :db.type/ref}
                           :mapkey        {:db/valueType   :db.type/ref
                                           :db/isComponent true}
                           :mapvalue      {:db/valueType   :db.type/ref
                                           :db/isComponent true}
                           :children      {:db/cardinality :db.cardinality/many
                                           :db/isComponent true
                                           :db/valueType   :db.type/ref}
                           :from          {:db/isComponent true
                                           :db/valueType   :db.type/ref}
                           :to            {:db/isComponent true
                                           :db/valueType   :db.type/ref}
                           :name          {:db/unique :db.unique/identity}
                           :node          {:db/isComponent true
                                           :db/valueType   :db.type/ref}})

(defn mapcatv [f col]
  (vec (mapcat f col)))

(def dependency-rule
  (concat
   (mapcatv
    (fn [[k {:keys [db/valueType db/isComponent]}]]
      (cond-> []
        ;; All ref attribues are children
        (= :db.type/ref valueType) (conj ['(child ?p ?c)
                                          ['?p k '?c]])
        ;; All non-component refs are dependencies
        (and (not isComponent)
             (= :db.type/ref valueType)) (conj ['(depends ?p ?c)
                                                ['?p k '?c]])))
    schema)
   '[[(depends ?p ?c)
      (child ?p ?intermediate)
      (depends ?intermediate ?c)]
     [(dep-for ?p ?a ?b)
      [(= ?p ?a)]
      (depends ?a ?b)]
     [(dep-for ?p ?a ?b)
      (depends ?p ?a)
      (depends ?a ?b)]]))

;; TODO: Not actually needed, (re)move?
;; #?(:cljs (defn create-ratom-db [schema]
;;            (r/atom (d/empty-db schema) :meta {:listeners (atom {}) })))

(defn create-empty-db []
  (d/create-conn schema))

(defn parse-tree->db! [db tree]
  (d/transact! db tree))

(defn load-by-id
  "Loads an ast given by its entity id from the database.
   Will not resolve refs automatically."
  [db id]
  (d/pull @db '[*] id))


(defn load-defs
  [db]
  (d/q '[:find [(pull ?id [*]) ...]
         :in $
         :where [?id :samak.nodes/type :samak.nodes/def]]
       @db))

(defn load-links [db]
  (d/q '[:find [(pull ?id [*]) ...]
         :in $
         :where [?id :samak.nodes/type :samak.nodes/link]]
       @db))

(defn load-recurse
  "loads an ast given by its entity id from the database"
  [db id]
  (w/postwalk (fn [form]
                (if-let [sub-id (when (and (map? form) (= (keys form) [:db/id]))
                                  (:db/id form))]
                  (load-recurse db sub-id)
                 form))
              (load-by-id db id)))

(defn resolve-name
  "Returns the db id for a given name"
  [db sym]
  (d/q '[:find ?e .
         :in $ ?sym
         :where
         [?e :samak.nodes/name ?sym]]
       @db
       sym))

(defn name-node
  "Returns known names of references to id"
  [db id]
  (d/q '[:find [?ref]
         :in $ ?id
         :where
         [?e :samak.nodes/name ?ref]
         [?e :samak.nodes/rhs ?fn]
         [?fn :samak.nodes/fn ?id]]
       @db
       id))


(defn find-links-from
  "finds all pipes that link from source to somewhere else"
  [db source]
  (when source
    (d/q '[:find [(pull ?pipe [*]) ...]
           :in $ ?source
           :where

           [?pipe :samak.nodes/from ?sourceref]
           [?sourceref :samak.nodes/fn ?source]]
         @db
         source)))

(defn load-travel
  "loads a network given a source entity id from the database"
  [db id]
  (let [ast (load-by-id db id)
        subs (mapv :db/id (find-links-from db id))
        pipes (mapv #(load-by-id db %1) subs)
        ends (mapv #(get-in %1 [:samak.nodes/to :samak.nodes/fn :db/id]) pipes)
        xf (mapv #(or (get-in %1 [:samak.nodes/xf :samak.nodes/fn :db/id])
                      (get-in %1 [:samak.nodes/xf :samak.nodes/fn-expression :samak.nodes/fn :db/id])) pipes)
        rec (reduce (fn [{ends :ends pipes :pipes xf :xf} s]
                      (let [add (load-travel db s)]
                        {:ends (into ends (:ends add))
                         :xf (into xf (:xf add))
                         :pipes (into pipes (:pipes add))}))
                    {:ends [] :pipes [] :xf []}
                    ends)]
    {:ends (into ends (:ends rec))
     :xf (into xf (:xf rec))
     :pipes (into pipes (:pipes rec))}))


(defn load-network
  "loads the specified network from the database"
  [db net]
  (let [loaded (load-travel db net)]
    {net {:ends (sort (distinct (:ends loaded)))
          :xf (sort (distinct (remove nil? (:xf loaded))))
          :pipes (:pipes loaded)}}))

(defn load-dependencies [db id]
  (d/q '[:find [?x ...]
         :in $ % ?id
         :where (depends ?id ?x)]
       @db
       dependency-rule
       id))

(defn load-dependency-edges [db id]
  (d/q '[:find ?a ?b
         :in $ % ?id
         :where (dep-for ?id ?a ?b)]
       @db
       dependency-rule
       id))

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
