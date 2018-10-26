(ns samak.caravan
  (:require [clojure.string :as s]
            [samak.api      :as api]
            [samak.code-db  :as db]
            [samak.stdlib   :as std]))

(def db-conn (atom {}))
(def fns (atom {}))
(def net (atom []))


(defmulti handle-node :samak.nodes/type)

(defmethod handle-node
  :samak.nodes/fn-call
  [node]
  {:type "func"
   :value (str (:samak.nodes/name (:samak.nodes/fn node)))})

(defmethod handle-node
  :samak.nodes/integer
  [node]
  {:type "int"
   :value (str (:samak.nodes/value node))})

(defmethod handle-node
  :samak.nodes/string
  [node]
  {:type "str"
   :value (str (:samak.nodes/value node))})

(defmethod handle-node
  :samak.nodes/keyword
  [node]
  {:type "kw"
   :value (str (:samak.nodes/value node))})

(defn make-sym
  [value]
  {:type "sym"
   :value value})

(defmethod handle-node
  :samak.nodes/symbol
  [node]
  (make-sym (str (:samak.nodes/value node))))

(defmethod handle-node
  :samak.nodes/float
  [node]
  {:type "float"
   :value (str (:samak.nodes/value node))})

(defmethod handle-node
  :samak.nodes/map
  [node]
  {:type "table"
   :value "###"})

(defmethod handle-node
  :samak.nodes/vector
  [node]
  {:type "list"
   :value "---"})

(defmethod handle-node
  :samak.nodes/key-fn
  [node]
  {:type "acc"
   :value (str ":-" (name (:samak.nodes/value node)))})

(defmethod handle-node
  :default
  [node]
  (if (and (vector? node) (= (count node) 2) (= :samak.nodes/name (first node)))
    (make-sym (str (second node)))
    (do (println (str "!!! unknown node: " (get node :samak.nodes/type) " - " node))
        {:type (str "unknown: " (get node :samak.nodes/type))})))

(defn get-children
  ""
  [node]
  (cond
    (api/is-vector? node) (map :samak.nodes/node (get node :samak.nodes/children))
    (api/is-map? node) (get node :samak.nodes/mapkv-pairs)
    (api/is-entry? node) [(get node :samak.nodes/mapvalue)]
    (api/is-fn-call? node) (map :samak.nodes/node (get node :samak.nodes/arguments))))


(defn parse-node
  ""
  [ast lvl]
  (if-not ast
    []
    (let [children (get-children ast)
          result (mapv #(parse-node % (inc lvl)) children)]
      (into [(assoc (handle-node (or (:samak.nodes/mapkey ast) ast)) :level lvl)] result))))


(defn make-cell-list
  ""
  [src]
  (into [] (map-indexed (fn [i c] (assoc c :counter (inc i)))
                (flatten (parse-node (:samak.nodes/rhs src) 1)))))

(defn notify-source
  ""
  [src]
  (std/notify-source src))


(defn is-sink?
  ""
  [exp]
  (let [rhs-fn (:samak.nodes/fn (:samak.nodes/rhs exp))
        has-name (get rhs-fn :samak.nodes/name)
        fn-name (str (:samak.nodes/name rhs-fn))
        is-stdlib (s/starts-with? fn-name "pipes/")]
    (and (api/is-def? exp)
         has-name
         is-stdlib)))

(defn add-node
  ""
  [sym fn]
  (swap! fns assoc sym fn)
  (println (str "state is: " @fns))
  (let [type (if (is-sink? fn) :caravan/sink :caravan/func)
        ast (make-cell-list fn)]
    (if (empty? ast)
      (println (str "ERROR: no ast for: " sym " - " fn))
      (notify-source {:caravan/type type
                      :caravan/name sym
                      :caravan/ast ast}))))

(defn add-pipe
  ""
  [pipe]
  (let [args (:samak.nodes/arguments pipe)
        source (str (second (:samak.nodes/node (first args))))
        func (str (second (:samak.nodes/node (second args))))
        sink (str (second (:samak.nodes/node (nth args 2))))]
    (swap! net conj pipe)
    (notify-source {:caravan/type :caravan/pipe
                    :caravan/source source
                    :caravan/func func
                    :caravan/sink sink})))


(defn repl-eval
  [exp]
  (if (api/is-pipe? exp)
    (add-pipe exp)
    (add-node (:samak.nodes/name exp) exp)))


(defn find-cell
  [src cell counter parent]
  ;; (println (str "find: " src ",  " cell ",  " counter ",  " parent))
  (if (= counter cell)
    [src parent]
    (first (filter some? (map-indexed
                          (fn [i child] (find-cell child cell (+ counter i 1) src))
                          (get-children src))))))

(defn add-cell-internal
  ""
  [src cell]
  (let [root (:samak.nodes/rhs src)]
    (find-cell root cell 0 nil)))

(defn persist!
  ""
  [db tx-records]
  (let [new-ids (-> (db/parse-tree->db! db tx-records)
                    :tempids
                    (dissoc :db/current-tx)
                    vals)]
    (into {}
          (for [id new-ids]
            [id (db/load-by-id db id)]))))


(defn single!
  ""
  [exp]
  (let [loaded (persist! @db-conn [(assoc exp :db/id -1)])
        ast (first (vals loaded))]
    ast))

(defn add-cell
  ""
  []
  (fn [x]
    (println (str "adding: " x))
    (let [sym (symbol (:name x))
          src (get @fns sym)
          idx (dec (:cell x))]
      (when (and sym src idx)
          (let [[cell par] (add-cell-internal src idx)
                cell-id (:db/id cell)
                content (api/string "bar")
                updated (update cell :samak.nodes/arguments conj {:db/id -1 :order 1 :samak.nodes/node content})]
            (println (str "cell: " cell " - " par))
            (println (str "updated: " updated))
            (let [write (persist! @db-conn [updated])
                  load (db/load-by-id @db-conn cell-id)
                  exp (api/defexp sym load)]
              (println (str "res: " exp))
              (add-node sym exp)))))))


(defn create-sink
  ""
  []
  (fn [x]
    (println "create sink: " x)
    (let [pipe-name (:name x)
          sym (str pipe-name "-" (rand-int 1000000000))
          exp (api/defexp (symbol sym) (api/fn-call (api/symbol (symbol (str "pipes/" pipe-name))) nil))
          ast (single! exp)]
      (add-node sym ast)
      :okay)))

(defn connect
  ""
  []
  (fn [{:keys [:source :sink] :as x}]
    (println "connect: " x)
    (let [connector (symbol (str "c/" source "-" sink))
          fn (api/defexp connector (api/fn-call (api/symbol '|>) [(api/fn-call (api/symbol 'id) [])]))
          fn-ast (single! fn)
          pipe (api/pipe [(api/symbol source) (api/symbol connector) (api/symbol sink)])]
      (add-node connector fn-ast)
      (add-pipe pipe)
      ;; [fn-ast pipe]
      :okay)))

(defn load-node
  ""
  []
  (let [oasis (db/load-ast @db-conn 'oasis)]
    (println (str "oasis: " oasis))
    ;; (add-node 'oasis )
    )
  )


(defn init
  [rt-db]
  (reset! db-conn rt-db))

(def symbols
  {'create-sink create-sink
   'load-node load-node
   'connect connect
   'add-cell add-cell})
