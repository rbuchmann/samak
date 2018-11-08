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
  {:type :caravan/func
   :display "func"
   :value (str (:samak.nodes/name (:samak.nodes/fn node)))})

(defmethod handle-node
  :samak.nodes/integer
  [node]
  {:type :caravan/int
   :display "int"
   :value (str (:samak.nodes/value node))})

(defmethod handle-node
  :samak.nodes/string
  [node]
  {:type :caravan/str
   :display "str"
   :value (str (:samak.nodes/value node))})

(defmethod handle-node
  :samak.nodes/keyword
  [node]
  {:type :caravan/kw
   :display "kw"
   :value (str (:samak.nodes/value node))})

(defn make-sym
  [value]
  {:type :caravan/sym
   :display "sym"
   :value value})

(defmethod handle-node
  :samak.nodes/symbol
  [node]
  (make-sym (str (:samak.nodes/value node))))

(defmethod handle-node
  :samak.nodes/float
  [node]
  {:type :caravan/float
   :display "float"
   :value (str (:samak.nodes/value node))})

(defmethod handle-node
  :samak.nodes/map
  [node]
  {:type :caravan/table
   :display "table"
   :value "###"})

(defmethod handle-node
  :samak.nodes/vector
  [node]
  {:type :caravan/list
   :display "list"
   :value "---"})

(defmethod handle-node
  :samak.nodes/key-fn
  [node]
  {:type :caravan/acc
   :display "acc"
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
    (api/is-vector? node) (map :samak.nodes/node (sort-by :order (get node :samak.nodes/children)))
    (api/is-map? node) (get node :samak.nodes/mapkv-pairs)
    (api/is-entry? node) [(get node :samak.nodes/mapvalue)]
    (api/is-fn-call? node) (map :samak.nodes/node (sort-by :order (get node :samak.nodes/arguments)))))


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
  (println (str "function cache: " @fns))
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
    (let [sym (:name x)
          src (get @fns sym)
          idx (dec (:cell x))]
      (when (and sym src idx)
          (let [[cell par] (add-cell-internal src idx)
                root-id (:db/id src)
                content (api/string "bar")
                updated (update cell :samak.nodes/arguments conj {:db/id -1 :order 1 :samak.nodes/node content})]
            (println (str "cell: " cell " - " par))
            (println (str "updated: " updated))
            (let [write (persist! @db-conn [updated])
                  exp (db/load-by-id @db-conn root-id)]
              (println (str "res: " exp))
              (add-node sym exp)))))))

(defn edit-cell
  ""
  []
  (fn [x]
    (println (str "editing: " x))
    (let [sym (:name x)
          src (get @fns sym)
          idx (dec (:cell x))]
      (println (str "src: " src))
      (when (and sym src idx)
          (let [[cell par] (add-cell-internal src idx)
                root-id (:db/id src)
                updated (assoc cell :samak.nodes/value (:value x))]
            (println (str "cell: " cell " - " par))
            (println (str "updated: " updated))
            (let [write (persist! @db-conn [updated])
                  exp (db/load-by-id @db-conn root-id)]
              (println (str "res: " exp))
              (add-node sym exp)))))))


(defn fall-cell
  ""
  []
  (fn [x]
    (println (str "fall: " x))
    (let [sym (:name x)
          src (get @fns sym)
          idx (dec (:cell x))]
      (println (str "src: " src))
      (when (and sym src idx)
          (let [[cell par] (add-cell-internal src idx)
                root-id (:db/id src)
                sorted (update par :samak.nodes/arguments #(sort-by :order %))
                fall1 (update-in sorted [:samak.nodes/arguments 0] #(assoc % :order 1))
                fall2 (update-in fall1 [:samak.nodes/arguments 1] #(assoc % :order 0))
                ]
            (println (str "cell: " cell " - " par))
            (println (str "updated: " fall2))
            (let [write (persist! @db-conn [fall2])
                  exp (db/load-by-id @db-conn root-id)]
              (println (str "res: " exp))
              (add-node sym exp))
            )))))

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
    (let [connector  (str "c/" source "-" sink)
          fn (api/defexp (symbol connector) (api/fn-call (api/symbol '|>) [(api/fn-call (api/symbol 'id) [])]))
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
   'add-cell add-cell
   'fall-cell fall-cell
   'edit-cell edit-cell})
