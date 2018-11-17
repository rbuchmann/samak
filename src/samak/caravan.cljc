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

(defn get-child-key
  ""
  [node]
  (cond
    (api/is-vector? node) :samak.nodes/children
    (api/is-map? node)  :samak.nodes/mapkv-pairs
    (api/is-entry? node) :samak.nodes/mapvalue
    (api/is-fn-call? node) :samak.nodes/arguments))

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

(defn repl-eval
  [exp]
  (if (api/is-pipe? exp)
    (add-pipe exp)
    (let [loaded (single! exp)]
      (add-node (str (:samak.nodes/name exp)) loaded))))


;; TODO: this is broken if other nodes have children
(defn find-cell
  [src cell counter parent parent-idx]
  ;; (println (str "find: " src ",  " cell ",  " counter ",  " parent))
  (if (= counter cell)
    [src parent parent-idx]
    (first (filter some? (map-indexed ;; needs to be replaced with reduce to persist count
                          (fn [i child] (find-cell child cell (+ counter i 1) src counter))
                          (get-children src))))))

(defn add-cell-internal
  ""
  [src cell]
  (let [root (:samak.nodes/rhs src)]
    (find-cell root cell 0 nil 0)))

(defn content-from-type
  ""
  [x]
  (case x
    :string (api/string "")
    :integer (api/integer 0)
    :float (api/float 0.0)
    :symbol (api/symbol 'id)
    :keyword (api/keyword :div)
    :table (api/map {})
    :list (api/vector [])
    :accessor (api/key-fn :test)
    :function (api/fn-call (api/symbol 'id) [])))


(defn is-listy-node
  ""
  [cell]
  (contains? #{:samak.nodes/vector :samak.nodes/map :samak.nodes/fn-call}
             (:samak.nodes/type cell)))

(defn is-map-node
  ""
  [cell]
  (= (:samak.nodes/type cell) :samak.nodes/map))

(defn is-mapish
  ""
  [cell]
  (or (is-map-node cell)
      (contains? cell :samak.nodes/mapkey)))



(defn add-map
  ""
  [target key content]
  (let [wrap (api/map-entry [(api/integer key) content])]
    (update target :samak.nodes/mapkv-pairs #(into %2 %1) [wrap]))
)


(defn add-list
  ""
  [target content]
  (let [target-key (get-child-key target)
        target-args (get target target-key)
        updated (update target target-key conj {:db/id -1 :order (count target-args) :samak.nodes/node content})]
    updated))


(defn add-cell
  ""
  []
  (fn [{:keys [sym cell type] :as x}]
    (println (str "adding: " x))
    (let [src (get @fns sym)
          idx (dec cell)]
      (when (and sym src idx type)
        (let [[cell par par-idx] (add-cell-internal src idx)
              _ (println (str "cell: " cell))
              root-id (:db/id src)
              content (content-from-type type)
              updated (if (is-mapish cell)
                        (add-map (if (is-map-node cell) cell par) (- idx 1 par-idx) content)
                        (add-list (if (is-listy-node cell) cell par) content))]
            (let [write (persist! @db-conn [updated])
                  exp (db/load-by-id @db-conn root-id)]
              (println (str "res: " exp))
              (add-node sym exp)
              :done))))))

(defn value-from-type
  ""
  [cell value]
  (println (str "type: " cell " - " value))
  (case (:samak.nodes/type cell)
    :samak.nodes/fn-call (assoc cell :samak.nodes/fn (api/symbol (symbol value)))
    (assoc cell :samak.nodes/value value)))


(defn edit-cell
  ""
  []
  (fn [{:keys [sym cell value] :as x}]
    (println (str "editing: " x))
    (let [src (get @fns sym)
          idx (dec cell)]
      (when (and sym src idx value)
          (let [[cell par] (add-cell-internal src idx)
                root-id (:db/id src)
                updated (value-from-type cell value)]
            (let [write (persist! @db-conn [updated])
                  exp (db/load-by-id @db-conn root-id)]
              (println (str "res: " exp))
              (add-node sym exp)))))))


(defn change-order
  "switch the :order value inside the children at the two given indexes"
  [v from to]
  (let [fi (update-in v [from :order] (constantly to))
        scd (update-in fi [to :order] (constantly from))]
    scd))


(defn swap-cell
  ""
  []
  (fn [{:keys [:sym :cell-idx :target] :as x}]
    (println (str "swap: " x))
    (let [src (get @fns sym)
          idx (dec cell-idx)]
      (when (and sym src idx target)
          (let [[cell par par-idx] (add-cell-internal src idx)
                root-id (:db/id src)
                arg-source-idx (- idx 1 par-idx)
                arg-target-idx (- target 2 par-idx)
                sorted-args (vec (sort-by :order (get par (get-child-key par)))) ;; need to make a copy because sort-by is inplace sometimes
                changed (change-order sorted-args arg-source-idx arg-target-idx)
                node (assoc par (get-child-key par) changed)]
            (let [write (persist! @db-conn [node])
                  exp (db/load-by-id @db-conn root-id)]
              (println (str "res: " exp))
              (add-node sym exp))
            )))))

(defn remove-arg
  ""
  [v idx]
  (vec (for [elem v
             :when (not (= (:order elem) idx))]
         (if (> (:order elem) idx)
           (update elem :order dec)
           elem))))


(defn cut-cell
  ""
  []
  (fn [{:keys [sym cell-idx] :as x}]
    (println (str "cut: " x))
    (let [src (get @fns sym)
          idx (dec cell-idx)]
      (when (and sym src idx)
          (let [[cell par par-idx] (add-cell-internal src idx)
                root-id (:db/id src)
                arg-idx (- idx 1 par-idx)
                removed-args (remove-arg (get par (get-child-key par)) arg-idx)
                updated (assoc par (get-child-key par) removed-args)
                target-node (some #(when (= (:order %) arg-idx) %) (get par (get-child-key par)))
                retract [:db/retract (:db/id par) (get-child-key par) (:db/id target-node)]]
            (let [write (persist! @db-conn [updated retract])
                  exp (db/load-by-id @db-conn root-id)]
              (println (str "res: " exp))
              (add-node sym exp)
              :done))))))

(defn indent-cell
  ""
  []
  (fn [{:keys [sym cell-idx] :as x}]
    (println (str "indent: " x))
    (let [src (get @fns sym)
          idx (dec cell-idx)]
      (when (and sym src idx type)
          (let [[cell par par-idx] (add-cell-internal src idx)
                root-id (:db/id src)
                arg-idx (- idx 1 par-idx)
                removed-args (remove-arg (get par (get-child-key par)) arg-idx)
                _ (println (str "upd: " removed-args))
                removed (assoc par (get-child-key par) removed-args)
                target-node (some #(when (= (:order %) (dec arg-idx)) %) (get par (get-child-key par)))
                target (:samak.nodes/node target-node)
                _ (println (str "target: " target))
                inserted (update target (get-child-key target) conj {:db/id -1 :order (count (get target (get-child-key target))) :samak.nodes/node cell})
                _ (println (str "inserted: " inserted))
                updated (update-in par [(get-child-key par)] #(assoc % (dec arg-idx) inserted))
                _ (println (str "updated: " updated))
                retract-node (some #(when (= (:order %) arg-idx) %) (get par (get-child-key par)))
                retract [:db/retract (:db/id par) (get-child-key par) (:db/id retract-node)]
                _ (println (str "retract: " retract))
                ]
            (let [write (persist! @db-conn [updated retract])
                  exp (db/load-by-id @db-conn root-id)]
              (println (str "res: " exp))
              (add-node sym exp)
              :done))))))

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
   'swap-cell swap-cell
   'cut-cell cut-cell
   'indent-cell indent-cell
   'edit-cell edit-cell})
