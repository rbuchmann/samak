(ns samak.caravan
  #?@
  (:clj
   [(:require [clojure.string :as s]
              [clojure.walk   :as w]
              [clojure.core.async :as a :refer [<! put! chan go-loop close!]]
              [samak.api      :as api]
              [samak.runtime  :as rt]
              [samak.pipes    :as pipes]
              [samak.builtins :as builtins]
              [samak.stdlib   :as std]
              [samak.tools :as tools])]
   :cljs
   [(:require [clojure.string :as s]
              [clojure.walk   :as w]
              [clojure.core.async :as a :refer [<! put! chan close!]]
              [samak.api      :as api]
              [samak.runtime  :as rt]
              [samak.pipes    :as pipes]
              [samak.builtins :as builtins]
              [samak.stdlib   :as std]
              [samak.tools :as tools])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(def rt-conn (atom {}))
(def fns (atom {}))
(def net (atom []))

(declare symbols)

(def rt-preview (atom {}))

(defmulti handle-node :samak.nodes/type)

(defmethod handle-node
  :samak.nodes/fn-call
  [node]
  (println (str "func: " node))
  {:type :caravan/func
   :display "func"
   :value (str (or (get-in node [:samak.nodes/fn-expression :samak.nodes/fn :samak.nodes/name])
                   (get-in node [:samak.nodes/fn-expression :samak.nodes/fn 1])))})

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
  (let [rhs-fn (get-in exp [:samak.nodes/rhs :samak.nodes/fn-expression :samak.nodes/fn])
        has-name (get rhs-fn :samak.nodes/name)
        fn-name (str (:samak.nodes/name rhs-fn))
        is-stdlib (s/starts-with? fn-name "pipes/")
        is-reductions (= "pipes/reductions" fn-name)]
    (and (api/is-def? exp)
         has-name
         is-stdlib
         (not is-reductions))))

(defn reset-rt
  ""
  [rt]
  (reset! rt (reduce rt/eval-expression! @rt @net))
  (let [p (rt/get-definition-by-name @rt (symbol "start"))
        r (when p (pipes/fire! p "1"))]
    (println (str "fire! " p " - " r))))


(defn add-node
  ""
  [sym fn]
  (swap! fns assoc sym fn)
  (println (str "function cache: " (keys @fns)))
  (swap! rt-preview rt/link-storage (:store @rt-conn))
  (swap! rt-preview rt/eval-expression! fn)
  (reset-rt rt-preview)
  (let [type (if (is-sink? fn) :caravan/sink :caravan/func)
        ast (make-cell-list fn)]
    (if (empty? ast)
      (println (str "ERROR: no ast for: " sym " - " fn))
      (notify-source {:caravan/type type
                      :caravan/name (str sym)
                      :caravan/ast ast}))))

(defn name-of-node
  ""
  [node]
  (str (second (get-in node [:samak.nodes/fn]))))


(defn add-pipe
  ""
  [pipe]
  (println (str "add pipe " pipe))
  (let [source (name-of-node (:samak.nodes/from pipe))
        func (name-of-node (:samak.nodes/xf pipe))
        sink (name-of-node (:samak.nodes/to pipe))]
    (println (str "adding pipe from " source " with " func " to " sink))
    (when (and source func sink)
      (swap! net conj pipe)
      (swap! rt-preview rt/link-storage (:store @rt-conn))
      (swap! rt-preview rt/eval-expression! pipe)
      (reset-rt rt-preview)
      (notify-source {:caravan/type :caravan/pipe
                      :caravan/source source
                      :caravan/func func
                      :caravan/sink sink}))))

(defn load-ast
  "loads an ast given by its entity id from the database"
  [rt id]
  (w/postwalk (fn [form]
                (if-let [sub-id (when (and (map? form) (= (keys form) [:db/id]))
                                  (:db/id form))]
                 (rt/load-by-id rt sub-id)
                 form))
              (rt/load-by-id rt id)))

(defn persist!
  ""
  [rt tx-records]
  (rt/store! (:store rt) tx-records))


(defn single!
  ""
  [exp]
  (let [loaded (persist! @rt-conn [(assoc exp :db/id -1)])
        ast (load-ast @rt-conn (:db/id (first loaded)))]
    ast))

(defn repl-eval
  [exp]
  (if (api/is-pipe? exp)
    (add-pipe exp)
    (let [loaded (single! exp)]
      (add-node (symbol (str (:samak.nodes/name exp))) loaded))))


(defn find-cell-internal
  [src cell counter parent parent-idx]
  ;; (println (str "find: " src ",  " cell ",  " counter ",  " parent "\n\n"))
  (if (>= counter cell)
    (do
      {:i counter :result [src parent parent-idx]})
    (reduce (fn [{i :i result :result} child]
              (if result
                (do
                  {:i i :result result})
                (let [{subcount :i subresult :result}
                      (find-cell-internal child cell (+ counter i 1) src counter)]
                  {:i (+ subcount i 1) :result subresult})))
            {:i 0 :result nil}
            (get-children src))))

(defn find-cell
  ""
  [src cell]
  (:result (find-cell-internal src cell 0 nil 0)))


(defn add-cell-internal
  ""
  [src cell]
  (let [root (:samak.nodes/rhs src)]
    (find-cell root cell)))

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
  [{:keys [sym cell type] :as x}]
    (println (str "adding: " x))
    (let [src (get @fns (symbol sym))
          idx (dec cell)]
      (when (and sym src idx type)
        (let [[cell par par-idx] (add-cell-internal src idx)
              _ (println (str "cell: " cell))
              _ (println (str "par: " par))
              root-id (:db/id src)
              content (content-from-type type)
              updated (if (is-mapish cell)
                        (add-map (if (is-map-node cell) cell par) (- idx 1 par-idx) content)
                        (add-list (if (is-listy-node cell) cell par) content))]
            (let [write (persist! @rt-conn [updated])
                  exp (load-ast @rt-conn root-id)]
              (println (str "res: " exp))
              (add-node (symbol sym) exp)
              :done)))))

(defn value-from-type
  ""
  [cell value]
  (println (str "type: " cell " - " value))
  (case (:samak.nodes/type cell)
    :samak.nodes/fn-call (assoc cell :samak.nodes/fn (api/symbol (symbol value)))
    :samak.nodes/keyword (assoc cell :samak.nodes/value (keyword value))
    (assoc cell :samak.nodes/value value)))


(defn edit-cell
  ""
  []
  (fn [{:keys [sym cell value] :as x}]
    (println (str "editing: " x))
    (let [src (get @fns (symbol sym))
          idx (dec cell)]
      (when (and sym src idx value)
          (let [[cell par] (add-cell-internal src idx)
                root-id (:db/id src)
                updated (value-from-type cell value)]
            (let [write (persist! @rt-conn [updated])
                  exp (load-ast @rt-conn root-id)]
              (println (str "res: " exp))
              (add-node (symbol sym) exp)))))))


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
    (let [src (get @fns (symbol sym))
          idx (dec cell-idx)]
      (when (and sym src idx target)
          (let [[cell par par-idx] (add-cell-internal src idx)
                root-id (:db/id src)
                arg-source-idx (- idx 1 par-idx)
                arg-target-idx (- target 2 par-idx)
                sorted-args (vec (sort-by :order (get par (get-child-key par)))) ;; need to make a copy because sort-by is inplace sometimes
                changed (change-order sorted-args arg-source-idx arg-target-idx)
                node (assoc par (get-child-key par) changed)]
            (let [write (persist! @rt-conn [node])
                  exp (load-ast @rt-conn root-id)]
              (println (str "res: " exp))
              (add-node (symbol sym) exp))
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
    (let [src (get @fns (symbol sym))
          idx (dec cell-idx)]
      (when (and sym src idx)
          (let [[cell par par-idx] (add-cell-internal src idx)
                root-id (:db/id src)
                arg-idx (- idx 1 par-idx)
                removed-args (remove-arg (get par (get-child-key par)) arg-idx)
                updated (assoc par (get-child-key par) removed-args)
                target-node (some #(when (= (:order %) arg-idx) %) (get par (get-child-key par)))
                retract [:db/retract (:db/id par) (get-child-key par) (:db/id target-node)]]
            (let [write (persist! @rt-conn [updated retract])
                  exp (load-ast @rt-conn root-id)]
              (println (str "res: " exp))
              (add-node (symbol sym) exp)
              :done))))))

(defn indent-cell
  ""
  []
  (fn [{:keys [sym cell-idx] :as x}]
    (println (str "indent: " x))
    (let [src (get @fns (symbol sym))
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
            (let [write (persist! @rt-conn [updated retract])
                  exp (load-ast @rt-conn root-id)]
              (println (str "res: " exp))
              (add-node (symbol sym) exp)
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
      (println (str "res: " ast))
      (add-node (symbol sym) ast)
      :okay)))

(defn connect
  ""
  []
  (fn [{:keys [:source :sink] :as x}]
    (println "connect: " x)
    (when (and sink source (not= sink source))
        (let [connector  (str "c/" source "-" sink)
              fn (api/defexp (symbol connector) (api/fn-call (api/symbol '|>) [(api/fn-call (api/symbol 'id) [])]))
              ;; fn (api/defexp (symbol connector) (api/fn-call (api/symbol '|>) [(api/vector [(api/keyword :div) (api/string "Hello world")])]))
              fn-ast (single! fn)
              pipe (api/pipe (api/symbol (symbol source))
                             (api/symbol (symbol connector))
                             (api/symbol (symbol sink)))]
          (add-node (symbol connector) fn-ast)
          (add-pipe pipe)
          ;; [fn-ast pipe]
          :okay))))

;; (defn load-node
;;   ""
;;   []
;;   (let [oasis (rt/load-ast @rt-conn 'oasis)]
;;     (println (str "oasis: " oasis))
;;     ;; (add-node 'oasis )
;;     )
;;   )


(defn init
  [rt]
  (reset! rt-conn rt)
  (reset! rt-preview (rt/make-runtime (merge builtins/samak-symbols
                                             symbols
                                             std/pipe-symbols))))

(defn caravan-pipe
  ""
  []
  (let [caravan-chan (chan)]
    (go-loop []
      (when-let [x (<! caravan-chan)]
        (when-let [call (:call x)]
          (do
            (tools/log "foo" call)
            (case (:action call)
              :insert (add-cell (:arguments call))
              (tools/log "actions unknown: " call))))
        (recur)))
    (pipes/sink caravan-chan)))


(def symbols
  {'create-sink create-sink
   ;; 'load-node load-node
   'pipes/caravan caravan-pipe
   'connect connect
   'add-cell add-cell
   'swap-cell swap-cell
   'cut-cell cut-cell
   'indent-cell indent-cell
   'edit-cell edit-cell})
