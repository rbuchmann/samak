(ns samak.caravan
  #?@
  (:clj
   [(:require [clojure.string        :as s]
              [clojure.walk          :as w]
              [clojure.core.async    :as a :refer [<! >! put! chan go go-loop close!]]
              [samak.test-programs   :as test-programs]
              [samak.api             :as api]
              [samak.helpers         :as helpers]
              [samak.lisparser       :as p]
              [samak.scheduler       :as sched]
              [samak.runtime         :as rt]
              [samak.runtime.servers :as servers]
              [samak.pipes           :as pipes]
              [samak.modules         :as modules]
              [samak.builtins        :as builtins]
              [samak.stdlib          :as std]
              [samak.tools           :as tools]
              [samak.trace           :as trace]
              [samak.nodes           :as nodes]
              [samak.runtime.stores  :as stores]
              [samak.runtime.servers :as servers]
              [clojure.string        :as str])]
   :cljs
   [(:require [clojure.string        :as s]
              [clojure.walk          :as w]
              [clojure.core.async    :as a :refer [<! >! put! chan close!]]
              [samak.test-programs   :as test-programs]
              [samak.api             :as api]
              [samak.helpers         :as helpers]
              [samak.lisparser       :as p]
              [samak.scheduler       :as sched]
              [samak.runtime         :as rt]
              [samak.runtime.servers :as servers]
              [samak.pipes           :as pipes]
              [samak.builtins        :as builtins]
              [samak.modules         :as modules]
              [samak.stdlib          :as std]
              [samak.trace           :as trace]
              [samak.nodes           :as nodes]
              [samak.tools           :as tools])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(def rt-conn (atom {:state :uninited}))
(def rt-preview (atom {}))
(def fns (atom {}))
(def net (atom {}))

(declare symbols)

(defmulti handle-node :samak.nodes/type)

(defmethod handle-node
  :samak.nodes/fn-call
  [node]
  ;; (println (str "func: " node))
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
  :samak.nodes/builtin
  [node]
  {:type :caravan/func
   :display "lib"
   :value (str (get-in node [:samak.nodes/value]))})

(defmethod handle-node
  :samak.nodes/fn-ref
  [node]
  (handle-node (get-in node [:samak.nodes/fn :samak.nodes/rhs]))
  ;; {:type (str "fn-ref: " (get-in node [:samak.nodes/fn :samak.nodes/rhs]))}
  )

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
  ([c src]
   (notify-source c src nil))
  ([c src cb]
  (doall (map (fn [[key val]] (if cb
                                (put! c (pipes/make-paket {(str key) val} ::notify) cb)
                                (put! c (pipes/make-paket {(str key) val} ::notify))))
              src))))


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
  (reset! rt (rt/make-runtime (merge builtins/samak-symbols
                                             symbols
                                             std/pipe-symbols)))
  (reset! rt (reduce rt/eval-expression! @rt (vals @net)))
  (let [p (rt/get-definition-by-name @rt (symbol "start"))
        r (when p (pipes/fire! p 1 ::init))]
    (println (str "fire2! " p " - " r))))

(defn format-node
  ""
  [sym fn]
  (let [type (if (is-sink? fn) :caravan/sink :caravan/func)
        ast (make-cell-list fn)]
    (if (empty? ast)
      (println (str "ERROR: no ast for: " sym " - " fn))
      {sym {:caravan/type type
            :caravan/id sym
            :caravan/name (:samak.nodes/name fn)
            :caravan/ast ast}})))

(defn add-node
  ""
  [sym fn]
  ;; (println (str "function cache: " (keys @fns)))
  (swap! fns assoc sym fn)
  (swap! rt-conn #(update % :server rt/eval-all [fn]))
  ;; (swap! rt-preview rt/link-storage (:store @rt-conn))
  ;; (reset-rt rt-preview)
  (format-node sym fn))

(defn name-of-node
  ""
  [node]
  (let [fun (or (get-in node [:samak.nodes/fn])
                (get-in node [:samak.nodes/fn-expression :samak.nodes/fn]))]
    (or (if (= (:samak.nodes/type node) :samak.nodes/def) (:samak.nodes/name node))
        (if (= (:samak.nodes/type fun) :samak.nodes/def) (:samak.nodes/name fun))
        (when-let [named (second fun)] (str named))
        (str "anon-" (rand-int 100000)))
    ))

(defn make-pipe-key
  [source func sink]
  (str source "-" func "-" sink))

(defn format-pipe
  ""
  [pipe]
  (println (str "pipe: " pipe))
  (let [source (name-of-node (:samak.nodes/from pipe))
        xf (:samak.nodes/xf pipe)
        func (if xf (name-of-node xf) nil)
        sink (name-of-node (:samak.nodes/to pipe))
        pipe-name (str source "-" func "-" sink)
        id (:db/id pipe)]
    {id {:caravan/type :caravan/pipe
         :caravan/id id
         :caravan/name pipe-name
         :caravan/source (:db/id (:samak.nodes/fn (:samak.nodes/from pipe)))
         :caravan/source-name source
         :caravan/func (:db/id (:samak.nodes/fn xf)) ;;This might be wrong?
         :caravan/func-name func
         :caravan/sink (:db/id (:samak.nodes/fn (:samak.nodes/to pipe)))
         :caravan/sink-name sink
         }}))


(defn add-pipe
  ""
  [pipe]
  (let [source (name-of-node (:samak.nodes/from pipe))
        func (name-of-node (:samak.nodes/xf pipe))
        sink (name-of-node (:samak.nodes/to pipe))
        pipe-name (str source "-" func "-" sink)]
    (println (str "adding pipe from " source " with " func " to " sink))
    (when (and source func sink)
      ;; (swap! net assoc key pipe)
      ;; (swap! rt-preview rt/link-storage (:store @rt-conn))
      (swap! rt-conn rt/eval-expression! pipe)
      ;; (reset-rt rt-preview)
      (format-pipe pipe))))


(defn load-ast
  "loads an ast given by its entity id from the database"
  [rt id]
  (rt/load-by-id rt id)
  ;; (w/postwalk (fn [form]
  ;;                 (if-let [sub-id (when (and (map? form) (= (keys form) [:db/id]))
  ;;                                   (:db/id form))]
  ;;                   (rt/load-by-id rt sub-id)
  ;;                   form))
  ;;             (rt/load-by-id rt id))
  )


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
   (throw (str "broken: " exp))
  ;; (if (api/is-pipe? exp)
  ;;   (notify-source (add-pipe exp))
  ;;   (let [loaded (single! exp)]
  ;;     (notify-source (add-node (symbol (str (:samak.nodes/name exp))) loaded))))
  )


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
  [ev {:keys [sym cell type] :as x}]
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
          (notify-source ev (add-node (symbol sym) exp))
          :done)))))

(defn value-from-type
  ""
  [cell value]
  (println (str "type: " cell " - " value))
  (case (:samak.nodes/type cell)
    :samak.nodes/fn-call (assoc cell :samak.nodes/fn-expression (api/symbol (symbol value)))
    :samak.nodes/keyword (assoc cell :samak.nodes/value (keyword value))
    (assoc cell :samak.nodes/value value)))


(defn edit-cell
  ""
  [ev {:keys [sym cell value] :as x}]
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
          (notify-source ev (add-node (symbol sym) exp)))))))


(defn change-order
  "switch the :order value inside the children at the two given indexes"
  [v from to]
  (let [fi (update-in v [from :order] (constantly to))
        scd (update-in fi [to :order] (constantly from))]
    scd))


(defn swap-cell
  ""
  [ev {:keys [:sym :cell-idx :target] :as x}]
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
          (notify-source ev (add-node (symbol sym) exp)))
        ))))

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
  [ev {:keys [sym cell-idx] :as x}]
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
          (notify-source ev (add-node (symbol sym) exp))
          :done)))))

(defn indent-cell
  ""
  [ev {:keys [sym cell-idx] :as x}]
  (println (str "indent: " x))
  (let [src (get @fns (symbol sym))
        idx (dec cell-idx)]
    (when (and sym src idx type)
      (let [[cell par par-idx] (add-cell-internal src idx)
            root-id (:db/id src)
            own-arg (some #(when (= (:db/id (:samak.nodes/node %)) (:db/id cell)) %) (get par (get-child-key par)))
            own-order (:order own-arg)
            ;; _ (println (str "own-order: " own-order))
            target-node (some #(when (= (:order %) (dec own-order)) %) (get par (get-child-key par)))
            ;; _ (println (str "target-node: " target-node))
            target (:samak.nodes/node target-node)
            ;; _ (println (str "target: " target))
            ]
        (when (and target (get-child-key target))
          (let [insertion {:db/id (:db/id target) (get-child-key target) [{:db/id -1 :order (count (get target (get-child-key target))) :samak.nodes/node cell}]}
                ;; _ (println (str "insertion: " insertion))
                later-sibs (filterv #(> (:order %) own-order) (get par (get-child-key par)))
                ;; _ (println (str "later-sibs: " later-sibs))
                fixup {:db/id (:db/id par) (get-child-key par) (map #(update % :order dec) later-sibs)}
                ;; _ (println (str "fixup: " fixup))
                retract [:db/retract (:db/id par) (get-child-key par) (:db/id own-arg)]
                ;; _ (println (str "retract: " retract))
                ]
            (let [write (persist! @rt-conn [insertion fixup retract])
                  exp (load-ast @rt-conn root-id)]
              (println (str "res: " exp))
              (notify-source ev (add-node (symbol sym) exp))
              :done)))))))

(defn create-sink
  ""
  [cmd ev x]
  (println "create sink: " x)
  (let [pipe-name (:name x)
        sym (str pipe-name "-" (rand-int 1000000000))
        exp (api/defexp (symbol sym) (api/fn-call (api/symbol (symbol (str "pipes/" pipe-name))) nil))
        ast (single! exp)]
    (println (str "res: " ast))
    (notify-source ev (add-node (:db/id ast) ast)) ;; FIXME
    (println (str "res: " ast))
    ::okay))

(defn disconnect
  ""
  []
  (println "disconnect"))

(defn connect
  ""
  [ev source connector sink]
  ;; (println (str "connect " source " with " connector " to " sink))
  (let [fn (api/defexp (symbol connector) (api/symbol '_))
        fn-ast (single! fn)
        pipe (api/pipe (api/symbol (symbol source))
                       (api/symbol (symbol connector))
                       (api/symbol (symbol sink)))]
    (notify-source ev (add-node (symbol connector) fn-ast))
    (notify-source ev (add-pipe pipe))))


(defn link
  ""
  [ev cmd {:keys [:source :sink] :as x}]
  (println "connect: " x)
  (let [connector (str "c-" source "-" sink)
        pipe-key (make-pipe-key source connector sink)
        existing (contains? @net pipe-key)]
    (when (and sink source (not= sink source) )
      (if existing
        (disconnect)
        (connect ev source connector sink)
        ))))

(defn link-pipes
  ""
  [from to xf]
  (rt/link-fn from
              to
              (when xf (pipes/transduction-pipe (pipes/instrument :caravan/adhoc nil xf)))))


(defn find-tests
  ""
  [conf]
  (println "conf " conf)
  (when conf
    (let [is-module (= (:samak.nodes/type conf) :samak.nodes/module)
          sym (if is-module (symbol (str "test-" (:samak.nodes/name conf))) (symbol (:samak.nodes/name conf)))
          conf (if is-module
                 (api/defexp sym (api/fn-call conf []))
                 conf)
          rt2 (update @rt-conn :server #(rt/eval-all % [conf]))
          evaled (rt/get-definition-by-name rt2 sym)]
      (println "evaled " evaled)
      ;; (println "evaled " (:samak.nodes/definition evaled))
      (:tests evaled))))

(defn attach-assert
  ""
  [verify source-pipe xf-fn test-fn]
  (let [assert-name (str "assert-" (rand-int 1000000))
        assert-exp (api/defexp (symbol assert-name) (api/fn-call (api/symbol 'pipes/debug) []))
        assert-ast (single! assert-exp)]
    (add-node (symbol assert-name) assert-ast)
    (let [assert-pipe (rt/get-definition-by-name @rt-conn (symbol assert-name))
          verify-pipe (rt/get-definition-by-name @rt-conn (symbol verify))]
      (link-pipes source-pipe assert-pipe xf-fn)
      (link-pipes assert-pipe verify-pipe test-fn))))

(defn resolve-sym-or-fn
  ""
  [exp]
  (if (= (:samak.nodes/type exp) :samak.nodes/fn-ref)
    (rt/get-definition-by-name @rt-conn (get-in exp [:samak.nodes/fn :samak.nodes/name]))
    (do
      (println exp)
      (let [ast (load-ast @rt-conn (:db/id exp))]
        (println "loaded" ast)
        (swap! rt-conn #(update % :server rt/eval-all [ast])))
      (println "evaled" exp)
      (let [r (rt/resolve-fn @rt-conn (:db/id exp))]
        (println "resolved " r)
        r)
      )))


(defn add-pipe-net
  ""
  [verify config ast]
  (let [source (resolve-sym-or-fn (:samak.nodes/from ast))
        source-name (get-in ast [:samak.nodes/from :samak.nodes/fn :samak.nodes/name])
        xf (get-in ast [:samak.nodes/xf :samak.nodes/fn])
        sink (resolve-sym-or-fn (:samak.nodes/to ast))
        sink-name (get-in ast [:samak.nodes/to :samak.nodes/fn :samak.nodes/name])
        test-ref (get config (str sink-name))]
    (println "  V" "Adding pipe:" source-name
             "with" (if xf
                       (str "[" (:db/id xf) "] " (:samak.nodes/name xf))
                       "~none~")
             "to" sink-name)
    (let [xf-pipe (get (servers/get-defined (:server @rt-conn)) (:db/id xf))]
      (when test-ref
        (println "  V" "Verifying pipe:" sink-name "with" test-ref)
        (attach-assert verify source xf-pipe (first test-ref)))
      (link-pipes source sink xf-pipe))))


(defn setup-verify
  ""
  []
  (println "  V" "Set up result collection")
  (let [verify-name (symbol (str "verify-" (rand-int 1000000)))
        verify-exp (api/defexp verify-name (api/fn-call (api/symbol 'pipes/debug) []))
        verify-ast (single! verify-exp)]
    (add-node verify-name verify-ast)
    verify-name))


(defn handle-source
  ""
  [source eval?]
  (let [nodes (:nodes source)
        _ (println "  V" "Loading asts: " (s/join ", " nodes))
        asts (map #(load-ast @rt-conn %1) nodes)
        _ (println "  V" "Adding nodes: " (s/join ", " (map #(str (:samak.nodes/name %) "(" (:db/id %) ")") asts)))
        adder (if eval? add-node format-node)
        node-notify (doall (map #(adder (:db/id %) %) asts))
        pipes (:pipes source)
        _ (println "  V" "Adding pipes: " (s/join ", " pipes))
        pipe-asts (doall (map #(load-ast @rt-conn %1) pipes))]
    [node-notify pipe-asts]))

(defn handle-mod
  ""
  [module]
  (println "mod" module)
  (let [id (:id module)
        root (:roots module)]
    {id {:caravan/type :caravan/module
         :caravan/name (str id)
         :caravan/ports (into [] (concat (:sources module) (:sinks module)))
         :caravan/nodes (into [] (:nodes root))
         :caravan/pipes (into [] (:pipes root))}}))


(defn runtime-net
  ""
  [net config verify]
  ;; (println "Loading source: " (str sym))
  (let [[_ pipe-asts] (handle-source net true)]
    (doall (map #(add-pipe-net verify (:then config) %) pipe-asts))))


(defn database-net
  ""
  [net]
  (let [[node-notify pipe-asts] (handle-source net false)
        pipe-notify (map format-pipe pipe-asts)]

    {:nodes node-notify :pipes pipe-notify}))

(defn handle-deps
  ""
  [deps]
  (reduce (fn [acc x]
            (let [roots (database-net (:roots x))]
              {:nodes (into [] (concat (:nodes acc) (:nodes roots)))
               :pipes (into [] (concat (:pipes acc) (:pipes roots)))
               :modules (conj (:modules acc) (handle-mod x))}))
          {:nodes []
           :pipes []
           :modules []}
          deps))

(defn eval-bundle
  ""
  [sym]
  (let [bundle (sched/load-bundle @rt-conn sym)
        _ (println "ev b" bundle)
        roots (:roots bundle)
        deps (handle-deps (:deps bundle))
        _ (println "deps" deps)
        rootnotify (assoc (database-net roots) :modules [(handle-mod bundle)])
        ;; rootnotify  (database-net roots)
        _ (println "root" rootnotify)
        a1 (merge-with into rootnotify deps)
        _ (println "ev n" a1)]
    (assoc a1 :id (:id bundle))
    ))

(defn test-bundle
  ""
  [sym test]
  (let [verify (setup-verify)
        bundle (:roots (sched/load-bundle @rt-conn sym))]
    (runtime-net bundle test verify)
    verify))

(defn trace-dump
  ""
  []
  (trace/init-tracer @rt-conn {})
  (trace/dump))


(defn persist-net
  ""
  [code]
  (let [parsed (p/parse-all (s/join " " code))
        _ (rt/persist-to-ids! (:store @rt-conn) (:value parsed))
         ]
    :done))


(defn run-event
  ""
  [pipe pipe-name content]
  (let [source-name (str "test/" pipe-name)
        paket (pipes/make-paket content source-name)]
    (println (str "f! " pipe-name " -> " content))
    (trace/trace source-name 0 paket)
    (pipes/fire-raw! pipe paket)
    ))


(defn run-test
  ""
  [c sym [name tst]]
  (println (str "test " name " - " tst))

  (let [verify (test-bundle sym tst)]
    (go (let [pipe (rt/get-definition-by-name @rt-conn verify)
              listener (chan 1)]
          (a/tap (.-out pipe) listener) ;; TODO: FIX protocol?
          (loop [results []]
            (let [msg (<! listener)
                  results (conj results msg)
                  runs (count (flatten (vals (:then tst))))]
              (println (str (count results) "/" runs " - " msg))
              (if (= (count results) runs)
                (>! c (or (some #(when (not= :success (:samak.pipes/content %)) %) results) :success))
                (recur results))))))
    (doall (map (fn [[pipe-name values]]
                  (println (str "pipe " (str pipe-name) " values: " values))
                  (let [pipe (rt/get-definition-by-name @rt-conn (symbol pipe-name))]
                    (if (nil? pipe)
                      (println "no such pipe: " pipe-name)
                      (doall (map #(run-event pipe pipe-name %) values)))))
                (:when tst)))))


(defn run-testsuite
  ""
  ([c sym] (run-testsuite c sym {}))
  ([c sym {timeout :timeout :or {timeout 3000}}]
   (let [net (rt/load-by-sym @rt-conn sym)
         _ (println "Preloading network")
         _ (test-bundle sym :noop)
         _ (println "Loading test definitions")
         tests (find-tests net)
         test-num (count tests)
         test-results-chan (chan 1)]
     (if (zero? test-num)
       (a/put! c :no-tests)
       (go-loop [results []
                 tests tests]
         (run-test test-results-chan sym (first tests))
         (let [[raw port] (a/alts! [test-results-chan (a/timeout timeout)])
               msg (if (= port test-results-chan) raw :timeout)
               results (conj results msg)]
           (println (str "#" (count results) "/" test-num " - " msg))
           (if (>= (count results) test-num)
             (>! c (or (some #(when (not= :success %) %) results) :success))
             (recur results (rest tests)))))))))

(defn load-lib
  ""
  [cmd ev sym]
  (let [bundle (eval-bundle sym)]
    ;; (println (str "count: " cnt))
    (doall (map #(notify-source ev %) (:modules bundle)))
    (doall (map #(notify-source ev %) (:nodes bundle)))
    (doall (map #(notify-source ev %) (:pipes bundle)))
    (notify-source
     ev
     {::state ::done}
     #(a/put! cmd (pipes/make-paket {::event ::load ::status ::done ::percent 100 ::id (:id bundle)} ::caravan)))))

(defn load-net
  ""
  [cmd ev prog sym]
  (persist-net prog)
  (helpers/debounce #(load-lib cmd ev sym)))

(defn test-net
  ""
  [c prog sym]
  (persist-net prog)
  (run-testsuite c sym {:timeout 3000}))

(defn load-chuck
  ""
  [cmd ev]
  (load-net cmd ev test-programs/chuck 'chuck)
  )

(defn load-test
  ""
  [cmd ev]
  (load-net cmd ev test-programs/test-nested-modules-test 'baz)
  )

(defn test-chuck
  ""
  [c]
  (test-net c test-programs/chuck 'chuck))

(defn load-oasis
  ""
  [cmd ev]
  (helpers/debounce #(load-lib cmd ev 'oasis)))

(defn test-oasis
  ""
  [c]
  (run-testsuite c 'oasis {:timeout 3000}))

(defn test-example
  ""
  [cmd ev arg]
  (case arg
    :load (load-test cmd ev)
    :self (load-oasis cmd ev)
    (tools/log "load unknown: " arg)))


(defn oasis-hook
  ""
  []
  (let [c (chan 1)]
    (test-oasis c)
    (go
      (let [[raw port] (a/alts! [c (a/timeout 30000)])
            val (if (= port c) raw :timeout-overall)]
        (println (str "\nresult: " val))
        (println (str "\ntraces: "))
        (trace-dump)))))


(defn init
  [rt]
  (reset! rt-conn rt)
  ;; (reset! rt-preview (rt/make-runtime (merge builtins/samak-symbols
  ;;                                            symbols
  ;;                                            std/pipe-symbols)))
  )


(defn pong
  ""
  [caravan-out x]
  (tools/log "ponging: " x caravan-out)
  (a/put! caravan-out (pipes/make-paket {::event ::pong} ::caravan)))


(defn caravan-module
  ""
  []
  (println "def caravan")
  (fn []
    (println "init caravan")
    (let [caravan-in (chan)
          caravan-cmd (chan)
          caravan-eval (chan)]
      (go-loop []
        (when-let [x (<! caravan-in)]
          (tools/log "caravan-in: " x)
          (when (:ping (:samak.pipes/content x))
            (pong caravan-cmd x))
          (when-let [call (:call (:samak.pipes/content x))]
            (do
              (tools/log "caravan-call: " call)
              (case (:action call)
                :load (test-example caravan-cmd caravan-eval (:arguments call))
                :test (test-chuck caravan-cmd)
                :trace (trace-dump)
                :create-sink (create-sink caravan-cmd caravan-eval (:arguments call))
                :link (link caravan-cmd caravan-eval (:arguments call))
                :insert (add-cell caravan-eval (:arguments call))
                :edit (edit-cell caravan-eval (:arguments call))
                :cut (cut-cell caravan-eval (:arguments call))
                :swap (swap-cell caravan-eval (:arguments call))
                :indent (indent-cell caravan-eval (:arguments call))
                (tools/log "actions unknown: " call))))
          (recur)))
      (let [foo {:sources {:commands (pipes/source caravan-cmd)
                           :eval (pipes/source caravan-eval)}
                 :sinks {:actions (pipes/sink caravan-in)}}]
        foo))))


(def symbols
  (merge
   {'modules/caravan caravan-module}))
