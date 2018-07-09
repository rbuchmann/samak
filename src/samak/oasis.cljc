(ns samak.oasis
  (:require [samak.api    :as api]
            [samak.core   :as core]
            [samak.stdlib :as pipes]
            [samak.spec] ;;include specs as side effect
            #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])
            [samak.code-db :as db]))

(defn defncall
  ([sym fn-name]
   (api/defexp (api/symbol sym) (api/fn-call (api/symbol fn-name) nil)))
  ([sym fn-name & args]
   (api/defexp (api/symbol sym) (api/fn-call (api/symbol fn-name) args))))

(defn defpipe
  ""
  [sym call in-spec out-spec & args]
  (defncall sym call args))


(defn pipe
  ""
  [& args]
  (api/pipe (map api/symbol args)))

(def get-val (defncall 'get-val '|>
                  (api/key-fn :samak.nodes/rhs)
                  (api/key-fn :samak.nodes/value)))

(s/def ::hiccup
  (s/cat :tag        keyword?
            :attributes (s/? map?)
            :content    (s/* (s/or :terminal string?
                                   :element  ::hiccup))))

(s/def ::ui-element (s/keys :req [:oasis.gui/order :oasis.gui/element]))

(s/def :oasis.spec/state (s/coll-of :samak.spec/toplevel-exp))
(s/def :oasis.spec/render (s/map-of keyword? ::ui-element))
(s/def :oasis.spec/gui (s/map-of keyword? ::ui-element))

(defn start
  []
  (let [oasis [(defncall 'ui 'pipes/ui)
               (defncall 'd 'pipes/debug)
               (defncall 'log 'pipes/log)
               (defncall 'layout 'pipes/layout)

               (defncall 'log-state 'pipes/log (api/string "state: "))
               (defncall 'log-layout 'pipes/log (api/string "layout: "))
               (defncall 'log-render 'pipes/log (api/string "render: "))
               (defncall 'log-events 'pipes/log (api/string "events: "))
               (defncall 'n 'pipes/eval-notify)

               get-val

               (defncall 'get-event-val '|>
                 (api/key-fn :event)
                 (api/key-fn :target)
                 (api/key-fn :value))

               (api/defexp (api/symbol 'handle-input)
                 (api/map {(api/keyword :input)
                           (api/symbol 'get-event-val)}))

               (api/defexp (api/symbol 'handle-submit)
                 (api/map {(api/keyword :submit)
                           (api/string "submit")}))

               (defncall 'is-change '=
                 (api/keyword :change))

               (defncall 'is-submit '=
                 (api/keyword :submit))

               (defncall 'is-input '|>
                 (api/key-fn :data)
                 (api/symbol 'is-change))

               (defncall 'is-submit-data '|>
                 (api/key-fn :data)
                 (api/symbol 'is-submit))

               (defncall 'filter-input 'if
                 (api/symbol 'is-input)
                 (api/symbol 'handle-input)
                 (api/symbol 'ignore))

               (defncall 'filter-submit 'if
                 (api/symbol 'is-submit-data)
                 (api/symbol 'handle-submit)
                 (api/symbol 'ignore))

               (defncall 'raw-events 'pipes/debug)
               (defncall 'reduced-events 'pipes/debug)
               (defncall 'events 'pipes/debug)

               (defncall 'merge-state '|>
                 (api/vector [(api/key-fn :state) (api/key-fn :next)])
                 (api/fn-call (api/symbol 'concat) [(api/map {})]))

               (defncall 'has-submit '|>
                 (api/key-fn :state)
                 (api/key-fn :submit))

               (defncall 'select-input '|>
                 (api/key-fn :state)
                 (api/key-fn :input))

               (defncall 'merge-without-submit '|>
                 (api/vector [(api/map {(api/keyword :input) (api/symbol 'select-input)})
                              (api/key-fn :next)])
                 (api/fn-call (api/symbol 'concat) [(api/map {})]))

               (defncall 'input-reduce 'pipes/reductions
                 (api/fn-call (api/symbol 'if) [(api/symbol 'has-submit)
                                                (api/symbol 'merge-without-submit)
                                                (api/symbol 'merge-state)])
                 (api/map {}))


               (defncall 'is-complete 'and
                 (api/key-fn :input)
                 (api/key-fn :submit))

               (defncall 'only-complete 'only (api/symbol 'is-complete))


               (defncall 'ev 'pipes/eval-line)

               (api/defexp (api/symbol 'make-eval)
                 (api/key-fn :input))


               (defncall 'translate-str 'str
                 (api/string "translate(")
                 (api/key-fn :x)
                 (api/string ",")
                 (api/key-fn :y)
                 (api/string ")"))

               (defncall 'start 'pipes/debug)
               (api/defexp (api/symbol 'repl)
                 (api/map {(api/keyword :repl)
                           (api/map {(api/keyword :oasis.gui/order)
                                     (api/integer 10)
                                     (api/keyword :oasis.gui/element)
                                     (api/vector [(api/keyword :form) (api/map {(api/keyword :on-submit) (api/keyword :submit)})
                                                  (api/vector [(api/keyword :input) (api/map {(api/keyword :on-change) (api/keyword :change)})])])})}))

               (api/defexp (api/symbol 'header)
                 (api/map {(api/keyword :header)
                           (api/map {(api/keyword :oasis.gui/order)
                                     (api/integer 1)
                                     (api/keyword :oasis.gui/element)
                                     (api/vector [(api/keyword :div)
                                                  (api/string "Oasis")])})}))

               (api/defexp (api/symbol 'css-background)
                 (api/map {(api/keyword :width) (api/string "600px")
                           (api/keyword :height) (api/string "400px")
                           (api/keyword :position) (api/string "absolute")
                           (api/keyword :z-index) (api/integer -100)
                           (api/keyword :background-color) (api/string "#AABBDD")}))

               (defncall 'calculate-y '|>
                 (api/vector [(api/integer 30) (api/key-fn :position)])
                 (api/symbol 'mult)
                 (api/vector [(api/integer 10) (api/symbol 'id)])
                 (api/symbol 'sum))

               (defncall 'menu-transform '|>
                 (api/map {(api/keyword :x) (api/integer 5)
                           (api/keyword :y) (api/symbol 'calculate-y)})
                 (api/symbol 'translate-str))

               (api/defexp (api/symbol 'render-menu-entry)
                 (api/vector [(api/keyword :g)
                              (api/map {(api/keyword :transform) (api/symbol 'menu-transform)})
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :height) (api/string "20")
                                                     (api/keyword :width) (api/string "50")
                                                     (api/keyword :fill) (api/string "#eee")})])
                              (api/vector [(api/keyword :text)
                                           (api/map {(api/keyword :height) (api/string "20")
                                                     (api/keyword :width) (api/string "100%")
                                                     (api/keyword :text-anchor) (api/keyword :middle)
                                                     (api/keyword :x) (api/integer 25)
                                                     (api/keyword :y) (api/integer 0)
                                                     (api/keyword :dy) (api/integer 14)})
                                           (api/key-fn :name)])]))

               (defncall 'render-menu '|>
                 (api/fn-call (api/symbol 'map) [(api/symbol 'render-menu-entry)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :g)])])
                 (api/vector [(api/keyword :g)
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :height) (api/string "400")
                                                     (api/keyword :width) (api/string "60")
                                                     (api/keyword :fill) (api/string "#ccc")})])
                              (api/symbol 'id)])
                 (api/map {(api/keyword :menu) (api/symbol 'id)}))

               (defncall 'menu 'pipes/debug)
               (defncall 'menu-items 'pipes/debug)

               (defncall 'menu-const '|>
                 (api/vector [(api/string "first")
                              (api/string "second")
                              (api/string "third")])
                 (api/symbol 'many))

               (defncall 'map-menu-item '|>
                 (api/map {(api/keyword :position)
                           (api/fn-call (api/symbol '|>) [(api/key-fn :state)
                                                          (api/symbol 'count)])
                           (api/keyword :name)
                           (api/key-fn :next)}))

               (defncall 'menu-map 'pipes/reductions
                 (api/fn-call (api/symbol '|>)
                              [(api/vector [(api/key-fn :state) (api/symbol 'map-menu-item)])
                               (api/symbol 'flatten)])
                 (api/vector []))


               ;; keep evaluations in state reduction

               (defncall 'state-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '|>)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/symbol 'flatten)])
                 (api/vector []))

               (defncall 'state 'pipes/debug (api/keyword :oasis.spec/state))


               ;; convert and layout nodes

               (defncall 'def-name 'str
                 (api/string "d/")
                 (api/key-fn :samak.nodes/name))

               (defncall 'is-keyword '|>
                 (api/key-fn :samak.nodes/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :samak.nodes/keyword)]))

               (defncall 'is-symbol '|>
                 (api/key-fn :samak.nodes/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :samak.nodes/symbol)]))

               (defncall 'is-long '|>
                 (api/key-fn :samak.nodes/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :samak.nodes/long)]))

               (defncall 'is-string '|>
                 (api/key-fn :samak.nodes/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :samak.nodes/string)]))

               (defncall 'is-integer '|>
                 (api/key-fn :samak.nodes/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :samak.nodes/integer)]))

               (defncall 'is-vector '|>
                 (api/key-fn :samak.nodes/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :samak.nodes/vector)]))

               (defncall 'format-node '|>
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-string)
                                                    (api/key-fn :samak.nodes/type)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-integer)
                                                    (api/key-fn :samak.nodes/value)]))

               (defncall 'init-walk '|>
                 (api/map {(api/keyword :stack)
                           (api/vector [(api/vector [(api/key-fn :samak.nodes/rhs)
                                                     (api/integer 0)])])})
                 (api/fn-call (api/symbol 'concat)
                              [(api/map {(api/keyword :current) (api/keyword :init)
                                         (api/keyword :result) (api/vector [(api/keyword :g)])
                                         (api/keyword :counter) (api/integer 0)})]))

               (defncall 'has-stack '|>
                 (api/key-fn :stack)
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "stack")])
                 (api/symbol 'count)
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "stacksize")])
                 (api/fn-call (api/symbol '>) [(api/integer 0)])
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "hasstack")])
                 )

               (defncall 'has-children '|>
                 (api/key-fn :current)
                 (api/key-fn :samak.nodes/children)
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "haschild")])
                 )

               (defncall 'walk-cont 'or
                 (api/symbol 'has-stack))

               (defncall 'stack-children '|>
                 (api/vector [(api/fn-call (api/symbol '|>) [(api/key-fn :current)
                                                             (api/key-fn :samak.nodes/children)])
                              (api/fn-call (api/symbol '|>) [(api/vector [(api/integer 10) ;FIXME
                                                                          (api/fn-call (api/symbol 'inc) [(api/key-fn :level)])])
                                                             (api/fn-call (api/symbol 'repeat) [])])])
                 (api/fn-call (api/symbol 'zip) []))

               (defncall 'push-children '|>
                 (api/map {(api/keyword :stack) (api/fn-call (api/symbol '|>)
                                                             [(api/vector [(api/key-fn :stack)
                                                                           (api/symbol 'stack-children)])
                                                              (api/fn-call (api/symbol 'into) [])])
                           (api/keyword :current) (api/key-fn :current)
                           (api/keyword :level) (api/key-fn :level)
                           (api/keyword :counter) (api/key-fn :counter)
                           (api/keyword :result) (api/key-fn :result)}))

               (defncall 'handle-children '|>
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "children")])
                 (api/fn-call (api/symbol 'incase)
                              [(api/symbol 'has-children)
                               (api/symbol 'push-children)])
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "afterchildren")])
                 )

               (api/defexp (api/symbol 'pop-current)
                 (api/map {(api/keyword :stack) (api/fn-call (api/symbol '|>)
                                                             [(api/key-fn :stack)
                                                              (api/fn-call (api/symbol 'drop)
                                                                           [(api/integer 1)])])
                           (api/keyword :current) (api/fn-call (api/symbol '|>)
                                                             [(api/key-fn :stack)
                                                              (api/fn-call (api/symbol 'nth)
                                                                           [(api/integer 0)])
                                                              (api/fn-call (api/symbol 'nth)
                                                                           [(api/integer 0)])])
                           (api/keyword :level) (api/fn-call (api/symbol '|>)
                                                             [(api/key-fn :stack)
                                                              (api/fn-call (api/symbol 'nth)
                                                                           [(api/integer 0)])
                                                              (api/fn-call (api/symbol 'nth)
                                                                           [(api/integer 1)])])
                           (api/keyword :counter) (api/fn-call (api/symbol 'inc) [(api/key-fn :counter)])
                           (api/keyword :result) (api/key-fn :result)}))

               (defncall 'cell-y '|>
                 (api/vector [(api/integer 16)
                              (api/key-fn :counter)])
                 (api/symbol 'mult))

               (defncall 'cell-x '|>
                 (api/vector [(api/integer 10)
                              (api/key-fn :level)])
                 (api/symbol 'mult))

               (defncall 'cell-pos '|>
                 (api/map {(api/keyword :x)
                           (api/symbol 'cell-x)
                           (api/keyword :y)
                           (api/symbol 'cell-y)})
                 (api/symbol 'translate-str))

               (defncall 'type-pos '|>
                 (api/map {(api/keyword :x)
                           (api/integer 50)
                           (api/keyword :y)
                           (api/symbol 'cell-y)})
                 (api/symbol 'translate-str))

               (defncall 'make-cell '|>
                 (api/vector [(api/keyword :g)
                              (api/vector [(api/keyword :text)
                                           (api/map {(api/keyword :transform)
                                                     (api/symbol 'cell-pos)})
                                           (api/fn-call (api/symbol 'str) [(api/key-fn :value)])])
                              (api/vector [(api/keyword :text)
                                           (api/map {(api/keyword :transform)
                                                     (api/symbol 'type-pos)
                                                     (api/keyword :font-style)
                                                     (api/string "italic")
                                                     (api/keyword :fill)
                                                     (api/string "darkgrey")})
                                           (api/fn-call (api/symbol 'str) [(api/key-fn :type)])])]))

               (defncall 'handle-unknown '|>
                 (api/fn-call (api/symbol 'spy) [(api/string "unknown")])
                 (api/map {(api/keyword :result)
                           (api/fn-call (api/symbol '|>)
                                        [(api/fn-call (api/symbol 'str) [(api/string "unknown: ")
                                                                         (api/symbol 'id)])
                                         (api/symbol 'make-cell)])}))

               (defncall 'handle-int '|>
                 (api/fn-call (api/symbol 'spy) [(api/string "int")])
                 (api/map {(api/keyword :result)
                           (api/fn-call (api/symbol '|>)
                                        [(api/map {(api/keyword :value) (api/fn-call (api/symbol 'str) [(api/fn-call (api/symbol '|>) [(api/key-fn :current)
                                                                                                                                       (api/key-fn :samak.nodes/value)])])
                                                   (api/keyword :type) (api/string "int")
                                                   (api/keyword :counter) (api/key-fn :counter)
                                                   (api/keyword :level) (api/key-fn :level)})
                                         (api/symbol 'make-cell)])}))

               (defncall 'handle-str '|>
                 (api/fn-call (api/symbol 'spy) [(api/string "str")])
                 (api/map {(api/keyword :result)
                           (api/fn-call (api/symbol '|>)
                                        [(api/fn-call (api/symbol 'str) [(api/string "str: ")
                                                                         (api/key-fn :samak.nodes/value)])
                                         (api/symbol 'make-cell)])}))

               (defncall 'make-vec '|>
                 (api/vector [(api/keyword :text)
                              (api/map {(api/keyword :transform)
                                        (api/fn-call (api/symbol '|>) [(api/map {(api/keyword :x)
                                                                                 (api/integer 15)
                                                                                 (api/keyword :y)
                                                                                 (api/symbol 'calculate-y)})
                                                                       (api/symbol 'translate-str)])})]))

               (defncall 'handle-vector '|>
                 (api/fn-call (api/symbol 'spy) [(api/string "vec")])
                 (api/map {(api/keyword :result)

                           (api/fn-call (api/symbol '|>)
                                        [(api/key-fn :samak.nodes/children)
                                         (api/fn-call (api/symbol 'str) [(api/string "vec: ")
                                                                         (api/symbol 'id)])
                                         (api/symbol 'make-cell)])}))



               (defncall 'handle-by-type '|>
                 (api/fn-call (api/symbol 'spy) [(api/string "type")])
                 (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '|>) [(api/key-fn :current)
                                                                                   (api/symbol 'is-integer)])
                                                    (api/symbol 'handle-int)])
                 (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '|>) [(api/key-fn :current)
                                                                                   (api/symbol 'is-vector)])
                                                    (api/symbol 'handle-vector)])
                 (api/fn-call (api/symbol 'unless) [(api/key-fn :result) (api/symbol 'handle-unknown)])
                 (api/key-fn :result)
                 (api/vector [(api/symbol 'id)])
                 (api/fn-call (api/symbol 'spy) [(api/string "handled")]))

               (defncall 'handle-node '|>
                 (api/fn-call (api/symbol 'spy) [(api/string "HANDLE")])

                 (api/map {(api/keyword :result) (api/fn-call (api/symbol '|>)
                                                              [(api/vector [(api/key-fn :result)
                                                                            (api/symbol 'handle-by-type)])
                                                               (api/fn-call (api/symbol 'into) [])])
                           (api/keyword :stack) (api/key-fn :stack)
                           (api/keyword :counter) (api/key-fn :counter)
                           (api/keyword :level) (api/key-fn :level)
                           (api/keyword :current) (api/key-fn :current)})
                 (api/fn-call (api/symbol 'spy) [(api/string "after")]))


               (defncall 'walk-node '|>
                 (api/symbol 'pop-current)
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "pop")])
                 (api/symbol 'handle-node)
                 (api/symbol 'handle-children))

               (defncall 'walk-ast 'loop
                 (api/symbol 'has-stack)
                 (api/symbol 'walk-node)
                 (api/symbol 'init-walk))

               (defncall 'format-ast '|>
                 (api/symbol 'walk-ast)
                 (api/key-fn :result)
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "ast")])
                 )

               (api/defexp (api/symbol 'format-def)
                 (api/map {(api/keyword :id) (api/symbol 'def-name)
                           (api/keyword :name) (api/key-fn :samak.nodes/name)
                           (api/keyword :value) (api/symbol 'format-ast)
                           (api/keyword :width) (api/integer 100)
                           (api/keyword :height) (api/integer 100)}))


               (defncall 'pipe-name 'str
                 (api/string "p/")
                 (api/key-fn :from)
                 (api/string "-")
                 (api/key-fn :to))


               (defncall 'format-pipe '|>
                 (api/map {(api/keyword :test) (api/symbol 'id)
                           (api/keyword :id) (api/symbol 'pipe-name)
                           (api/keyword :source) (api/fn-call (api/symbol 'str)
                                                              [(api/string "d/")
                                                               (api/key-fn :from)])
                           (api/keyword :target) (api/fn-call (api/symbol 'str)
                                                              [(api/string "d/")
                                                               (api/key-fn :to)])}))


               (defncall 'is-def '|>
                 (api/key-fn :samak.nodes/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :samak.nodes/def)]))

               (defncall 'is-pipe '|>
                 (api/key-fn :samak.nodes/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :samak.nodes/pipe)]))

               (defncall 'filter-nodes 'filter (api/symbol 'is-def))
               (defncall 'filter-connections 'filter (api/symbol 'is-pipe))

               (defncall 'format-defs '|>
                 (api/key-fn :defs)
                 (api/fn-call (api/symbol 'map) [(api/symbol 'format-def)]))

               (defncall 'extract-connection '|>
                 (api/key-fn :samak.nodes/arguments)
                 (api/fn-call (api/symbol 'map) [(api/key-fn :samak.nodes/value)])
                 (api/vector [(api/map {(api/keyword :from) (api/fn-call (api/symbol 'nth) [(api/integer 0)])
                                        (api/keyword :to) (api/fn-call (api/symbol 'nth) [(api/integer 1)])})
                              (api/map {(api/keyword :from) (api/fn-call (api/symbol 'nth) [(api/integer 1)])
                                        (api/keyword :to) (api/fn-call (api/symbol 'nth) [(api/integer 2)])})]))


               (defncall 'format-pipes '|>
                 (api/key-fn :pipes)
                 (api/fn-call (api/symbol 'mapcat) [(api/symbol 'extract-connection)])
                 (api/fn-call (api/symbol 'map) [(api/symbol 'format-pipe)]))

               (defncall 'format-state '|>
                 (api/map {(api/keyword :defs) (api/symbol 'filter-nodes)
                           (api/keyword :pipes) (api/symbol 'filter-connections)})
                 (api/map {(api/keyword :id) (api/string "root")
                           (api/keyword :children) (api/symbol 'format-defs)
                           (api/keyword :edges) (api/symbol 'format-pipes)}))


               (defncall 'lay-in 'pipes/debug)

               ;; graphing of nodes

               (api/defexp (api/symbol 'graph-node)
                 (api/vector [(api/keyword :g)
                              (api/map {(api/keyword :transform)
                                        (api/symbol 'translate-str) })
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :width) (api/key-fn :width)
                                                     (api/keyword :height) (api/key-fn :height)
                                                     (api/keyword :style) (api/map {(api/keyword :fill) (api/string "#fff")
                                                                                    (api/keyword :stroke) (api/string "darkgrey")})})])
                              (api/vector [(api/keyword :text)
                                           (api/map {(api/keyword :x) (api/integer 5)
                                                     (api/keyword :y) (api/integer 20)
                                                     (api/keyword :font-weight) (api/string "bold")})
                                           (api/key-fn :name)])
                              (api/vector [(api/keyword :g)
                                           (api/map {(api/keyword :transform)
                                                     (api/fn-call (api/symbol '|>) [(api/map {(api/keyword :x)
                                                                                              (api/integer 5)
                                                                                              (api/keyword :y)
                                                                                              (api/integer 20)})
                                                                       (api/symbol 'translate-str)])})
                                           (api/key-fn :value)])]))

               (defncall 'graph-nodes '|>
                 (api/key-fn :children)
                 (api/fn-call (api/symbol 'map) [(api/symbol 'graph-node)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :g)])]))

               (defncall 'graph-connection '|>
                 (api/key-fn :sections)
                 (api/fn-call (api/symbol 'nth) [(api/integer 0)])
                 (api/vector [(api/keyword :g)
                              (api/vector [(api/keyword :line)
                                           (api/map {(api/keyword :style) (api/map {(api/keyword :stroke) (api/string "darkgrey")})
                                                     (api/keyword :x1) (api/fn-call (api/symbol '|>)[(api/key-fn :startPoint) (api/key-fn :x)])
                                                     (api/keyword :y1) (api/fn-call (api/symbol '|>)[(api/key-fn :startPoint) (api/key-fn :y)])
                                                     (api/keyword :x2) (api/fn-call (api/symbol '|>)[(api/key-fn :endPoint) (api/key-fn :x)])
                                                     (api/keyword :y2) (api/fn-call (api/symbol '|>)[(api/key-fn :endPoint) (api/key-fn :y)])})])]))

               (defncall 'graph-connections '|>
                 (api/key-fn :edges)
                 (api/fn-call (api/symbol 'map) [(api/symbol 'graph-connection)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :g)])]))

               (api/defexp (api/symbol 'graph)
                 (api/map {(api/keyword :graph)
                           (api/vector [(api/keyword :g)
                                        (api/map {(api/keyword :transform) (api/string "translate(70,0)")})
                                        (api/symbol 'graph-nodes)
                                        (api/symbol 'graph-connections)])}))


               ;; reduce elements to latest version of GUI element

               (defncall 'elements-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '|>)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
                 (api/map {}))

               (defncall 'svg-elements-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '|>)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
                 (api/map {}))

               (defncall 'reducer 'pipes/debug (api/keyword :oasis.spec/gui))
               (defncall 'render 'pipes/debug (api/keyword :oasis.spec/render))

               ;; render elements to hiccup
               (defncall 'render-elements '|>
                 (api/fn-call (api/symbol 'vals) nil)
                 ;; (api/fn-call (api/symbol 'sort-by [(api/symbol 'id)]))
                 (api/fn-call (api/symbol 'map) [(api/key-fn :oasis.gui/element)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :div)])]))

               (defncall 'svg-reduced 'pipes/debug (api/keyword :oasis.spec/render))

               ;; render SVG components
               (defncall 'svg-render 'pipes/debug)
               (api/defexp (api/symbol 'render-svg)
                 (api/map {(api/keyword :svg)
                           (api/map {(api/keyword :oasis.gui/order)
                                     (api/integer 2)
                                     (api/keyword :oasis.gui/element)
                                     (api/vector [(api/keyword :svg)
                                                  (api/map {(api/keyword :width) (api/integer 600)
                                                            (api/keyword :height) (api/integer 400)})
                                                  (api/vector [(api/keyword :rect)
                                                               (api/map {(api/keyword :width) (api/integer 600)
                                                                         (api/keyword :height) (api/integer 400)
                                                                         (api/keyword :fill) (api/string "#eee")})
                                                               ])
                                                  (api/key-fn :graph)
                                                  (api/key-fn :menu)])})}))

               (defncall 'oasis 'net
                 (api/vector [(pipe 'd 'log)
                              (pipe 'ui 'log)

                              (pipe 'reduced-events 'only-complete 'events)
                              (pipe 'raw-events 'input-reduce 'reduced-events)

                              (pipe 'events 'log-events)
                              (pipe 'events 'make-eval 'ev)
                              (pipe 'events 'make-eval 'log)

                              (pipe 'ui 'filter-input 'raw-events)
                              (pipe 'ui 'filter-submit 'raw-events)

                              (pipe 'n 'state-reduce 'state)
                              (pipe 'state 'log-state)

                              (pipe 'state 'format-state 'layout)

                              (pipe 'layout 'log-layout)

                              (pipe 'layout 'graph 'svg-render)
                              (pipe 'render 'elements-reduce 'reducer)
                              (pipe 'render 'elements-reduce 'log-render)

                              (pipe 'reducer 'render-elements 'log)
                              (pipe 'reducer 'render-elements 'ui)

                              (pipe 'svg-render 'svg-elements-reduce 'svg-reduced)
                              (pipe 'svg-reduced 'render-svg 'render)

                              (pipe 'start 'menu-const 'menu-items)
                              (pipe 'menu-items 'menu-map 'menu)
                              (pipe 'menu 'render-menu 'svg-render)
                              (pipe 'start 'header 'render)
                              (pipe 'start 'repl 'render)])
                 )]]
    oasis))

(defn persist [db]
  (db/parse-tree->db db (start)))
