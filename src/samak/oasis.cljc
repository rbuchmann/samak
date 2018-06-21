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
               (defncall 'n 'pipes/eval-notify)

               get-val

               (defncall 'get-event-val '|>
                 (api/key-fn :event)
                 (api/key-fn :target)
                 (api/key-fn :value))

               (api/defexp (api/symbol 'handle-input)
                 (api/vector [(api/keyword :div)
                              (api/symbol 'get-event-val)]))

               (api/defexp (api/symbol 'handle-submit)
                 (api/vector [(api/keyword :div)
                              (api/string "submit")]))

               (defncall 'is-change '=
                 (api/keyword :change))

               (defncall 'is-input '|>
                 (api/key-fn :data)
                 (api/symbol 'is-change))

               (defncall 'handle-event '|>
                 (api/fn-call (api/symbol 'if) [(api/symbol 'is-input)
                                                (api/symbol 'handle-input)
                                                (api/symbol 'handle-submit)]))


               (api/defexp (api/symbol 'test) (api/map {(api/keyword :test) (api/keyword :foo)}))

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

               (defncall 'render 'pipes/debug (api/keyword :oasis.spec/render))

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

               (api/defexp (api/symbol 'format-def)
                 (api/map {(api/keyword :id) (api/symbol 'def-name)
                           (api/keyword :name) (api/key-fn :samak.nodes/name)
                           (api/keyword :value) (api/symbol 'get-val)
                           (api/keyword :width) (api/integer 100)
                           (api/keyword :height) (api/integer 100)}))

               (defncall 'first-arg-value '|>
                 (api/key-fn :samak.nodes/arguments)
                 (api/fn-call (api/symbol 'nth) [(api/integer 0)])
                 (api/key-fn :samak.nodes/value))

               (defncall 'second-arg-value '|>
                 (api/key-fn :samak.nodes/arguments)
                 (api/fn-call (api/symbol 'nth) [(api/integer 1)])
                 (api/key-fn :samak.nodes/value))


               (defncall 'pipe-name 'str
                 (api/string "p/")
                 (api/symbol 'first-arg-value)
                 (api/string "-")
                 (api/symbol 'second-arg-value))

               (api/defexp (api/symbol 'format-pipe)
                 (api/map {(api/keyword :id) (api/symbol 'pipe-name)
                           (api/keyword :source) (api/fn-call (api/symbol 'str)
                                                              [(api/string "d/")
                                                               (api/symbol 'first-arg-value)])
                           (api/keyword :target) (api/fn-call (api/symbol 'str)
                                                              [(api/string "d/")
                                                               (api/symbol 'second-arg-value)])}))


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

               (defncall 'format-pipes '|>
                 (api/key-fn :pipes)
                 (api/fn-call (api/symbol 'map) [(api/symbol 'format-pipe)]))

               (defncall 'format-state '|>
                 (api/map {(api/keyword :defs) (api/symbol 'filter-nodes)
                           (api/keyword :pipes) (api/symbol 'filter-connections)})
                 (api/map {(api/keyword :id) (api/string "root")
                           (api/keyword :children) (api/symbol 'format-defs)
                           (api/keyword :edges) (api/symbol 'format-pipes)}))


               (defncall 'lay-in 'pipes/debug)


               (defncall 'translate-str 'str
                 (api/string "translate(")
                 (api/key-fn :x)
                 (api/string ",")
                 (api/key-fn :y)
                 (api/string ")"))

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
                                           (api/map {(api/keyword :x) (api/integer 0)
                                                     (api/keyword :y) (api/integer 20)})
                                           (api/key-fn :name)
                                           (api/string " - ")
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
                           (api/map {(api/keyword :oasis.gui/order)
                                     (api/integer 5)
                                     (api/keyword :oasis.gui/element)
                                     (api/vector [(api/keyword :svg)
                                                  (api/map {(api/keyword :width) (api/integer 600)
                                                            (api/keyword :height) (api/integer 400)})
                                                  (api/vector [(api/keyword :rect)
                                                               (api/map {(api/keyword :width) (api/integer 600)
                                                                         (api/keyword :height) (api/integer 400)
                                                                         (api/keyword :fill) (api/string "#eee")})
                                                               ])
                                                  (api/symbol 'graph-nodes)
                                                  (api/symbol 'graph-connections)])})}))


               ;; reduce elements to latest version of GUI element

               (defncall 'elements-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '|>)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
                 (api/map {}))


               (defncall 'reducer 'pipes/debug (api/keyword :oasis.spec/gui))

               ;; render elements to hiccup
               (defncall 'render-elements '|>

                 (api/fn-call (api/symbol 'vals) nil)
                 ;; (api/fn-call (api/symbol 'sort-by [(api/symbol 'id)]))
                 (api/fn-call (api/symbol 'map) [(api/key-fn :oasis.gui/element)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :div)])]))

               (defncall 'oasis 'net
                 (pipe 'd 'log)
                 (pipe 'ui 'log)
                 (pipe 'ui 'handle-input 'log)
                 (pipe 'ui 'handle-event 'log)

                 (pipe 'start 'header 'render)
                 (pipe 'start 'repl 'render)

                 (pipe 'n 'state-reduce 'state)
                 (pipe 'state 'log-state)

                 (pipe 'state 'format-state 'lay-in)
                 (pipe 'state 'format-state 'layout)

                 (pipe 'layout 'log-layout)
                 (pipe 'lay-in 'log-layout)

                 (pipe 'layout 'graph 'render)
                 (pipe 'render 'elements-reduce 'reducer)
                 (pipe 'render 'elements-reduce 'log-render)

                 (pipe 'reducer 'render-elements 'log)
                 (pipe 'reducer 'render-elements 'ui)
                 )
               ]]
    oasis))

(defn persist [db]
  (db/parse-tree->db db (start)))
