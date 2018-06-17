(ns samak.oasis
  (:require [samak.api    :as api]
            [samak.core   :as core]
            [samak.stdlib :as pipes]
            #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])))

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

(s/def ::ui-element (s/keys :req [:oasis/order :oasis/element]))

(s/def :oasis/eval-node (s/keys :req [:samak.nodes/type
                                      :samak.nodes/name]
                                :opt [:samak.nodes/rhs]))

(s/def :oasis/state (s/coll-of :oasis/eval-node))
(s/def :oasis/render (s/map-of keyword? ::ui-element))
(s/def :oasis/gui (s/map-of keyword? ::ui-element))

(defn start
  []
  (let [oasis [(defncall 'ui 'pipes/ui)
               (defncall 'd 'pipes/debug)
               (defncall 'log 'pipes/log)
               (defncall 'log-state 'pipes/log (api/string "state: "))
               (defncall 'log-render 'pipes/log (api/string "render: "))
               (defncall 'n 'pipes/eval-notify)

               (api/defexp (api/symbol 'graph-node)
                 (api/vector [(api/keyword :g)
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :width) (api/integer 60)
                                                     (api/keyword :height) (api/integer 40)
                                                     (api/keyword :x) (api/integer 50)
                                                     (api/keyword :y) (api/integer 20)
                                                     (api/keyword :style) (api/map {(api/keyword :fill) (api/string "#fff")
                                                                                    (api/keyword :stroke) (api/string "darkgrey")})})])
                              (api/vector [(api/keyword :text)
                                           (api/map {(api/keyword :x) (api/integer 80)
                                                     (api/keyword :y) (api/integer 40)})
                                           (api/key-fn :samak.nodes/name)
                                           (api/symbol 'get-val)])]))

               (defncall 'graph-nodes '|>
                 (api/fn-call (api/symbol 'map) [(api/symbol 'graph-node)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :g)])]))



               (api/defexp (api/symbol 'graph)
                 (api/map {(api/keyword :graph)
                           (api/map {(api/keyword :oasis/order)
                                     (api/integer 5)
                                     (api/keyword :oasis/element)
                                     (api/vector [(api/keyword :svg)
                                                  (api/map {(api/keyword :width) (api/integer 600)
                                                            (api/keyword :height) (api/integer 400)})
                                                  (api/vector [(api/keyword :rect)
                                                               (api/map {(api/keyword :width) (api/integer 600)
                                                                         (api/keyword :height) (api/integer 400)
                                                                         (api/keyword :fill) (api/string "#eee")})
                                                               ])
                                                  (api/symbol 'graph-nodes)])})}))

               (pipe 'd 'log)
               (pipe 'ui 'log)

               (defncall 'get-event-val '|>
                 (api/key-fn :event)
                 (api/key-fn :target)
                 (api/key-fn :value))

               (api/defexp (api/symbol 'handle-input)
                 (api/vector [(api/keyword :div)
                              (api/symbol 'get-event-val)]))
               (pipe 'ui 'handle-input 'log)

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

               (pipe 'ui 'handle-event 'log)

               (defncall 'oasis 'pipes/debug)
               (api/defexp (api/symbol 'repl)
                 (api/map {(api/keyword :repl)
                           (api/map {(api/keyword :oasis/order)
                                     (api/integer 10)
                                     (api/keyword :oasis/element)
                                     (api/vector [(api/keyword :form) (api/map {(api/keyword :on-submit) (api/keyword :submit)})
                                                  (api/vector [(api/keyword :input) (api/map {(api/keyword :on-change) (api/keyword :change)})])])})}))

               (api/defexp (api/symbol 'header)
                 (api/map {(api/keyword :header)
                           (api/map {(api/keyword :oasis/order)
                                     (api/integer 1)
                                     (api/keyword :oasis/element)
                                     (api/vector [(api/keyword :div)
                                                  (api/string "Oasis")])})}))

               (defncall 'render 'pipes/debug (api/keyword :oasis/render))
               (pipe 'oasis 'header 'render)
               (pipe 'oasis 'repl 'render)

               ;; keep evaluations in state reduction

               (defncall 'state-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '|>)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/symbol 'flatten)])
                 (api/vector []))

               (defncall 'state 'pipes/debug (api/keyword :oasis/state))

               (pipe 'n 'state-reduce 'state)
               (pipe 'state 'log-state)
               (pipe 'state 'graph 'render)

               ;; reduce elements to latest version of GUI element

               (defncall 'elements-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '|>)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
                 (api/map {}))


               (defncall 'reducer 'pipes/debug (api/keyword :oasis/gui))
               (pipe 'render 'elements-reduce 'reducer)
               (pipe 'render 'elements-reduce 'log-render)

               ;; render elements to hiccup

               (defncall 'render-elements '|>
                 (api/fn-call (api/symbol 'vals) nil)
                 ;; (api/fn-call (api/symbol 'sort-by [(api/symbol 'id)]))
                 (api/fn-call (api/symbol 'map) [(api/key-fn :oasis/element)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :div)])]))

               (pipe 'reducer 'render-elements 'log)
               (pipe 'reducer 'render-elements 'ui)
               ]]
    oasis))
