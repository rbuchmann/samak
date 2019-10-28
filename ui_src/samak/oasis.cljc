(ns ^:figwheel-no-load samak.oasis
  #?@
   (:clj
    [(:require
      [clojure.spec.alpha :as s]
      [samak.api :as api]
      [samak.code-db :as db]
      [samak.nodes :as n]
      [samak.pipes :as pipes]
      samak.spec
      [samak.stdlib :as std])]
    :cljs
    [(:require
      [cljs.spec.alpha :as s]
      [samak.api :as api]
      [samak.code-db :as db]
      [samak.nodes :as n]
      [samak.pipes :as pipes]
      samak.spec
      [samak.stdlib :as std])]))

(defn defncall
  ([sym fn-name]
   (api/defexp sym (api/fn-call (api/symbol fn-name) [])))
  ([sym fn-name & args]
   (api/defexp sym (api/fn-call (api/symbol fn-name) args))))

(defn defmap
  [sym m]
  (defncall sym '-> (api/map m)))

(defn defpipe
  ""
  [sym call in-spec out-spec & args]
  (defncall sym call args))

(defn pipify
  ""
  [x]
  (api/defexp (symbol (str "x-" (name x))) (api/fn-call (api/symbol '|>) [(api/symbol x)])))

(defn pipe
  ""
  ([in out]
   (api/pipe (api/symbol in) (api/symbol out)))
  ([in x out]
   (let [xfer (symbol (str "x-" (name x)))]
     [(pipify x)
      (api/pipe (api/symbol in)
                (api/symbol xfer)
                (api/symbol out))])))

(defn red
  ""
  ([in x out]
   (api/pipe (api/symbol in)
             (api/symbol x)
             (api/symbol out))))

(s/def ::hiccup
  (s/cat :tag        keyword?
            :attributes (s/? map?)
            :content    (s/* (s/or :terminal string?
                                   :element  ::hiccup))))

(s/def ::ui-element (s/keys :req [:oasis.gui/order :oasis.gui/element]))

;; (s/def :oasis.spec/eval-content (s/coll-of :samak.spec/toplevel-exp))
;; (s/def :oasis.spec/eval-state (s/keys :req [:eval]))

;; (s/def :oasis.spec/state (s/keys :req-un [:]))

(s/def :oasis.spec/render (s/map-of keyword? ::ui-element))
(s/def :oasis.spec/gui (s/map-of keyword? ::ui-element))

(s/def :oasis.spec/mouse-event (s/keys :req-un [:samak.mouse/type]))
(s/def :oasis.spec/mouse-state (s/keys :req-un [:samak.mouse/type]))

(def oasis   [(defncall 'oasis-ui 'pipes/ui (api/integer 2))
              (defncall 'oasis-ev 'pipes/ui (api/integer 3))
              (defncall 'oasis-mouse 'pipes/mouse)
              (defncall 'oasis-kb 'pipes/keyboard)
              (defncall 'oasis-layout 'pipes/layout)

              (defncall 'd 'pipes/debug)
              (defncall 'log 'pipes/log)

              (defncall 'log-state 'pipes/log (api/string "log-state: "))
              (defncall 'log-command 'pipes/log (api/string "cmd: "))
              (defncall 'log-layout 'pipes/log (api/string "layout: "))
              (defncall 'log-render 'pipes/log (api/string "render: "))
              (defncall 'log-events 'pipes/log (api/string "events: "))
              (defncall 'log-editor 'pipes/log (api/string "editor: "))
              (defncall 'log-mouse 'pipes/log (api/string "mouse: "))
              (defncall 'log-hover 'pipes/log (api/string "hover: "))
              (defncall 'log-keyboard 'pipes/log (api/string "keyboard: "))
              (defncall 'n 'pipes/eval-notify)

              ;; dark theme based on base16-atelierdune-dark
              ;; (http://atelierbram.github.io/syntax-highlighting/atelier-schemes/dune)
              (defmap 'get-color
                {(api/keyword :cell-active) (api/string "#4d4a41")
                 (api/keyword :cell-edit) (api/string "#6684e1")
                 (api/keyword :cell-seclight) (api/string "#999580")
                 (api/keyword :cell-background) (api/string "#292824")
                 (api/keyword :cell-content) (api/string "#e8e4cf")
                 (api/keyword :cell-active-content) (api/string "#fefbec")
                 (api/keyword :cell-dividers) (api/string "#6e6b5e")
                 (api/keyword :cell-type-fill) (api/string "#6e6b5e")
                 (api/keyword :cell-type-stroke) (api/string "#20201d")
                 (api/keyword :cell-counter-stroke) (api/string "#6e6b5e")
                 (api/keyword :node-selected) (api/string "#6684e1")
                 (api/keyword :node-bg) (api/string "#292824")
                 (api/keyword :node-name-stroke) (api/string "#e8e4cf")
                 (api/keyword :node-gutter) (api/string "#292824")
                 (api/keyword :element-highlight-stroke) (api/string "#6684e1")
                 (api/keyword :pipe-fill) (api/string "#292824")
                 (api/keyword :pipe-stroke) (api/string "#a6a28c")
                 (api/keyword :graph-background) (api/string "#20201d")
                 (api/keyword :shadow-flood) (api/string "#292824")
                 (api/keyword :menu-entry-bg) (api/string "#999580")
                 (api/keyword :menu-entry-active-bg) (api/string "#a6a28c")
                 (api/keyword :menu-entry-text) (api/string "#fefbec")
                 (api/keyword :menu-bar) (api/string "#6e6b5e")
                 })

              (defmap 'get-font
                {(api/string "str") (api/string "serif")})

              (defmap 'get-syntax-color
                {(api/string "str") (api/map {(api/keyword :cell-content) (api/string "#60ac39")
                                              (api/keyword :cell-active-content) (api/string "#60ac39")})
                 (api/string "kw") (api/map {(api/keyword :cell-content) (api/string "#b65611")
                                             (api/keyword :cell-active-content) (api/string "#b65611")})
                 (api/string "int") (api/map {(api/keyword :cell-content) (api/string "#1fad83")
                                              (api/keyword :cell-active-content) (api/string "#1fad83")})
                 (api/string "float") (api/map {(api/keyword :cell-content) (api/string "#1fad83")
                                                (api/keyword :cell-active-content) (api/string "#1fad83")})
                 (api/string "acc") (api/map {(api/keyword :cell-content) (api/string "#ae9513")
                                              (api/keyword :cell-active-content) (api/string "#ae9513")})
                 (api/string "func") (api/map {(api/keyword :cell-content) (api/string "#6684e1")
                                               (api/keyword :cell-active-content) (api/string "#6684e1")})
                 (api/string "table") (api/map {(api/keyword :cell-content) (api/string "#d43552")
                                                (api/keyword :cell-active-content) (api/string "#d43552")})
                 (api/string "list") (api/map {(api/keyword :cell-content) (api/string "#b854d4")
                                               (api/keyword :cell-active-content) (api/string "#b854d4")})})

              (defncall 'config-color '->
                (api/symbol 'get-color))


              (defncall 'get-event-val '->
                (api/key-fn :event)
                (api/key-fn :target)
                (api/key-fn :value))

              (defmap 'handle-input
                {(api/keyword :input)
                 (api/symbol 'get-event-val)})

              (defmap 'handle-submit
                {(api/keyword :submit)
                 (api/string "submit")})

              (defncall 'is-change '=
                (api/keyword :change))

              (defncall 'is-submit '=
                (api/keyword :submit))

              (defncall 'is-input '->
                (api/key-fn :data)
                (api/symbol 'is-change))

              (defncall 'is-submit-data '->
                (api/key-fn :data)
                (api/symbol 'is-submit))

              (defncall 'filter-input '->
                (api/fn-call (api/symbol 'if)
                             [(api/symbol 'is-input)
                              (api/symbol 'handle-input)
                              (api/symbol 'ignore)]))

              (defncall 'filter-submit '->
                (api/fn-call (api/symbol 'if)
                             [(api/symbol 'is-submit-data)
                              (api/symbol 'handle-submit)
                              (api/symbol 'ignore)]))

              (defncall 'is-lmb-click-event '->
                (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '->) [(api/key-fn :data)
                                                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :click)])])
                                                (api/fn-call (api/symbol '->) [(api/key-fn :event)
                                                                               (api/key-fn :button)
                                                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :primary)])])]))

              (defncall 'construct-select '->
                (api/map {(api/keyword :command) (api/keyword :select)
                          (api/keyword :data) (api/key-fn :event)}))

              (defncall 'filter-select 'if
                (api/symbol 'is-lmb-click-event)
                (api/symbol 'construct-select)
                (api/symbol 'ignore))

              (defncall 'raw-events 'pipes/debug)
              (defncall 'reduced-events 'pipes/debug)
              (defncall 'events 'pipes/debug)

              (defncall 'merge-state '->
                (api/vector [(api/key-fn :state) (api/key-fn :next)])
                (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]))

              (defncall 'has-submit '->
                (api/key-fn :state)
                (api/key-fn :submit))

              (defncall 'select-input '->
                (api/key-fn :state)
                (api/key-fn :input))

              (defncall 'merge-without-submit '->
                (api/vector [(api/map {(api/keyword :input) (api/symbol 'select-input)})
                             (api/key-fn :next)])
                )

              (defncall 'input-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
                (api/map {}))

              (defncall 'tag-events '->
                (api/map {(api/keyword :events) (api/symbol '_)}))

              (defncall 'is-complete 'and
                (api/key-fn :input)
                (api/key-fn :submit))

              (defncall 'only-complete '->
                (api/fn-call (api/symbol 'only) [(api/symbol 'is-complete)]))


              ;; (defncall 'ev 'pipes/eval-line)

              (defncall 'make-eval '->
                (api/key-fn :input))

              (defncall 'is-valid-target '->
                (api/fn-call (api/symbol '=) [(api/fn-call (api/symbol 'count) [(api/symbol '_)])
                                             (api/integer 2)]))

              (defncall 'make-target '->
                (api/fn-call (api/symbol 'str-split) [(api/symbol '_) (api/string "/")])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-valid-target)
                                                   (api/map {(api/keyword :type) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])
                                                             (api/keyword :name) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])})]))


              ;; helpers

              (defncall 'translate-str 'str
                (api/string "translate(")
                (api/key-fn :x)
                (api/string ",")
                (api/key-fn :y)
                (api/string ")"))

              (defncall 'fn-name-from-select '->
                (api/fn-call (api/symbol 'str-split) [(api/symbol '_) (api/string "d/")]) ;; func/d/
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-valid-target)
                                                   (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])]))


              ;; repl

              (defncall 'oasis-init 'pipes/debug)
              (defncall 'init 'pipes/debug)
              (defmap 'repl
                {(api/keyword :repl)
                 (api/map {(api/keyword :oasis.gui/order)
                           (api/integer 10)
                           (api/keyword :oasis.gui/element)
                           (api/vector [(api/keyword :form) (api/map {(api/keyword :on-submit) (api/keyword :submit)})
                                        (api/vector [(api/keyword :input) (api/map {(api/keyword :on-change) (api/keyword :change)
                                                                                    (api/keyword :id) (api/string "input/repl")
                                                                                    (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "auto")})})])])})})

              (defncall 'header '->
                (api/map {(api/keyword :header)
                          (api/map {(api/keyword :oasis.gui/order)
                                    (api/integer 1)
                                    (api/keyword :oasis.gui/element)
                                    (api/vector [(api/keyword :h1)
                                                 (api/string "사막 Oasis")])})}))

              (defncall 'calculate-y '->
                (api/fn-call (api/symbol '*) [(api/integer 100) (api/key-fn :position)])
                (api/fn-call (api/symbol '+) [(api/integer 10) (api/symbol '_)]))

              (defncall 'menu-transform '->
                (api/key-fn :item)
                (api/map {(api/keyword :x) (api/integer 50)
                          (api/keyword :y) (api/symbol 'calculate-y)})
                (api/symbol 'translate-str))

              (defncall 'animate-sink '->
                (api/vector [(api/vector [(api/keyword :animate)
                                          (api/map {(api/keyword :attributeName) (api/string "stroke")
                                                    (api/keyword :values) (api/string "#999580;#6684e1;#6684e1;#6684e1;#999580")
                                                    (api/keyword :dur) (api/string "3s")
                                                    (api/keyword :repeatCount) (api/string "indefinite")})])
                             (api/vector [(api/keyword :animate)
                                          (api/map {(api/keyword :attributeName) (api/string "r")
                                                    (api/keyword :values) (api/string "43;37;37;37;35")
                                                    (api/keyword :dur) (api/string "3s")
                                                    (api/keyword :repeatCount) (api/string "indefinite")})])]))

              (defncall 'animate-source '->
                (api/vector [(api/vector [(api/keyword :animate)
                                          (api/map {(api/keyword :attributeName) (api/string "stroke")
                                                    (api/keyword :values) (api/string "#999580;#6684e1;#6684e1;#6684e1;#999580")
                                                    (api/keyword :dur) (api/string "3s")
                                                    (api/keyword :repeatCount) (api/string "indefinite")})])
                             (api/vector [(api/keyword :animate)
                                          (api/map {(api/keyword :attributeName) (api/string "r")
                                                    (api/keyword :values) (api/string "35;37;37;37;43")
                                                    (api/keyword :dur) (api/string "3s")
                                                    (api/keyword :repeatCount) (api/string "indefinite")})])]))

              (defncall 'is-hovered-entry '->
                (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :item)
                                                            (api/vector [(api/key-fn :type) (api/key-fn :name)])])
                             (api/fn-call (api/symbol '->) [(api/key-fn :hover)
                                                            (api/key-fn :current)
                                                            (api/vector [(api/key-fn :type) (api/key-fn :name)])])])
                (api/fn-call (api/symbol '->) [(api/fn-call (api/symbol 'distinct) [(api/symbol '_)])
                                               (api/fn-call (api/symbol 'count) [(api/symbol '_)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/integer 1)])]))

              (defncall 'is-entry-sink '->
                (api/key-fn :item)
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "sink")]))

              (defncall 'get-animation-style '->
                (api/fn-call (api/symbol 'if) [(api/symbol 'is-hovered-entry)
                                               (api/fn-call (api/symbol 'if) [(api/symbol 'is-entry-sink)
                                                                              (api/symbol 'animate-sink)
                                                                              (api/symbol 'animate-source)])
                                               (api/string "")]))

              (defncall 'get-entry-bg '->
                (api/fn-call (api/symbol 'if) [(api/symbol 'is-hovered-entry)
                                               (api/keyword :menu-entry-active-bg)
                                               (api/keyword :menu-entry-bg)]))

              (defncall 'render-menu-entry '->
                (api/vector [(api/keyword :g)
                             (api/map {(api/keyword :transform) (api/symbol 'menu-transform)})
                             (api/vector [(api/keyword :circle)
                                          (api/map {(api/keyword :cx) (api/integer 0)
                                                    (api/keyword :cy) (api/integer 45)
                                                    (api/keyword :r) (api/integer 45)
                                                    (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :menu-entry-bg)
                                                                                                       (api/symbol 'get-color)])})])
                             ;; (api/fn-call (api/symbol '->) [(api/vector [(api/vector [(api/keyword :circle)
                             ;;                                                          (api/map {(api/keyword :cx) (api/integer 0)
                             ;;                                                                    (api/keyword :cy) (api/integer 45)
                             ;;                                                                    (api/keyword :r) (api/integer 40)
                             ;;                                                                    (api/keyword :stroke-width) (api/integer 2)
                             ;;                                                                    (api/keyword :stroke) (api/fn-call (api/symbol '->) [(api/keyword :element-highlight-stroke)
                             ;;                                                                                                                         (api/symbol 'get-color)])
                             ;;                                                                    (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/symbol 'get-entry-bg)
                             ;;                                                                                                                       (api/symbol 'get-color)])})])
                             ;;                                             (api/symbol 'get-animation-style)])
                             ;;                                (api/fn-call (api/symbol 'concat) [])])
                             (api/vector [(api/keyword :text)
                                          (api/map {(api/keyword :height) (api/integer 20)
                                                    (api/keyword :width) (api/string "100%")
                                                    (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :menu-entry-text) (api/symbol 'get-color)])
                                                    (api/keyword :text-anchor) (api/keyword :middle)
                                                    (api/keyword :x) (api/integer 0)
                                                    (api/keyword :y) (api/integer 35)
                                                    (api/keyword :dy) (api/integer 14)})
                                          (api/fn-call (api/symbol '->) [(api/key-fn :item)
                                                                         (api/key-fn :name)])])
                             (api/vector [(api/keyword :circle)
                                          (api/map {(api/keyword :id) (api/fn-call (api/symbol '->) [(api/key-fn :item)
                                                                                                     (api/key-fn :id)])
                                                    (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "all")})
                                                    (api/keyword :cx) (api/integer 0)
                                                    (api/keyword :cy) (api/integer 45)
                                                    (api/keyword :r) (api/integer 45)
                                                    (api/keyword :fill-opacity) (api/integer 0)
                                                    (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :menu-entry-bg)
                                                                                                       (api/symbol 'get-color)])})])]))

              (defncall 'get-menu-fill '->
                (api/keyword :menu-bar)
                (api/symbol 'config-color))

              (defncall 'tag-item-hover '->
                (api/map {(api/keyword :item) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])
                          (api/keyword :hover) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])}))

              (defncall 'render-source-menu '->
                (api/fn-call (api/symbol 'myzip) [(api/key-fn :items)
                                                (api/fn-call (api/symbol 'repeat) [(api/fn-call (api/symbol 'count) [(api/key-fn :items)])
                                                                                   (api/key-fn :hover)])])
                (api/fn-call (api/symbol 'map) [(api/symbol 'tag-item-hover) (api/symbol '_)])
                (api/fn-call (api/symbol 'map) [(api/symbol 'render-menu-entry) (api/symbol '_)])
                (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :g)]) (api/symbol '_)])
                (api/vector [(api/keyword :g)
                             (api/vector [(api/keyword :rect)
                                          (api/map {(api/keyword :id) (api/string "menu/source")
                                                    (api/keyword :height) (api/string "100%")
                                                    (api/keyword :width) (api/integer 100)
                                                    (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#shadow)")
                                                                                   (api/keyword :pointer-events) (api/string "all")})
                                                    (api/keyword :fill) (api/symbol 'get-menu-fill)})])
                             (api/symbol '_)])
                (api/map {(api/keyword :source-menu) (api/symbol '_)}))

              (defncall 'render-sink-menu '->
                (api/fn-call (api/symbol 'myzip) [(api/key-fn :items)
                                                (api/fn-call (api/symbol 'repeat) [(api/fn-call (api/symbol 'count) [(api/key-fn :items)])
                                                                                   (api/key-fn :hover)])])
                (api/fn-call (api/symbol 'map) [(api/symbol 'tag-item-hover) (api/symbol '_)])
                (api/fn-call (api/symbol 'map) [(api/symbol 'render-menu-entry) (api/symbol '_)])
                (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :g)]) (api/symbol '_)])
                (api/vector [(api/keyword :g)
                             (api/map {(api/keyword :transform) (api/string "translate(1100,0)")})
                             (api/vector [(api/keyword :rect)
                                          (api/map {(api/keyword :id) (api/string "menu/sink")
                                                    (api/keyword :height) (api/string "100%")
                                                    (api/keyword :width) (api/integer 100)
                                                    (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#leftshadow)")
                                                                                   (api/keyword :pointer-events) (api/string "all")})
                                                    (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :menu-bar)
                                                                                                       (api/symbol 'get-color)])})])
                             (api/symbol '_)])
                (api/map {(api/keyword :sink-menu) (api/symbol '_)}))

              (defncall 'get-menu-state '->
                (api/vector [(api/string "mode: ") (api/fn-call (api/symbol '->) [(api/key-fn :editor)
                                                                                  (api/key-fn :mode)])])
                (api/fn-call (api/symbol 'str-join) [(api/string " ") (api/symbol '_)]))

              (defncall 'render-menu-action '->
                ;; (api/fn-call (api/symbol 'spy) [(api/string "render action")])
                (api/symbol '_))

              (defncall 'get-menu-actions '->
                (api/fn-call (api/symbol '->) [(api/key-fn :mode)
                                               (api/key-fn :actions)
                                               (api/fn-call (api/symbol 'map) [(api/symbol 'render-menu-action) (api/symbol '_)])])
                (api/fn-call (api/symbol 'str-join) [(api/string ",") (api/symbol '_)])
                (api/vector [(api/string "type: ") (api/symbol '_)])
                (api/fn-call (api/symbol 'str-join) [(api/string "") (api/symbol '_)]))

              (defncall 'render-action-menu '->
                (api/map {(api/keyword :action-menu)
                          (api/vector [(api/keyword :g)
                                       (api/map {(api/keyword :transform) (api/string "translate(0,700)")})
                                       (api/vector [(api/keyword :rect)
                                                    (api/map {(api/keyword :id) (api/string "menu/action")
                                                              (api/keyword :height) (api/integer 100)
                                                              (api/keyword :width) (api/string "100%")
                                                              (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#upshadow)")
                                                                                             (api/keyword :pointer-events) (api/string "all")})
                                                              (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :menu-bar)
                                                                                                                 (api/symbol 'get-color)])})])
                                       (api/vector [(api/keyword :text)
                                                    (api/map {(api/keyword :height) (api/integer 20)
                                                              (api/keyword :width) (api/string "100%")
                                                              (api/keyword :text-anchor) (api/keyword :middle)
                                                              (api/keyword :x) (api/integer 600)
                                                              (api/keyword :y) (api/integer 5)
                                                              (api/keyword :dy) (api/integer 14)})
                                                    (api/fn-call (api/symbol '->)
                                                                 [(api/vector [(api/string "state: ")
                                                                               (api/symbol 'get-menu-state)])
                                                                  (api/fn-call (api/symbol 'str-join) [(api/string "") (api/symbol '_)])])])
                                       (api/vector [(api/keyword :text)
                                                    (api/map {(api/keyword :height) (api/integer 20)
                                                              (api/keyword :width) (api/string "100%")
                                                              (api/keyword :text-anchor) (api/keyword :middle)
                                                              (api/keyword :x) (api/integer 600)
                                                              (api/keyword :y) (api/integer 25)
                                                              (api/keyword :dy) (api/integer 14)})
                                                    (api/fn-call (api/symbol '->)
                                                                 [(api/vector [(api/string "actions: ")
                                                                               (api/symbol 'get-menu-actions)])
                                                                  (api/fn-call (api/symbol 'str-join) [(api/string "") (api/symbol '_)])])])])}))

              (defncall 'source-menu 'pipes/debug)
              (defncall 'source-menu-items 'pipes/debug)
              (defncall 'source-menu-events 'pipes/debug)
              (defncall 'source-menu-state 'pipes/debug)


              (defncall 'source-menu-const '->
                (api/vector [(api/string "debug")
                             (api/string "ui")
                             (api/string "http")])
                (api/symbol 'many))

              (defncall 'sink-menu 'pipes/debug)
              (defncall 'sink-menu-items 'pipes/debug)
              (defncall 'sink-menu-events 'pipes/debug)
              (defncall 'sink-menu-state 'pipes/debug)

              (defncall 'sink-menu-const '->
                (api/vector [(api/string "debug")
                             (api/string "log")
                             (api/string "ui")
                             (api/string "http")])
                (api/symbol 'many))

              (defncall 'map-menu-item '->
                (api/map {(api/keyword :position)
                          (api/key-fn :count)
                          (api/keyword :name)
                          (api/key-fn :name)
                          (api/keyword :type)
                          (api/key-fn :type)
                          (api/keyword :id)
                          (api/fn-call (api/symbol 'str) [(api/key-fn :type)
                                                          (api/string "/")
                                                          (api/key-fn :name)])}))

              (defncall 'source-menu-map 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state)
                                           (api/fn-call (api/symbol '->) [(api/map {(api/keyword :count)
                                                                                    (api/fn-call (api/symbol 'count) [(api/key-fn :state)])
                                                                                    (api/keyword :type)
                                                                                    (api/string "source")
                                                                                    (api/keyword :name)
                                                                                    (api/key-fn :next)})
                                                                          (api/symbol 'map-menu-item)])])
                              (api/fn-call (api/symbol 'flatten) [(api/symbol '_)])])
                (api/vector []))

              (defncall 'sink-menu-map 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state)
                                           (api/fn-call (api/symbol '->) [(api/map {(api/keyword :count)
                                                                                    (api/fn-call (api/symbol 'count) [(api/key-fn :state)])
                                                                                    (api/keyword :type)
                                                                                    (api/string "sink")
                                                                                    (api/keyword :name)
                                                                                    (api/key-fn :next)})
                                                                          (api/symbol 'map-menu-item)])])
                              (api/fn-call (api/symbol 'flatten) [(api/symbol '_)])])
                (api/vector []))

              (defncall 'tag-items '->
                (api/map {(api/keyword :items) (api/symbol '_)}))

              (defncall 'reduce-menu-source 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
                (api/map {(api/keyword :items) (api/vector [])
                          (api/keyword :hover) (api/map {})}))

              (defncall 'reduce-menu-sink 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
                (api/map {(api/keyword :items) (api/vector [])
                          (api/keyword :hover) (api/map {})}))

              (defncall 'is-mouse-move '->
                (api/key-fn :next)
                (api/key-fn :samak.mouse/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :move)]))

              (defncall 'calculate-mouse-delta-x '->
                (api/fn-call (api/symbol '+) [(api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                             (api/key-fn :samak.mouse/page-x)])
                                              (api/fn-call (api/symbol '-) [(api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                                                                           (api/key-fn :mouse)
                                                                                                           (api/key-fn :position)
                                                                                                           (api/key-fn :x)
                                                                                                           ])])]))

              (defncall 'calculate-mouse-delta-y '->
                (api/fn-call (api/symbol '+) [(api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                             (api/key-fn :samak.mouse/page-y)])
                                              (api/fn-call (api/symbol '-) [(api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                                                                           (api/key-fn :mouse)
                                                                                                           (api/key-fn :position)
                                                                                                           (api/key-fn :y)
                                                                                                           ])])]))

              (defncall 'handle-mouse-move '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :position)
                                                        (api/map {(api/keyword :x) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :samak.mouse/page-x)])
                                                                  (api/keyword :y) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :samak.mouse/page-y)])})
                                                        (api/keyword :delta)
                                                        (api/map {(api/keyword :x)
                                                                  (api/symbol 'calculate-mouse-delta-x)
                                                                  (api/keyword :y)
                                                                  (api/symbol 'calculate-mouse-delta-y)})
                                                        (api/keyword :drag)
                                                        (api/fn-call (api/symbol '->) [(api/key-fn :state) (api/key-fn :mouse) (api/key-fn :drag)
                                                                                       (api/map {(api/keyword :active) (api/key-fn :active)
                                                                                                 (api/keyword :source) (api/key-fn :source)
                                                                                                 (api/keyword :button) (api/key-fn :button)})])})}))

              (defncall 'is-mouse-down '->
                (api/key-fn :next)
                (api/key-fn :samak.mouse/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :down)]))

              (defncall 'handle-mouse-down '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/fn-call (api/symbol '->)
                                                           [(api/key-fn :next)
                                                            (api/map {(api/keyword :drag) (api/map {(api/keyword :active) (api/keyword :true)
                                                                                                    (api/keyword :button) (api/key-fn :samak.mouse/button)
                                                                                                    (api/keyword :source) (api/key-fn :samak.mouse/target)})

                                                                      (api/keyword :start) (api/map {(api/keyword :x) (api/key-fn :samak.mouse/page-x)
                                                                                                     (api/keyword :y) (api/key-fn :samak.mouse/page-y)})})])}))

              (defncall 'is-mouse-up '->
                (api/key-fn :next)
                (api/key-fn :samak.mouse/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :up)]))

              (defncall 'handle-mouse-up '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :drag)
                                                        (api/map {(api/keyword :active) (api/keyword :false)
                                                                  (api/keyword :end) (api/keyword :end)
                                                                  (api/keyword :button) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :samak.mouse/button)])
                                                                  (api/keyword :target) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :samak.mouse/target)])
                                                                  (api/keyword :source) (api/fn-call (api/symbol '->) [(api/key-fn :state) (api/key-fn :mouse) (api/key-fn :drag) (api/key-fn :source)])})})}))

              (defncall 'mouse-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/fn-call (api/symbol 'incase) [(api/symbol 'is-mouse-move)
                                                                 (api/symbol 'handle-mouse-move)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mouse-down)
                                                                 (api/symbol 'handle-mouse-down)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mouse-up)
                                                                 (api/symbol 'handle-mouse-up)])
                              (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                                          (api/key-fn :mouse)])
                                           (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])
                              (api/map {(api/keyword :mouse) (api/symbol '_)})])
                (api/map {(api/keyword :mouse) (api/map {})}))

              (defncall 'mouse-state 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                )

              (defncall 'target-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/map {(api/keyword :prev) (api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                                                           (api/key-fn :current)])
                                        (api/keyword :current) (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                                              (api/key-fn :samak.mouse/target)
                                                                                              (api/symbol 'make-target)])})])
                (api/map {(api/keyword :current) (api/map {(api/keyword :type) (api/string "none")
                                                           (api/keyword :name) (api/string "none")})
                          (api/keyword :prev) (api/map {(api/keyword :type) (api/string "none")
                                                        (api/keyword :name) (api/string "none")})}))

              (defncall 'only-different '->
                (api/fn-call (api/symbol 'except) [(api/fn-call (api/symbol '->) [(api/vector [(api/key-fn :prev) (api/key-fn :current)])
                                                                                  (api/fn-call (api/symbol 'distinct) [(api/symbol '_)])
                                                                                  (api/fn-call (api/symbol 'count) [(api/symbol '_)])
                                                                                  (api/fn-call (api/symbol '=) [(api/symbol '_) (api/integer 1)])])]))

              (defncall 'tag-hover '->
                (api/map {(api/keyword :hover) (api/symbol '_)}))

              (defncall 'target-events 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                )
              (defncall 'hover-events 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                )
              (defncall 'hover-state 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                )

              (defncall 'is-drag '->
                (api/key-fn :mouse)
                (api/key-fn :drag)
                (api/key-fn :active)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :true)]))

              (defncall 'is-drag-end '->
                (api/key-fn :mouse)
                (api/key-fn :drag)
                (api/key-fn :end)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :end)]))

              (defncall 'is-drag-or-end 'or
                (api/symbol 'is-drag)
                (api/symbol 'is-drag-end))

              (defncall 'filter-drag-end 'only (api/symbol 'is-drag-end))
              (defncall 'filter-drag 'only (api/symbol 'is-drag-or-end))


              (defncall 'drag-events 'pipes/debug)
              (defncall 'drag-state 'pipes/debug)

              ;; (defncall 'drag-reduce 'pipes/reductions
              ;;   (api/fn-call (api/symbol '->)
              ;;                [(api/vector [(api/key-fn :state) (api/key-fn :next)])
              ;;                 (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]) ])
              ;;   (api/map {}))


              ;; keyboard handling

              (defncall 'is-target-input '->
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "INPUT")]))

              (defncall 'is-key-common '->
                (api/key-fn :which)
                (api/fn-call (api/symbol '>) [(api/integer 31) (api/symbol '_)]))

              (defncall 'keyboard-filtered 'pipes/debug)
              (defncall 'filter-key-input 'if
                (api/fn-call (api/symbol 'and) [(api/symbol 'is-target-input)
                                                (api/symbol 'is-key-common)])
                (api/symbol 'ignore)
                (api/symbol '_))

              (defncall 'is-kb-mark-up '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "w")]))

              (defncall 'is-kb-mark-down '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "s")]))

              (defncall 'is-kb-indent '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "d")]))

              (defncall 'is-kb-dedent '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "a")]))

              (defncall 'is-kb-swap-up '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "W")]))

              (defncall 'is-kb-swap-down '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "S")]))

              (defncall 'is-kb-insert '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "f")]))

              (defncall 'is-kb-edit '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "e")]))

              (defncall 'is-kb-cut '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "x")]))

              (defncall 'is-kb-back '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "q")]))

              ;; future use

              (defncall 'is-kb-rename '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "r")]))

              (defncall 'is-kb-copy '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "c")]))

              (defncall 'is-kb-paste '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "v")]))

              (defncall 'is-kb-comment '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "<")]))

              (defncall 'is-kb-insert-go '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "g")]))

              ;; generic actions

              (defncall 'is-kb-first '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "1")]))

              (defncall 'is-kb-second '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "2")]))

              (defncall 'is-kb-third '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "3")]))

              (defncall 'is-kb-fourth '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "4")]))

              (defncall 'is-kb-fifth '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "5")]))

              (defncall 'is-kb-sixth '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "6")]))

              (defncall 'is-kb-seventh '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "7")]))

              (defncall 'is-kb-eigth '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "8")]))

              (defncall 'is-kb-ninth '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "9")]))

              (defncall 'is-kb-accept '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "Enter")]))

              (defncall 'is-kb-cancel '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "Escape")]))

              (defncall 'construct-cursor '->
                (api/map {(api/keyword :command) (api/keyword :cursor)
                          (api/keyword :data) (api/symbol '_)}))

              (defncall 'construct-up '->
                (api/keyword :up)
                (api/symbol 'construct-cursor))

              (defncall 'construct-down '->
                (api/keyword :down)
                (api/symbol 'construct-cursor))


              (defncall 'construct-mode '->
                (api/map {(api/keyword :command) (api/keyword :mode)
                          (api/keyword :data) (api/symbol '_)}))

              (defncall 'construct-insert-mode '->
                (api/keyword :insert)
                (api/symbol 'construct-mode))

              (defncall 'construct-edit-mode '->
                (api/keyword :edit)
                (api/symbol 'construct-mode))

              (defncall 'construct-action '->
                (api/map {(api/keyword :command) (api/keyword :action)
                          (api/keyword :type) (api/keyword :immediate)
                          (api/keyword :data) (api/symbol '_)}))

              (defncall 'construct-indent '->
                (api/keyword :indent)
                (api/symbol 'construct-action))

              (defncall 'construct-dedent '->
                (api/keyword :dedent)
                (api/symbol 'construct-action))

              (defncall 'construct-rename '->
                (api/keyword :rename)
                (api/symbol 'construct-action))

              (defncall 'construct-cut '->
                (api/keyword :cut)
                (api/symbol 'construct-action))

              (defncall 'construct-first '->
                (api/keyword :first)
                (api/symbol 'construct-action))

              (defncall 'construct-second '->
                (api/keyword :second)
                (api/symbol 'construct-action))

              (defncall 'construct-third '->
                (api/keyword :third)
                (api/symbol 'construct-action))

              (defncall 'construct-fourth '->
                (api/keyword :fourth)
                (api/symbol 'construct-action))

              (defncall 'construct-fifth '->
                (api/keyword :fifth)
                (api/symbol 'construct-action))

              (defncall 'construct-sixth '->
                (api/keyword :sixth)
                (api/symbol 'construct-action))

              (defncall 'construct-seventh '->
                (api/keyword :seventh)
                (api/symbol 'construct-action))

              (defncall 'construct-eigth '->
                (api/keyword :eigth)
                (api/symbol 'construct-action))

              (defncall 'construct-ninth '->
                (api/keyword :ninth)
                (api/symbol 'construct-action))

              (defncall 'construct-accept '->
                (api/keyword :accept)
                (api/symbol 'construct-action))

              (defncall 'construct-cancel '->
                (api/keyword :cancel)
                (api/symbol 'construct-action))

              (defncall 'construct-back '->
                (api/map {(api/keyword :command) (api/keyword :mode)
                          (api/keyword :data) (api/keyword :back)}))

              (defncall 'construct-leap '->
                (api/keyword :leap)
                (api/symbol 'construct-action))

              (defncall 'construct-fall '->
                (api/keyword :fall)
                (api/symbol 'construct-action))

              (defncall 'filter-edit '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-mark-down)
                                                   (api/symbol 'construct-down)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-mark-up)
                                                   (api/symbol 'construct-up)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-indent)
                                                   (api/symbol 'construct-indent)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-dedent)
                                                   (api/symbol 'construct-dedent)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-swap-down)
                                                   (api/symbol 'construct-fall)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-swap-up)
                                                   (api/symbol 'construct-leap)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-insert)
                                                   (api/symbol 'construct-insert-mode)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-edit)
                                                   (api/symbol 'construct-edit-mode)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-cut)
                                                   (api/symbol 'construct-cut)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-rename)
                                                   (api/symbol 'construct-rename)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-first)
                                                   (api/symbol 'construct-first)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-second)
                                                   (api/symbol 'construct-second)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-third)
                                                   (api/symbol 'construct-third)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-fourth)
                                                   (api/symbol 'construct-fourth)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-fifth)
                                                   (api/symbol 'construct-fifth)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-sixth)
                                                   (api/symbol 'construct-sixth)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-seventh)
                                                   (api/symbol 'construct-seventh)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-eigth)
                                                   (api/symbol 'construct-eigth)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-ninth)
                                                   (api/symbol 'construct-ninth)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-accept)
                                                   (api/symbol 'construct-accept)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-cancel)
                                                   (api/symbol 'construct-cancel)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-back)
                                                   (api/symbol 'construct-back)])
                (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                   (api/symbol 'ignore)]))
              (defncall 'is-kb-menu '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "^")]))

              (defncall 'construct-menu '->
                (api/map {(api/keyword :command) (api/keyword :menu)
                          (api/keyword :data) (api/keyword :none)}))

              (defncall 'is-kb-load '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "l")]))

              (defncall 'construct-load '->
                (api/map {(api/keyword :command) (api/keyword :load)
                          (api/keyword :data) (api/keyword :none)}))

              (defncall 'filter-menu '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-menu)
                                                   (api/symbol 'construct-menu)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-load)
                                                   (api/symbol 'construct-load)])
                (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                   (api/symbol 'ignore)]))

              (defncall 'is-kb-zoom-in '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "+")]))

              (defncall 'is-kb-zoom-out '->
                (api/key-fn :key)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "-")]))

              (defncall 'construct-zoom-out '->
                (api/map {(api/keyword :command) (api/keyword :zoom)
                          (api/keyword :data) (api/keyword 0.5)}))

              (defncall 'construct-zoom-in '->
                (api/map {(api/keyword :command) (api/keyword :zoom)
                          (api/keyword :data) (api/integer 1)}))

              (defncall 'filter-view '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-zoom-in)
                                                   (api/symbol 'construct-zoom-in)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-zoom-out)
                                                   (api/symbol 'construct-zoom-out)])
                (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                   (api/symbol 'ignore)]))

              (defncall 'make-zoom '->
                (api/map {(api/keyword :zoom) (api/key-fn :data)}))

              (defncall 'is-command '->
                (api/key-fn :command))


              (defncall 'handle-sink '->
                (api/key-fn :name)
                (api/map {(api/keyword :command) (api/keyword :create-sink)
                          (api/keyword :data) (api/map {(api/keyword :name) (api/symbol '_)})}))

              (defncall 'handle-source '->
                (api/key-fn :name)
                (api/map {(api/keyword :command) (api/keyword :create-sink)
                          (api/keyword :data) (api/map {(api/keyword :name) (api/symbol '_)})}))

              (defncall 'is-sink '->
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "sink")]))

              (defncall 'is-source '->
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "source")]))

              (defncall 'handle-mouse-click '->
                (api/key-fn :source)
                (api/symbol 'make-target)
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-source)
                                                   (api/symbol 'handle-source)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-sink)
                                                   (api/symbol 'handle-sink)])
                (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                   (api/symbol 'ignore)]))

              (defncall 'is-lmb-event '->
                (api/key-fn :button)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :primary)]))

              (defncall 'is-mouse-click '->
                (api/vector [(api/key-fn :source) (api/key-fn :target)])
                (api/fn-call (api/symbol 'distinct) [(api/symbol '_)])
                (api/fn-call (api/symbol 'count) [(api/symbol '_)])
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/integer 1)]))

              (defncall 'is-lmb-click '->
                (api/fn-call (api/symbol 'and) [(api/symbol 'is-lmb-event)
                                                (api/symbol 'is-mouse-click)]))

              (defncall 'get-pipe-name '->
                (api/fn-call (api/symbol 'str-split) [(api/symbol '_) (api/string "/")])
                (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)]))

              (defncall 'handle-mouse-connect '->
                (api/map {(api/keyword :command) (api/keyword :connect)
                          (api/keyword :data) (api/map {(api/keyword :source) (api/fn-call (api/symbol '->) [(api/key-fn :source) (api/symbol 'get-pipe-name)])
                                                        (api/keyword :sink) (api/fn-call (api/symbol '->) [(api/key-fn :target) (api/symbol 'get-pipe-name)])})}))

              (defncall 'is-pipe '->
                (api/fn-call (api/symbol 'str-index) [(api/string "pipe/") (api/symbol '_)])
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/integer 0)]))

              (defncall 'is-source-source '->
                (api/key-fn :source)
                (api/symbol 'is-pipe))

              (defncall 'is-target-sink '->
                (api/key-fn :target)
                (api/symbol 'is-pipe))

              (defncall 'is-different-pipe '->
                (api/vector [(api/key-fn :target) (api/key-fn :source)])
                (api/fn-call (api/symbol 'distinct) [(api/symbol '_)])
                (api/fn-call (api/symbol 'count) [(api/symbol '_)])
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/integer 2)]))

              (defncall 'both-pipe '->
                (api/fn-call (api/symbol 'and) [(api/symbol 'is-source-source)
                                                (api/symbol 'is-target-sink)]))

              (defncall 'interpret-drag '->
                (api/key-fn :mouse)
                (api/key-fn :drag)
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-lmb-click)
                                                   (api/symbol 'handle-mouse-click)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'both-pipe)
                                                   (api/symbol 'handle-mouse-connect)])
                (api/fn-call (api/symbol 'unless) [(api/symbol 'is-command)
                                                   (api/map {(api/keyword :command) (api/keyword :noop)})]))

              (defncall 'is-scroll '->
                (api/key-fn :mouse)
                (api/key-fn :drag)
                (api/key-fn :button)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :secondary)]))

              (defncall 'filter-scroll 'only (api/fn-call (api/symbol 'and) [(api/symbol 'is-scroll)
                                                                             (api/symbol 'is-drag)]))

              (defncall 'scroll-state 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                )

              (defncall 'construct-view '->
                (api/key-fn :mouse)
                (api/map {(api/keyword :x) (api/fn-call (api/symbol '->) [(api/key-fn :delta)
                                                                          (api/key-fn :x)])
                          (api/keyword :y) (api/fn-call (api/symbol '->) [(api/key-fn :delta)
                                                                          (api/key-fn :y)])}))

              (defncall 'center-view 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/map {(api/keyword :x) (api/integer 150)
                                        (api/keyword :y) (api/integer 50)})])
                (api/map {}))

              (defncall 'view-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/fn-call (api/symbol 'into) [(api/key-fn :state) (api/key-fn :next)])])
                (api/map {(api/keyword :zoom) (api/integer 1)
                          (api/keyword :x) (api/integer 150)
                          (api/keyword :y) (api/integer 50)}))

              (defncall 'tag-view '->
                (api/map {(api/keyword :view) (api/symbol '_)}))


              (defncall 'view-commands 'pipes/debug)
              (defncall 'view-events 'pipes/debug)

              (defncall 'view-delta 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/map {(api/keyword :x)
                                        (api/fn-call (api/symbol '+) [(api/fn-call (api/symbol '->)
                                                                                   [(api/key-fn :next)
                                                                                    (api/key-fn :x)])
                                                                      (api/fn-call (api/symbol '->)
                                                                                   [(api/key-fn :state)
                                                                                    (api/key-fn :x)])])
                                        (api/keyword :y)
                                        (api/fn-call (api/symbol '+) [(api/fn-call (api/symbol '->)
                                                                                   [(api/key-fn :next)
                                                                                    (api/key-fn :y)])
                                                                      (api/fn-call (api/symbol '->)
                                                                                   [(api/key-fn :state)
                                                                                    (api/key-fn :y)])])}
                                       )])
                (api/map {(api/keyword :x) (api/integer 150)
                          (api/keyword :y) (api/integer 50)}))


              (defncall 'view-raw 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                )
              (defncall 'view-state 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                )
              (defncall 'view-deltas 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                )

              ;; keep evaluations in state reduction

              (defncall 'is-node-func '->
                (api/key-fn :caravan/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/func)]))

              (defncall 'is-node-sink '->
                (api/key-fn :caravan/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/sink)]))

              (defncall 'is-def 'or
                (api/symbol 'is-node-func)
                (api/symbol 'is-node-sink))

              (defncall 'eval-id '->
                (api/fn-call (api/symbol 'if) [(api/symbol 'is-def)
                                               (api/key-fn :caravan/name)
                                               (api/symbol '_)])) ;;FIXME

              (defncall 'index-eval '->
                (api/vector [(api/vector [(api/symbol 'eval-id) (api/symbol '_)])])
                (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]))

              (defncall 'eval-events 'pipes/debug ;; (api/keyword :oasis.spec/eval-state)
                )

              (defncall 'eval-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
                (api/map {}))

              (defncall 'eval-raw 'pipes/debug ;; (api/keyword :oasis.spec/eval-state)
                )

              (defncall 'tag-eval '->
                (api/map {(api/keyword :eval) (api/symbol '_)}))

              (defncall 'eval-state 'pipes/debug ;; (api/keyword :oasis.spec/eval-state)
                )

              ;; commands

              (defncall 'editor-commands 'pipes/debug)
              (defncall 'editor-events 'pipes/debug)
              (defncall 'editor-cooked 'pipes/debug)
              (defncall 'editor-immediate 'pipes/debug)
              (defncall 'editor-actions 'pipes/debug)
              (defncall 'editor-state 'pipes/debug)
              (defncall 'select-events 'pipes/debug)

              (defncall 'be-commands 'pipes/debug)
              (defncall 'caravan 'pipes/caravan)

              (defncall 'filter-call 'only (api/key-fn :call))

              (defncall 'mode-state 'pipes/debug)
              (defncall 'mode-raw 'pipes/debug)

              (defncall 'is-immediate-command '->
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :immediate)]))
              (defncall 'filter-immediate 'only (api/symbol 'is-immediate-command))


              (defncall 'tag-editor '->
                (api/map {(api/keyword :editor) (api/symbol '_)}))

              (defncall 'is-create-sink '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :create-sink)]))

              (defncall 'handle-create-sink '->
                (api/key-fn :data)
                (api/fn-call (api/symbol 'create-sink) [])
                (api/map {(api/keyword :result) (api/symbol '_)}))

              (defncall 'is-connect '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :connect)]))

              (defncall 'handle-connect '->
                (api/key-fn :data)
                (api/fn-call (api/symbol 'connect) [])
                (api/map {(api/keyword :result) (api/symbol '_)}))

              (defncall 'is-select '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :select)]))

              (defncall 'handle-select '->
                (api/key-fn :data)
                (api/map {(api/keyword :selected)
                          (api/fn-call (api/symbol '->) [(api/key-fn :target)
                                                         (api/key-fn :id)])}))

              (defncall 'is-insert '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :insert)]))

              (defncall 'handle-insert '->
                (api/key-fn :data)
                (api/map {(api/keyword :inserted) (api/keyword :none)}))

              (defncall 'is-cursor '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :cursor)]))

              (defncall 'handle-cursor '->
                (api/key-fn :data)
                (api/map {(api/keyword :cursor)
                          (api/symbol '_)}))

              (defncall 'is-menu '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :menu)]))

              (defncall 'handle-menu '->
                (api/key-fn :data)
                (api/map {(api/keyword :menu) (api/keyword :none)}))

              (defncall 'is-load '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :load)]))

              (defncall 'handle-load '->
                (api/key-fn :data)
                (api/fn-call (api/symbol 'spy) [(api/string "load")])
                (api/symbol 'load-node)
                (api/map {(api/keyword :load) (api/keyword :none)}))

              (defncall 'is-mode-change '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :mode)]))

              (defncall 'handle-mode '->
                (api/key-fn :data)
                (api/map {(api/keyword :mode) (api/symbol '_)}))


              (defncall 'handle-commands '->
                (api/fn-call (api/symbol 'spy) [(api/string "comm")])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-create-sink)
                                                   (api/symbol 'handle-create-sink)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-connect)
                                                   (api/symbol 'handle-connect)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-cursor)
                                                   (api/symbol 'handle-cursor)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-select)
                                                   (api/symbol 'handle-select)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-menu)
                                                   (api/symbol 'handle-menu)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-load)
                                                   (api/symbol 'handle-load)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mode-change)
                                                   (api/symbol 'handle-mode)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mode-change)
                                                   (api/symbol 'handle-mode)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-command)
                                                   (api/symbol 'ignore)]))

              (defncall 'is-next-cursor '->
                (api/key-fn :next)
                (api/key-fn :cursor))

              (defncall 'get-fn-size '->
                (api/key-fn :ast)
                (api/key-fn :caravan/ast)
                (api/fn-call (api/symbol 'count) [(api/symbol '_)]))

              (defncall 'calc-down '->
                (api/key-fn :state)
                (api/fn-call (api/symbol 'min) [(api/fn-call (api/symbol '->) [(api/key-fn :mark)
                                                                               (api/fn-call (api/symbol 'inc) [(api/symbol '_)])])
                                                (api/symbol 'get-fn-size)]))

              (defncall 'calc-up '->
                (api/key-fn :state)
                (api/key-fn :mark)
                (api/fn-call (api/symbol 'max) [(api/fn-call (api/symbol 'dec) [(api/symbol '_)])
                                                (api/integer 1)]))

              (defncall 'calculate-mark '->
                (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol '->) [(api/symbol 'is-next-cursor)
                                                                              (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :down)])])
                                               (api/symbol 'calc-down)
                                               (api/symbol 'calc-up)]))

              (defncall 'change-mark '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :mark)
                                                        (api/symbol 'calculate-mark)})}))
              (defncall 'is-mode-navigate '->
                (api/key-fn :state)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :navigate)]))

              (defncall 'is-mode-insert '->
                (api/key-fn :state)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :insert)]))

              (defncall 'process-cursor '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mode-navigate)
                                                   (api/symbol 'change-mark)])
                ;; (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mode-insert)
                ;;                                    (api/symbol 'change-mark)])
                )

              (defncall 'selected-source '->
                (api/map {(api/keyword :key) (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                            (api/key-fn :selected)
                                                                            (api/symbol 'fn-name-from-select)])
                          (api/keyword :map) (api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                                            (api/key-fn :eval)])})
                (api/fn-call (api/symbol 'lookup) [(api/key-fn :map) (api/key-fn :key) (api/keyword :none)]))


              (defncall 'process-select '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :mark) (api/integer 1)
                                                        (api/keyword :mode) (api/keyword :navigate)
                                                        (api/keyword :selected) (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                                                               (api/key-fn :selected)])
                                                        (api/keyword :ast) (api/symbol 'selected-source)})}))

               (defncall 'selected-source-change '->
                 (api/fn-call (api/symbol 'lookup) [(api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                                   (api/key-fn :eval)])
                                                    (api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                                                (api/key-fn :selected)
                                                                                   (api/symbol 'fn-name-from-select)])
                                                    (api/keyword :none)]))


               (defncall 'process-eval '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/map {(api/keyword :eval) (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                                                            (api/key-fn :eval)])
                                                         (api/keyword :ast) (api/symbol 'selected-source-change)})}))

              (defncall 'is-next-mode '->
                (api/key-fn :next)
                (api/key-fn :mode))

              (defncall 'mode-set-navigate '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :mode) (api/keyword :navigate)})}))

              (defncall 'is-next-back '->
                (api/key-fn :next)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :back)]))

              (defncall 'is-next-edit '->
                (api/key-fn :next)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :edit)]))


              (defncall 'mode-set-back '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mode-insert)
                                                   (api/symbol 'mode-set-navigate)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-back)
                                                   (api/symbol 'mode-set-navigate)]))

              (defncall 'is-cell-editable '->
                (api/key-fn :state)
                (api/fn-call (api/symbol 'lookup)  [(api/fn-call (api/symbol '->) [(api/key-fn :ast)
                                                                                (api/key-fn :caravan/ast)])
                                                 (api/fn-call (api/symbol '->) [(api/key-fn :mark)
                                                                                (api/symbol 'dec)])
                                                    (api/keyword :none)])
                (api/key-fn :type)
                (api/fn-call (api/symbol 'or) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/str)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/kw)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/acc)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/int)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/float)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/func)])])
                )

              (defncall 'mode-set-edit '->
                (api/fn-call (api/symbol 'unless) [(api/symbol 'is-cell-editable)
                                                   (api/symbol 'mode-set-navigate)]))

              (defncall 'process-mode '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-edit)
                                                   (api/symbol 'mode-set-edit)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-back)
                                                   (api/symbol 'mode-set-back)]))

              (defncall 'editor-state-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-mode)
                                                                 (api/symbol 'process-mode)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-cursor)
                                                                 (api/symbol 'process-cursor)])
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :selected)])
                                                                 (api/symbol 'process-select)])
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :eval)])
                                                                 (api/symbol 'process-eval)])
                              (api/vector [(api/key-fn :state)
                                           (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
                (api/map {(api/keyword :mode) (api/keyword :navigate)
                          (api/keyword :mark) (api/integer 1)
                          (api/keyword :eval) (api/map {})
                          (api/keyword :ast) (api/map {})
                          (api/keyword :hover) (api/vector [])}))

              ;; global state

              (defncall 'state-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]) ])
                (api/map {}))

              (defncall 'state 'pipes/debug ;; (api/keyword :oasis.spec/state)
                )

              (defncall 'tag-layout '->
                (api/map {(api/keyword :layout) (api/symbol '_)}))

              (defncall 'layout-state 'pipes/debug ;; (api/keyword :oasis.spec/state)
                )


              ;; convert and layout nodes

              (defncall 'def-name 'str
                (api/string "d/")
                (api/key-fn :caravan/name))


              ;; cell handling

              (defncall 'func-id 'str
                (api/string "func/")
                (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :id)]))


              (defncall 'is-selected '->
                (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :selected)])
                             (api/symbol 'func-id)])
                (api/fn-call (api/symbol 'distinct) [(api/symbol '_)])
                (api/fn-call (api/symbol 'count) [(api/symbol '_)])
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/integer 1)]))


              (defncall 'cell-y '->
                (api/fn-call (api/symbol '*) [(api/integer 20)
                                              (api/key-fn :counter)])
                (api/fn-call (api/symbol '+) [(api/integer 10)
                                              (api/symbol '_)]))

              (defncall 'cell-x '->
                (api/fn-call (api/symbol '*) [(api/integer 15)
                                              (api/key-fn :level)]))

              (defncall 'cell-pos '->
                (api/map {(api/keyword :x)
                          (api/integer 0)
                          (api/keyword :y)
                          (api/symbol 'cell-y)})
                (api/symbol 'translate-str))

              (defncall 'line-pos '->
                (api/map {(api/keyword :x)
                          (api/integer 0)
                          (api/keyword :y)
                          (api/integer -15)})
                (api/symbol 'translate-str))


              (defncall 'text-pos '->
                (api/map {(api/keyword :x)
                          (api/symbol 'cell-x)
                          (api/keyword :y)
                          (api/integer 0)})
                (api/symbol 'translate-str))

              (defncall 'counter-pos '->
                (api/map {(api/keyword :x)
                          (api/integer 10)
                          (api/keyword :y)
                          (api/integer 0)})
                (api/symbol 'translate-str))

              (defncall 'type-pos '->
                (api/map {(api/keyword :x)
                          (api/fn-call (api/symbol '+)  [(api/integer 190)
                                                         ;; (api/symbol 'cell-x)
                                                         (api/integer 10)])
                          (api/keyword :y)
                          (api/integer 0)})
                (api/symbol 'translate-str))

              (defncall 'is-marked-cell '->
                (api/fn-call (api/symbol '-) [(api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                            (api/key-fn :mark)])
                                              (api/fn-call (api/symbol '->) [(api/key-fn :exp)
                                                                            (api/key-fn :counter)])])
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/integer 0)]))

              (defncall 'is-active-cell '->
                (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                               (api/key-fn :selected)])
                                                (api/symbol 'is-marked-cell)]))

              (defncall 'is-edited-cell '->
                (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                               (api/key-fn :mode)
                                                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :edit)])])
                                                (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                               (api/key-fn :selected)])
                                                (api/symbol 'is-marked-cell)]))

              (defncall 'is-active-branch '->
                (api/key-fn :exp)
                (api/key-fn :counter)
                (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '>) [(api/symbol '_) (api/integer 100)])
                                                (api/fn-call (api/symbol '<) [(api/symbol '_) (api/integer 101)])]))

              (defncall 'get-fill '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-edited-cell)
                                                   (api/fn-call (api/symbol '->) [(api/keyword :cell-edit)
                                                                                  (api/symbol 'get-color)])])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-active-cell)
                                                   (api/fn-call (api/symbol '->) [(api/keyword :cell-active)
                                                                                  (api/symbol 'get-color)])])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-active-branch)
                                                   (api/fn-call (api/symbol '->) [(api/keyword :cell-seclight)
                                                                                  (api/symbol 'get-color)])])
                (api/fn-call (api/symbol 'incase) [(api/key-fn :exp)
                                                   (api/fn-call (api/symbol '->) [(api/keyword :cell-background)
                                                                                  (api/symbol 'get-color)])]))

              (defncall 'make-cell '->
                (api/symbol '_))


              (defncall 'calc-def-height '->
                (api/key-fn :caravan/ast)
                (api/fn-call (api/symbol 'count) [(api/symbol '_)])
                (api/fn-call (api/symbol '*) [(api/symbol '_) (api/integer 20)])
                (api/fn-call (api/symbol '+) [(api/symbol '_) (api/integer 50)]))

              (defncall 'detect-pipe-node '->
                (api/key-fn :caravan/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/sink)]))

              (defncall 'format-def '->
                (api/map {(api/keyword :id) (api/symbol 'def-name)
                          (api/keyword :name) (api/key-fn :caravan/name)
                          (api/keyword :type) (api/key-fn :caravan/type)
                          ;; (api/keyword :display) (api/key-fn :caravan/display)
                          (api/keyword :value) (api/key-fn :caravan/ast)
                          (api/keyword :width) (api/fn-call (api/symbol 'if) [(api/symbol 'detect-pipe-node)
                                                                              (api/integer 100)
                                                                              (api/integer 300)])
                          (api/keyword :height) (api/fn-call (api/symbol 'if) [(api/symbol 'detect-pipe-node)
                                                                               (api/integer 100)
                                                                               (api/symbol 'calc-def-height)])}))


              (defncall 'pipe-name 'str
                (api/string "p/")
                (api/key-fn :from)
                (api/string "-")
                (api/key-fn :to))


              (defncall 'format-pipe '->
                (api/map {(api/keyword :id) (api/symbol 'pipe-name)
                          (api/keyword :source) (api/fn-call (api/symbol 'str)
                                                             [(api/string "d/")
                                                              (api/key-fn :from)])
                          (api/keyword :target) (api/fn-call (api/symbol 'str)
                                                             [(api/string "d/")
                                                              (api/key-fn :to)])}))



              (defncall 'is-pipe-eval '->
                (api/key-fn :caravan/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/pipe)]))

              (defncall 'filter-nodes 'filter
                (api/symbol 'is-def)
                (api/symbol '_))
              (defncall 'filter-connections 'filter
                (api/symbol 'is-pipe-eval)
                (api/symbol '_))

              (defncall 'format-defs '->
                (api/key-fn :defs)
                (api/fn-call (api/symbol 'map) [(api/symbol 'format-def) (api/symbol '_)]))

              (defncall 'extract-connection '->
                (api/vector [(api/map {(api/keyword :from) (api/key-fn :caravan/source)

                                       (api/keyword :to) (api/key-fn :caravan/func)})
                             (api/map {(api/keyword :from) (api/key-fn :caravan/func)
                                       (api/keyword :to) (api/key-fn :caravan/sink)})]))


              (defncall 'format-pipes '->
                (api/key-fn :pipes)
                (api/fn-call (api/symbol 'mapcat) [(api/symbol 'extract-connection) (api/symbol '_)])
                (api/fn-call (api/symbol 'map) [(api/symbol 'format-pipe) (api/symbol '_)]))

              (defncall 'format-state '->
                (api/key-fn :eval)
                (api/fn-call (api/symbol 'vals) [(api/symbol '_)])
                (api/map {(api/keyword :defs) (api/symbol 'filter-nodes)
                          (api/keyword :pipes) (api/symbol 'filter-connections)})
                (api/map {(api/keyword :id) (api/string "root")
                          (api/keyword :children) (api/symbol 'format-defs)
                          (api/keyword :edges) (api/symbol 'format-pipes)}))


              (defncall 'lay-in 'pipes/debug)


              (defncall 'edit-information '->
                (api/symbol '_))

              ;; handling complex state sideeffects

              (defncall 'get-selected-fn-name '->
                (api/key-fn :state)
                (api/key-fn :editor)
                (api/key-fn :selected)
                (api/symbol 'fn-name-from-select))

              (defncall 'get-mark '->
                (api/key-fn :state)
                (api/key-fn :editor)
                (api/key-fn :mark))

              (defncall 'map-choice-to-type '->
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :first)])
                                                   (api/keyword :string)])
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :second)])
                                                   (api/keyword :integer)])
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :third)])
                                                   (api/keyword :keyword)])
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :fourth)])
                                                   (api/keyword :symbol)])
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :fifth)])
                                                   (api/keyword :float)])
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :sixth)])
                                                   (api/keyword :table)])
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :seventh)])
                                                   (api/keyword :list)])
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :eigth)])
                                                   (api/keyword :accessor)])
                (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :ninth)])
                                                   (api/keyword :function)]))

              (defncall 'insert-call '->
                (api/map {(api/keyword :sym) (api/symbol 'get-selected-fn-name)
                          (api/keyword :cell) (api/symbol 'get-mark)
                          (api/keyword :type) (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                             (api/key-fn :data)
                                                                             (api/symbol 'map-choice-to-type)])}))

              (defncall 'insert-at-pos '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :call)
                                                        (api/map {(api/keyword :action)
                                                                  (api/keyword :insert)
                                                                  (api/keyword :arguments)
                                                                  (api/symbol 'insert-call)})})}))

              (defncall 'edit-call '->
                (api/map {(api/keyword :sym) (api/symbol 'get-selected-fn-name)
                          (api/keyword :cell) (api/symbol 'get-mark)
                          (api/keyword :value) (api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                                              (api/key-fn :events)
                                                                              (api/key-fn :input)])}))



              (defncall 'edit-at-pos '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :call)
                                                        (api/map {(api/keyword :action)
                                                                  (api/keyword :edit)
                                                                  (api/keyword :arguments)
                                                                  (api/symbol 'edit-call)})})}))

              (defncall 'swap-call '->
                (api/map {(api/keyword :sym) (api/symbol 'get-selected-fn-name)
                          (api/keyword :cell-idx) (api/symbol 'get-mark)
                          (api/keyword :target) (api/key-fn :target)}))

              (defncall 'swap-at-pos '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :call)
                                                        (api/map {(api/keyword :action)
                                                                  (api/keyword :swap)
                                                                  (api/keyword :arguments)
                                                                  (api/symbol 'swap-call)})})}))

              (defncall 'cut-call '->
                (api/map {(api/keyword :sym) (api/symbol 'get-selected-fn-name)
                          (api/keyword :cell-idx) (api/symbol 'get-mark)}))

              (defncall 'cut-at-pos '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :call)
                                                        (api/map {(api/keyword :action)
                                                                  (api/keyword :cut)
                                                                  (api/keyword :arguments)
                                                                  (api/symbol 'cut-call)})})}))

              (defncall 'fall-at-pos '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/key-fn :next)
                          (api/keyword :target) (api/fn-call (api/symbol '->) [(api/symbol 'get-mark)
                                                                               (api/symbol 'inc)])})
                (api/symbol 'swap-at-pos))

              (defncall 'leap-at-pos '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/key-fn :next)
                          (api/keyword :target) (api/fn-call (api/symbol '->) [(api/symbol 'get-mark)
                                                                               (api/symbol 'dec)])})
                (api/symbol 'swap-at-pos))

              (defncall 'indent-call '->
                (api/map {(api/keyword :sym) (api/symbol 'get-selected-fn-name)
                          (api/keyword :cell-idx) (api/symbol 'get-mark)}))

              (defncall 'indent-at-pos '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :call)
                                                        (api/map {(api/keyword :action)
                                                                  (api/keyword :indent)
                                                                  (api/keyword :arguments)
                                                                  (api/symbol 'indent-call)})})}))

              (defncall 'is-insert-state '->
                (api/key-fn :data)
                (api/fn-call (api/symbol 'or) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :first)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :second)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :third)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :fourth)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :fifth)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :sixth)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :seventh)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :eigth)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :ninth)])]))


              (defncall 'should-insert '->
                (api/key-fn :next)
                (api/symbol 'is-insert-state))

              (defncall 'is-editor-mode-insert '->
                (api/key-fn :state)
                (api/key-fn :editor)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :insert)]))

              (defncall 'should-edit '->
                (api/key-fn :next)
                (api/key-fn :data)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :accept)]))


              (defncall 'is-editor-mode-edit '->
                (api/key-fn :state)
                (api/key-fn :editor)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :edit)]))

              (defncall 'should-fall '->
                (api/key-fn :next)
                (api/key-fn :data)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :fall)]))

              (defncall 'should-leap '->
                (api/key-fn :next)
                (api/key-fn :data)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :leap)]))

              (defncall 'should-cut '->
                (api/key-fn :next)
                (api/key-fn :data)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :cut)]))

              (defncall 'should-indent '->
                (api/key-fn :next)
                (api/key-fn :data)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :indent)]))

              (defncall 'should-dedent '->
                (api/key-fn :next)
                (api/key-fn :data)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :dedent)]))

              (defncall 'reset-call '->
                (api/fn-call (api/symbol 'assoc) [(api/symbol '_)
                                                  (api/keyword :state)
                                                  (api/fn-call (api/symbol 'dissoc) [(api/key-fn :state)
                                                                                     (api/keyword :call)])]))

              (defncall 'handle-state 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/symbol 'reset-call)
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol 'and) [(api/symbol 'should-insert)
                                                                                                 (api/symbol 'is-editor-mode-insert)])
                                                                 (api/symbol 'insert-at-pos)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'should-fall)
                                                                 (api/symbol 'fall-at-pos)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'should-leap)
                                                                 (api/symbol 'leap-at-pos)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'should-indent)
                                                                 (api/symbol 'indent-at-pos)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'should-cut)
                                                                 (api/symbol 'cut-at-pos)])
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol 'and) [(api/symbol 'should-edit)
                                                                                                 (api/symbol 'is-editor-mode-edit)])
                                                                 (api/symbol 'edit-at-pos)])
                              (api/vector [(api/key-fn :state)
                                           (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
                (api/map {}))

              (defncall 'add-insert-actions '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :actions)
                                                        (api/vector [(api/string "Q back")
                                                                     (api/string "1 string")
                                                                     (api/string "2 number")
                                                                     (api/string "3 keyword")
                                                                     (api/string "4 symbol")
                                                                     (api/string "5 float")
                                                                     (api/string "6 table")
                                                                     (api/string "7 list")
                                                                     (api/string "8 accessor")
                                                                     (api/string "9 function")])})}))

              (defncall 'is-change-insert '->
                (api/key-fn :editor)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :insert)]))


              (defncall 'is-hover-func '->
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "func")]))

              (defncall 'is-hover-sink '->
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "sink")]))

              (defncall 'is-hover-node '->
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "pipe")]))

              (defncall 'is-hover-source '->
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "source")]))

              (defncall 'construct-lmb-action '->
                (api/key-fn :next)
                (api/key-fn :editor)
                (api/key-fn :hover)
                (api/key-fn :current)
                (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-hover-sink)
                                                    (api/vector [(api/string "LMB construct")])])
                (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-hover-sink)
                                                    (api/vector [(api/string "LMB construct")])])
                (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-hover-node)
                                                    (api/vector [(api/string "LMB connect")])])
                (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-hover-func)
                                                    (api/vector [(api/string "LMB select")])]))

              (defncall 'construct-mouse-actions '->
                (api/vector [(api/symbol 'construct-lmb-action)
                             (api/string "RMB pan")])
                (api/fn-call (api/symbol 'remove) [(api/key-fn :type) (api/symbol '_)]))


              (defncall 'is-func-selected '->
                (api/key-fn :editor)
                (api/key-fn :selected))

              (defncall 'construct-selected-action '->
                (api/key-fn :next)
                (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-func-selected)
                                                    (api/vector [(api/string "WS navigate")
                                                                 (api/string "F insert")
                                                                 (api/string "D indent")
                                                                 (api/string "Shift-WS Swap")
                                                                 (api/string "X cut")])]))
              (defncall 'construct-key-actions '->
                (api/vector [(api/symbol 'construct-selected-action)
                             (api/string "+- zoom")])
                (api/fn-call (api/symbol 'remove) [(api/key-fn :editor) (api/symbol '_)]))

              (defncall 'add-nav-actions '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :actions)
                                                        (api/fn-call (api/symbol 'flatten)
                                                                     [(api/vector [(api/symbol 'construct-key-actions)
                                                                                   (api/symbol 'construct-mouse-actions)])])})}))

              (defncall 'is-change-navigate '->
                (api/key-fn :editor)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :navigate)]))

              (defncall 'add-edit-actions '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :actions)
                                                        (api/vector [(api/string "Q back")
                                                                     (api/string "1 edit")])})}))

              (defncall 'is-change-edit '->
                (api/key-fn :editor)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :edit)]))


              (defncall 'mode-data 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'is-change-insert)])
                                                                 (api/symbol 'add-insert-actions)])
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'is-change-navigate)])
                                                                 (api/symbol 'add-nav-actions)])
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'is-change-edit)])
                                                                 (api/symbol 'add-edit-actions)])
                              (api/fn-call (api/symbol 'dissoc) [(api/symbol '_) (api/vector [(api/keyword :next) (api/keyword :editor)])])
                              (api/vector [(api/key-fn :state)
                                           (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]) ])
                (api/map {(api/keyword :actions) (api/vector [])}))

              (defncall 'tag-mode '->
                (api/map {(api/keyword :mode) (api/symbol '_)}))


              ;; graphing of nodes

              (defncall 'get-func-stroke '->
                (api/fn-call (api/symbol 'if) [(api/symbol 'is-selected)
                                               (api/fn-call (api/symbol '->) [(api/keyword :node-selected)
                                                                              (api/symbol 'get-color)])
                                               (api/fn-call (api/symbol '->) [(api/keyword :node-bg)
                                                                              (api/symbol 'get-color)])]))

              (defncall 'get-font-style '->
                (api/key-fn :exp)
                (api/key-fn :type)
                (api/fn-call (api/symbol 'lookup) [(api/symbol 'get-font) (api/symbol '_) (api/string "inherit")]))


              (defncall 'default-syntax-color '->
                (api/map {(api/keyword :cell-content)
                          (api/fn-call (api/symbol '->) [(api/keyword :cell-content)
                                                         (api/symbol 'get-color)])
                          (api/keyword :cell-active-content)
                          (api/fn-call (api/symbol '->) [(api/keyword :cell-active-content)
                                                         (api/symbol 'get-color)])}))

              (defncall 'get-type-color '->
                (api/key-fn :exp)
                (api/key-fn :type)
                (api/fn-call (api/symbol 'lookup) [(api/symbol 'get-syntax-color)
                                                   (api/symbol '_)
                                                   (api/symbol 'default-syntax-color)]))

              (defncall 'get-cell-content-color '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-active-cell)
                                                   (api/fn-call (api/symbol '->) [(api/symbol 'get-type-color)
                                                                                  (api/key-fn :cell-content)])])
                (api/fn-call (api/symbol 'incase) [(api/key-fn :exp)
                                                   (api/fn-call (api/symbol '->) [(api/symbol 'get-type-color)
                                                                                  (api/key-fn :cell-active-content)])]))

              (defncall 'graph-exp-counter '->
                (api/vector [(api/keyword :text)
                             (api/map {(api/keyword :transform)
                                       (api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/fn-call (api/symbol 'counter-pos) [(api/symbol '_)])])
                                       (api/keyword :text-anchor)
                                       (api/string "end")
                                       (api/keyword :fill)
                                       (api/fn-call (api/symbol '->) [(api/keyword :cell-counter-stroke) (api/symbol 'get-color)])})
                             (api/fn-call (api/symbol 'str) [(api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/key-fn :counter)])])]))

              (defncall 'graph-display-value '->
                (api/vector [(api/keyword :text)
                             (api/map {(api/keyword :transform)
                                       (api/fn-call (api/symbol '->) [(api/key-fn :exp)
                                                                      (api/symbol 'text-pos)])
                                       (api/keyword :fill)
                                       (api/symbol 'get-cell-content-color)
                                       (api/keyword :style)
                                       (api/map {(api/keyword :font-family)
                                                 (api/symbol 'get-font-style)})})
                             (api/fn-call (api/symbol 'str) [(api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/key-fn :value)])])]))

              (defncall 'get-input '->
                (api/key-fn :context)
                (api/key-fn :input))

              (defncall 'get-value-from-exp '->
                ;; (api/fn-call (api/symbol 'spy) [(api/string "value")])
                (api/key-fn :value))

              (defncall 'get-input-or-value '->
                (api/fn-call (api/symbol 'if) [(api/symbol 'get-input)
                                               (api/symbol 'get-input)
                                               (api/fn-call (api/symbol '->)
                                                            [(api/key-fn :exp)
                                                             (api/symbol 'get-value-from-exp)])]))

              (defncall 'graph-exp-edit-value '->
                (api/vector [(api/keyword :foreignObject)
                             (api/map {(api/keyword :x)
                                       (api/integer 0)
                                       (api/keyword :y)
                                       (api/integer -15)
                                       (api/keyword :width)
                                       (api/integer 100)
                                       (api/keyword :height)
                                       (api/integer 20)
                                       (api/keyword :transform)
                                       (api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/symbol 'text-pos)])})
                             (api/vector [(api/keyword :input)
                                          (api/map {(api/keyword :on-change) (api/keyword :change)
                                                    (api/keyword :id)
                                                    (api/fn-call (api/symbol '->)
                                                                 [(api/key-fn :exp)
                                                                  (api/fn-call (api/symbol 'str) [(api/string "input/")
                                                                                                  (api/key-fn :counter)])])
                                                    (api/keyword :style)
                                                    (api/map {(api/keyword :border)
                                                              (api/string "none")})
                                                    (api/keyword :value)
                                                    (api/symbol 'get-input-or-value)})])]))

              (defncall 'graph-exp-value '->
                (api/fn-call (api/symbol 'if) [(api/symbol 'is-edited-cell)
                                               (api/symbol 'graph-exp-edit-value)
                                               (api/symbol 'graph-display-value)]))

              (defncall 'graph-exp-type '->
                (api/vector [(api/keyword :text)
                             (api/map {(api/keyword :transform)
                                       (api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/symbol 'type-pos)])
                                       (api/keyword :font-style)
                                       (api/string "italic")
                                       (api/keyword :fill)
                                       (api/fn-call (api/symbol '->) [(api/keyword :cell-type-stroke) (api/symbol 'get-color)])})
                             (api/fn-call (api/symbol 'str) [(api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/key-fn :display)])])]))

              (defncall 'graph-exp-content '->
                (api/vector [(api/keyword :g)
                             (api/symbol 'graph-exp-counter)
                             (api/symbol 'graph-exp-value)
                             (api/symbol 'graph-exp-type)]))

              (defncall 'graph-exp '->
                (api/vector [(api/keyword :g)
                             (api/map {(api/keyword :transform) (api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/symbol 'cell-pos)])})
                             (api/vector [(api/keyword :rect)
                                          (api/map {(api/keyword :transform) (api/string "translate(-14,-15)")
                                                    (api/keyword :width) (api/integer 198)
                                                    (api/keyword :height) (api/integer 20)
                                                    (api/keyword :style) (api/map {(api/keyword :fill) (api/symbol 'get-fill)
                                                                                   (api/keyword :stroke) (api/fn-call (api/symbol '->) [(api/keyword :cell-dividers)
                                                                                                                                        (api/symbol 'get-color)])})})])
                             (api/symbol 'graph-exp-content)]))

              (defncall 'handle-exps '->
                (api/map {(api/keyword :cells) (api/key-fn :cells)
                          (api/keyword :cell) (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                            (api/symbol 'graph-exp)])])})
                (api/fn-call (api/symbol 'into) [(api/key-fn :cells) (api/key-fn :cell)]))

              (defncall 'reduce-exps '->
                (api/map {(api/keyword :state)
                          (api/key-fn :state)
                          (api/keyword :next)
                          (api/fn-call (api/symbol '->) [(api/map {(api/keyword :cells) (api/fn-call (api/symbol '->) [(api/key-fn :state) (api/key-fn :cells)])
                                                                   (api/keyword :next) (api/key-fn :next)})
                                                         (api/symbol 'handle-exps)])})
                (api/fn-call (api/symbol 'into) [(api/key-fn :state)
                                                 (api/map {(api/keyword :cells)
                                                           (api/key-fn :next)})]))

              (defncall 'graph-body '->
                (api/fn-call (api/symbol 'reduce) [(api/symbol 'reduce-exps) (api/map {(api/keyword :cells)
                                                                                       (api/vector [])})])
                (api/key-fn :cells)
                (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :g) (api/map {(api/keyword :style) (api/map {(api/keyword :font-family) (api/string "monospace")})})])
                                                 (api/symbol '_)]))

              (defncall 'merge-exp '->
                (api/map {(api/keyword :exp) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])
                          (api/keyword :context) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])}))

              (defncall 'make-body-context '->
                (api/map {(api/keyword :mark) (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                             (api/key-fn :editor)
                                                                             (api/key-fn :mark)])
                          (api/keyword :selected) (api/symbol 'is-selected)
                          (api/keyword :mode) (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                             (api/key-fn :editor)
                                                                             (api/key-fn :mode)])
                          (api/keyword :input) (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                              (api/key-fn :events)
                                                                              (api/key-fn :input)])}))

              (defncall 'merge-body '->
                (api/fn-call (api/symbol 'myzip) [(api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :value)])
                                                (api/fn-call (api/symbol '->) [(api/fn-call (api/symbol 'repeat) [(api/fn-call (api/symbol '->) [(api/key-fn :node)
                                                                                                                                                 (api/key-fn :value)
                                                                                                                                                 (api/fn-call (api/symbol 'count) [(api/symbol '_)])])
                                                                                                                  (api/symbol 'make-body-context)])])])
                (api/fn-call (api/symbol 'map) [(api/symbol 'merge-exp) (api/symbol '_)]))

              (defncall 'graph-func '->
                (api/vector [(api/keyword :g)
                             (api/map {(api/keyword :transform)
                                       (api/fn-call (api/symbol '->) [(api/key-fn :node)
                                                                      (api/symbol 'translate-str)])})
                             (api/vector [(api/keyword :rect)
                                          (api/map {(api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :width)])
                                                    (api/keyword :height) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :height)])
                                                    (api/keyword :style) (api/map {(api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :node-gutter) (api/symbol 'get-color)])
                                                                                   (api/keyword :stroke) (api/symbol 'get-func-stroke)
                                                                                   (api/keyword :filter) (api/string "url(#shadow)")
                                                                                   (api/keyword :pointer-events) (api/string "all")})})])
                             (api/vector [(api/keyword :rect)
                                          (api/map {(api/keyword :transform) (api/string "translate(200,0)")
                                                    (api/keyword :width) (api/integer 100)
                                                    (api/keyword :height) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :height)])
                                                    (api/keyword :style) (api/map {(api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :cell-type-fill) (api/symbol 'get-color)])
                                                                                   (api/keyword :stroke) (api/string "darkgrey")})})])
                             (api/vector [(api/keyword :rect)
                                          (api/map {(api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :node)(api/key-fn :width)])
                                                    (api/keyword :height) (api/integer 30)
                                                    (api/keyword :style) (api/map {(api/keyword :fill) (api/symbol 'get-func-stroke)
                                                                                   (api/keyword :stroke) (api/string "darkgrey")})})])
                             (api/vector [(api/keyword :text)
                                          (api/map {(api/keyword :x) (api/integer 150)
                                                    (api/keyword :y) (api/integer 20)
                                                    (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :node-name-stroke) (api/symbol 'get-color)])
                                                    (api/keyword :text-anchor) (api/keyword :middle)
                                                    (api/keyword :font-weight) (api/string "bold")})
                                          (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :name)])])
                             (api/vector [(api/keyword :g)
                                          (api/map {(api/keyword :transform)
                                                    (api/fn-call (api/symbol '->) [(api/map {(api/keyword :x)
                                                                                             (api/integer 15)
                                                                                             (api/keyword :y)
                                                                                             (api/integer 25)})
                                                                                   (api/symbol 'translate-str)])})
                                          (api/fn-call (api/symbol '->) [(api/symbol 'merge-body)
                                                                         (api/symbol 'graph-body)])])
                             (api/vector [(api/keyword :rect)
                                          (api/map {(api/keyword :id) (api/symbol 'func-id)
                                                    (api/keyword :on-click) (api/keyword :click)
                                                    (api/keyword :fill-opacity) (api/integer 0)

                                                    (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "all")})
                                                    (api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :width)])
                                                    (api/keyword :height) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :height)])})])]))

              (defncall 'pipe-id '->
                (api/fn-call (api/symbol 'str) [(api/string "pipe/")
                                                (api/key-fn :name)]))

              (defncall 'graph-pipe '->
                (api/key-fn :node)
                (api/vector [(api/keyword :g)
                             (api/map {(api/keyword :transform)
                                       (api/symbol 'translate-str) })
                             (api/vector [(api/keyword :circle)
                                          (api/map {(api/keyword :id) (api/symbol 'pipe-id)
                                                    (api/keyword :cx) (api/integer 50)
                                                    (api/keyword :cy) (api/integer 50)
                                                    (api/keyword :r) (api/integer 50)
                                                    (api/keyword :style) (api/map {(api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :pipe-fill) (api/symbol 'get-color)])
                                                                                   (api/keyword :stroke) (api/fn-call (api/symbol '->) [(api/keyword :pipe-stroke) (api/symbol 'get-color)])
                                                                                   (api/keyword :filter) (api/string "url(#shadow)")
                                                                                   (api/keyword :pointer-events) (api/string "all")})})])
                             (api/vector [(api/keyword :text)
                                          (api/map {(api/keyword :x) (api/integer 50)
                                                    (api/keyword :y) (api/integer 43)
                                                    (api/keyword :dy) (api/integer 14)
                                                    (api/keyword :fill)
                                                    (api/fn-call (api/symbol '->) [(api/keyword :node-name-stroke) (api/symbol 'get-color)])
                                                    (api/keyword :text-anchor) (api/keyword :middle)
                                                    (api/keyword :font-weight) (api/string "bold")})
                                          (api/key-fn :name)])]))

              (defncall 'is-pipe-node '->
                (api/key-fn :node)
                (api/key-fn :type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "sink")]))

              (defncall 'is-func-node '->
                (api/key-fn :node)
                (api/key-fn :value))

              (defncall 'graph-node '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-pipe-node)
                                                   (api/symbol 'graph-pipe)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-func-node)
                                                   (api/symbol 'graph-func)]))

              (defncall 'merge-node '->
                (api/map {(api/keyword :node) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])
                          (api/keyword :context) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])}))

              (defncall 'graph-nodes '->
                (api/fn-call (api/symbol 'myzip) [(api/fn-call (api/symbol '->) [(api/key-fn :layout)
                                                                                 (api/key-fn :children)])
                                                  (api/fn-call (api/symbol 'repeat) [(api/fn-call (api/symbol '->) [(api/key-fn :layout)
                                                                                                                    (api/key-fn :children)
                                                                                                                    (api/fn-call (api/symbol 'count) [(api/symbol '_)])])
                                                                                     (api/key-fn :context)])])
                (api/fn-call (api/symbol 'map) [(api/symbol 'merge-node) (api/symbol '_)])
                (api/fn-call (api/symbol 'map) [(api/symbol 'graph-node) (api/symbol '_)])
                (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :g)]) (api/symbol '_)]))

              (defncall 'graph-connection '->
                (api/key-fn :sections)
                (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])
                (api/vector [(api/keyword :g)
                             (api/vector [(api/keyword :line)
                                          (api/map {(api/keyword :style) (api/map {(api/keyword :stroke) (api/string "darkgrey")})
                                                    (api/keyword :x1) (api/fn-call (api/symbol '->)[(api/key-fn :startPoint) (api/key-fn :x)])
                                                    (api/keyword :y1) (api/fn-call (api/symbol '->)[(api/key-fn :startPoint) (api/key-fn :y)])
                                                    (api/keyword :x2) (api/fn-call (api/symbol '->)[(api/key-fn :endPoint) (api/key-fn :x)])
                                                    (api/keyword :y2) (api/fn-call (api/symbol '->)[(api/key-fn :endPoint) (api/key-fn :y)])})])]))

              (defncall 'graph-connections '->
                (api/key-fn :layout)
                (api/key-fn :edges)
                (api/fn-call (api/symbol 'map) [(api/symbol 'graph-connection) (api/symbol '_)])
                (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :g)]) (api/symbol '_)]))


              (defncall 'build-context '->
                (api/map {(api/keyword :editor) (api/key-fn :editor)
                          (api/keyword :selected) (api/fn-call (api/symbol '->) [(api/key-fn :editor)
                                                                                 (api/key-fn :selected)])
                          (api/keyword :events) (api/key-fn :events)}))


              (defncall 'translate-graph 'str
                (api/string "translate(")
                (api/key-fn :x)
                (api/string ",")
                (api/key-fn :y)
                (api/string ") ")
                (api/string "scale(")
                (api/key-fn :zoom)
                (api/string ")"))

              (defncall 'graph-background '->
                (api/vector [(api/keyword :rect)
                             (api/map {(api/keyword :id) (api/string "back/ground")
                                       (api/keyword :width) (api/integer 1200)
                                       (api/keyword :height) (api/integer 800)
                                       (api/keyword :x) (api/fn-call (api/symbol '->) [(api/key-fn :view) (api/key-fn :x) (api/fn-call (api/symbol '-) [(api/symbol '_)])])
                                       (api/keyword :y) (api/fn-call (api/symbol '->) [(api/key-fn :view) (api/key-fn :y) (api/fn-call (api/symbol '-) [(api/symbol '_)])])
                                       (api/keyword :fill) (api/string "url(#grid)")
                                       ;; (api/fn-call (api/symbol '->) [(api/keyword :graph-background)
                                       ;;   (api/symbol 'get-color)])
                                       (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "all")})})]))

              (defncall 'graph '->
                (api/map {(api/keyword :layout) (api/key-fn :layout)
                          (api/keyword :view) (api/key-fn :view)
                          (api/keyword :context) (api/symbol 'build-context)})
                (api/map {(api/keyword :graph)
                          (api/vector [(api/keyword :g)
                                       (api/map {(api/keyword :transform) (api/fn-call (api/symbol '->) [(api/key-fn :view) (api/symbol 'translate-graph)])})
                                       (api/symbol 'graph-background)
                                       (api/symbol 'graph-nodes)
                                       (api/symbol 'graph-connections)])}))


              ;; reduce elements to latest version of GUI element

              (defncall 'elements-reduce 'pipes/reductions
                (api/fn-call (api/symbol 'into) [(api/key-fn :state) (api/key-fn :next)])
                (api/map {}))

              (defncall 'svg-elements-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])
                              ;; (api/fn-call (api/symbol 'spy) [(api/string "XXX reduce")])
                              ])
                (api/map {(api/keyword :graph) (api/vector [(api/keyword :g)])
                          (api/keyword :sink-menu) (api/vector [(api/keyword :g)])
                          (api/keyword :source-menu) (api/vector [(api/keyword :g)])
                          (api/keyword :action-menu) (api/vector [(api/keyword :g)])}))

              (defncall 'reducer 'pipes/debug ;; (api/keyword :oasis.spec/gui)
                )
              (defncall 'render 'pipes/debug ;; (api/keyword :oasis.spec/render)
                )

              ;; render elements to hiccup
              (defncall 'render-elements '->
                (api/fn-call (api/symbol 'vals) [(api/symbol '_)])
                ;; (api/fn-call (api/symbol 'spy) [(api/string "vals")])
                ;; (api/fn-call (api/symbol 'sort-by [(api/symbol '_)]))
                (api/fn-call (api/symbol 'map) [(api/key-fn :oasis.gui/element) (api/symbol '_)])
                (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :div)]) (api/symbol '_)]))

              (defncall 'svg-reduced 'pipes/debug  ;; (api/keyword :oasis.spec/render)
                )

              (defncall 'svg-defs '->
                (api/vector [(api/keyword :defs)
                             (api/vector [(api/keyword :pattern)
                                          (api/map {(api/keyword :id) (api/string "grid")
                                                    (api/keyword :width) (api/integer 50)
                                                    (api/keyword :height) (api/integer 50)
                                                    (api/keyword :patternUnits) (api/string "userSpaceOnUse")})
                                          (api/vector [(api/keyword :path)
                                                       (api/map {(api/keyword :d) (api/string "M 50 0 L 0 0 0 50")
                                                                 (api/keyword :fill) (api/string "none")
                                                                 (api/keyword :stroke) (api/string "silver")
                                                                 (api/keyword :stroke-width) (api/string "0.5")})])])
                             (api/vector [(api/keyword :filter)
                                          (api/map {(api/keyword :id) (api/string "shadow")})
                                          (api/vector [(api/keyword :feDropShadow)
                                                       (api/map {(api/keyword :dx) (api/string "3")
                                                                 (api/keyword :dy) (api/string "3")
                                                                 (api/keyword :flood-color) (api/fn-call (api/symbol '->) [(api/keyword :shadow-flood)
                                                                                                                           (api/symbol 'get-color)])
                                                                 (api/keyword :flood-opacity) (api/string "0.3")
                                                                 (api/keyword :stdDeviation) (api/string "3")})])])
                             (api/vector [(api/keyword :filter)
                                          (api/map {(api/keyword :id) (api/string "leftshadow")})
                                          (api/vector [(api/keyword :feDropShadow)
                                                       (api/map {(api/keyword :dx) (api/string "-3")
                                                                 (api/keyword :dy) (api/string "3")
                                                                 (api/keyword :flood-color) (api/fn-call (api/symbol '->) [(api/keyword :shadow-flood)
                                                                                                                           (api/symbol 'get-color)])
                                                                 (api/keyword :flood-opacity) (api/string "0.3")
                                                                 (api/keyword :stdDeviation) (api/string "3")})])])
                             (api/vector [(api/keyword :filter)
                                          (api/map {(api/keyword :id) (api/string "upshadow")})
                                          (api/vector [(api/keyword :feDropShadow)
                                                       (api/map {(api/keyword :dx) (api/string "0")
                                                                 (api/keyword :dy) (api/string "-3")
                                                                 (api/keyword :flood-color) (api/fn-call (api/symbol '->) [(api/keyword :shadow-flood)
                                                                                                                           (api/symbol 'get-color)])
                                                                 (api/keyword :flood-opacity) (api/string "0.3")
                                                                 (api/keyword :stdDeviation) (api/string "3")})])])]))

              ;; render SVG components
              (defncall 'svg-render 'pipes/debug)
              (defncall 'render-svg '->
                ;; (api/fn-call (api/symbol 'spy) [(api/string "XXX rsvg")])
                (api/map {(api/keyword :svg)
                          (api/map {(api/keyword :oasis.gui/order)
                                    (api/integer 2)
                                    (api/keyword :oasis.gui/element)
                                    (api/vector [(api/keyword :svg)
                                                 (api/map {(api/keyword :width) (api/integer 1200)
                                                           (api/keyword :height) (api/integer 800)})
                                                 (api/symbol 'svg-defs)
                                                 (api/key-fn :graph)
                                                 (api/key-fn :source-menu)
                                                 (api/key-fn :sink-menu)
                                                 (api/key-fn :action-menu)
                                                 ])})}))

              (defncall 'init-view '->
                (api/map {(api/keyword :zoom) (api/integer 1)
                          (api/keyword :x) (api/integer 150)
                          (api/keyword :y) (api/integer 50)}))

              ])

(def network
  [              ;; networks

              (pipe 'd 'log)
              (pipe 'oasis-ev 'log)

              (red 'oasis-mouse 'mouse-reduce 'mouse-state)
              (red 'oasis-mouse 'target-reduce 'target-events)

              (pipe 'target-events 'only-different 'hover-events)
              (pipe 'hover-events 'tag-hover 'hover-state)

              (pipe 'oasis-kb 'filter-key-input 'keyboard-filtered)
              (pipe 'keyboard-filtered 'filter-edit 'editor-commands)
              (pipe 'keyboard-filtered 'filter-menu 'editor-commands)
              ;; (pipe 'keyboard-filtered 'log-keyboard)
              (pipe 'oasis-kb 'log-keyboard)

              (pipe 'keyboard-filtered 'filter-view 'view-commands)
              (pipe 'view-commands 'make-zoom 'view-events)

              (red 'raw-events 'input-reduce 'reduced-events)
              (pipe 'raw-events 'tag-events 'events)

              (pipe 'events 'log-events)

              (pipe 'oasis-ev 'filter-input 'raw-events)
              (pipe 'oasis-ev 'filter-submit 'raw-events)

              (pipe 'oasis-ev 'filter-select 'editor-commands)
              ;; (pipe 'select-events 'editor-commands)

              (pipe 'n 'eval-events)
              (red 'eval-events 'eval-reduce 'eval-raw)
              (pipe 'eval-raw 'tag-eval 'eval-state)


              (pipe 'mouse-state 'filter-drag 'drag-events)

              (pipe 'drag-events 'filter-drag-end 'drag-state)

              (pipe 'drag-state 'log-mouse)
              (pipe 'drag-state 'interpret-drag 'editor-commands)

              (pipe 'editor-commands 'handle-commands 'editor-events)
              (pipe 'hover-state 'editor-events)
              (red 'editor-events 'editor-state-reduce 'editor-cooked)
              (pipe 'editor-cooked 'tag-editor 'editor-state)
              (pipe 'editor-state 'log-editor)

              (pipe 'editor-commands 'filter-immediate 'editor-immediate)
              ;; (pipe 'editor-immediate 'log-command)

              (pipe 'mouse-state 'filter-scroll 'scroll-state)
              (pipe 'scroll-state 'construct-view 'view-deltas)
              (red 'view-deltas 'view-delta 'view-events)
              (red 'view-events 'view-reduce 'view-raw)
              (pipe 'view-raw 'tag-view 'view-state)


              (red 'editor-state 'state-reduce 'state)
              (red 'eval-state 'state-reduce 'state)
              (red 'view-state 'state-reduce 'state)
              ;; (red 'mouse-state 'state-reduce 'state)
              (red 'hover-state 'state-reduce 'state)
              (red 'mode-state 'state-reduce 'state)
              (red 'events 'state-reduce 'state)
              (pipe 'state 'log-state)

              (pipe 'eval-state 'format-state 'oasis-layout)
              (pipe 'eval-state 'format-state 'log-layout)

              (pipe 'eval-state 'edit-information 'editor-events)

              (pipe 'oasis-layout 'tag-layout 'layout-state)
              (pipe 'layout-state 'log-layout)
              (red 'layout-state 'state-reduce 'state)

              ;; (red 'select-events 'center-view 'view-events)
              ;; (red 'layout-state 'center-view 'view-events)

              (red 'editor-state 'mode-data 'mode-raw)
              (pipe 'mode-raw 'tag-mode 'mode-state)

              (pipe 'editor-state 'editor-actions)
              (pipe 'editor-immediate 'editor-actions)
              (pipe 'mode-state 'editor-actions)
              (pipe 'events 'editor-actions)

              (red 'editor-actions 'handle-state 'be-commands)
              ;; (pipe 'be-commands 'log-command)
              (pipe 'be-commands 'filter-call 'caravan)

              (pipe 'state 'graph 'svg-render)
              (red 'render 'elements-reduce 'reducer)
              ;; (red 'render 'elements-reduce 'log-render)

              ;; (pipe 'reducer 'render-elements 'log-render)
              (pipe 'reducer 'render-elements 'oasis-ui)

              (red 'svg-render 'svg-elements-reduce 'svg-reduced)
              (pipe 'svg-reduced 'render-svg 'render)

              (pipe 'init 'source-menu-const 'source-menu-items)
   (red 'source-menu-items 'source-menu-map 'source-menu)
              (pipe 'source-menu 'tag-items 'source-menu-events)
              (pipe 'hover-state 'source-menu-events)
              (red 'source-menu-events 'reduce-menu-source 'source-menu-state)
              (pipe 'source-menu-state 'render-source-menu 'svg-render)

              (pipe 'init 'sink-menu-const 'sink-menu-items)
              (red 'sink-menu-items 'sink-menu-map 'sink-menu)
              (pipe 'sink-menu 'tag-items 'sink-menu-events)
              (pipe 'hover-state 'sink-menu-events)
              (red 'sink-menu-events 'reduce-menu-sink 'sink-menu-state)
              (pipe 'sink-menu-state 'render-sink-menu 'svg-render)

              (pipe 'state 'render-action-menu 'svg-render)

              (pipe 'init 'header 'render)
              ;;                (pipe 'init 'repl 'render)
   (pipe 'init 'init-view 'view-events)
   ;; (api/defexp 'oasis-main (api/pipe (api/symbol 'oasis-init)
   ;;                                   (api/symbol 'init)))
   (api/defexp 'oasis (api/map {(api/keyword :source) (api/map {(api/keyword :main) (api/symbol 'init)
                                                                (api/keyword :ui) (api/symbol 'oasis-ev)
                                                     ;; (api/keyword :mouse) (api/symbol 'oasis-mouse)
                                                     ;; (api/keyword :kb) (api/symbol 'oasis-kb)
                                                     ;; (api/keyword :layout) (api/symbol 'oasis-layout)
                                                     })
                     ;; (api/keyword :sink) (api/vector [(api/symbol 'oasisp)])
                                   (api/keyword :tests) (api/map {(api/keyword ::test)
                                                                  (api/map {(api/keyword :when) (api/map {(api/string "init")
                                                                                                          (api/vector [(api/integer 1)])})
                                                                            (api/keyword :then) (api/map {(api/string "oasis-ui")
                                                                                                          (api/vector [(api/fn-call (api/symbol '|>) [(api/keyword :success)])])})})})
                     ;; (api/keyword :network) (api/)
                     }))
   ])


(defn start []
  (into oasis (flatten network)))

(defn store [stores]
  (.persist-tree! stores oasis)
  (.persist-tree! stores (flatten network))
  stores)
