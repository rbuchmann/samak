(ns samak.oasis
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

(defn pipe
  ""
  ([in out]
   (api/pipe [(api/symbol in) (api/symbol out)]))
  ([in x out]
   (api/pipe [(api/symbol in)
              (api/fn-call (api/symbol '|>) [(api/symbol x)])
              (api/symbol out)])))

(defn red
  ""
  ([in x out]
   (api/pipe [(api/symbol in)
              (api/symbol x)
              (api/symbol out)])))

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

(defn start
  []
  (let [oasis [(defncall 'ui 'pipes/ui)
               (defncall 'mouse 'pipes/mouse)
               (defncall 'keyboard 'pipes/keyboard)
               (defncall 'd 'pipes/debug)
               (defncall 'log 'pipes/log)
               (defncall 'layout 'pipes/layout)

               (defncall 'log-state 'pipes/log (api/string "state: "))
               (defncall 'log-command 'pipes/log (api/string "cmd: "))
               (defncall 'log-layout 'pipes/log (api/string "layout: "))
               (defncall 'log-render 'pipes/log (api/string "render: "))
               (defncall 'log-events 'pipes/log (api/string "events: "))
               (defncall 'log-editor 'pipes/log (api/string "editor: "))
               (defncall 'log-mouse 'pipes/log (api/string "mouse: "))
               (defncall 'log-keyboard 'pipes/log (api/string "keyboard: "))
               (defncall 'n 'pipes/eval-notify)

               (defmap 'get-color
                 {(api/keyword :cell-active) (api/string "#aaaaff")
                  (api/keyword :cell-seclight) (api/string "#ccccff")
                  (api/keyword :cell-background) (api/string "#ffffff")
                  (api/keyword :element-highlight-stroke) (api/string "#aaaaff")})

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

               ;; (defncall 'filter-input '->
               ;;   (api/fn-call (api/symbol 'if)
               ;;                [(api/symbol 'is-input)
               ;;                 (api/symbol 'handle-input)
               ;;                 (api/symbol 'ignore)]))

               ;; (defncall 'filter-submit '->
               ;;   (api/fn-call (api/symbol 'if)
               ;;                [(api/symbol 'is-submit-data)
               ;;                 (api/symbol 'handle-submit)
               ;;                 (api/symbol 'ignore)]))

               (defncall 'is-click-event '->
                 (api/key-fn :data)
                 (api/fn-call (api/symbol '=) [(api/keyword :click)]))

               (defncall 'construct-select '->
                 (api/map {(api/keyword :command) (api/keyword :select)
                           (api/keyword :data) (api/key-fn :event)}))

               (defncall 'filter-select 'if
                 (api/symbol 'is-click-event)
                 (api/symbol 'construct-select)
                 (api/symbol 'ignore))

               (defncall 'raw-events 'pipes/debug)
               (defncall 'reduced-events 'pipes/debug)
               (defncall 'events 'pipes/debug)

               (defncall 'merge-state '->
                 (api/vector [(api/key-fn :state) (api/key-fn :next)])
                 (api/fn-call (api/symbol 'concat) [(api/map {})]))

               (defncall 'has-submit '->
                 (api/key-fn :state)
                 (api/key-fn :submit))

               (defncall 'select-input '->
                 (api/key-fn :state)
                 (api/key-fn :input))

               (defncall 'merge-without-submit '->
                 (api/vector [(api/map {(api/keyword :input) (api/symbol 'select-input)})
                              (api/key-fn :next)])
                 (api/fn-call (api/symbol 'concat) [(api/map {})]))

               ;; (defncall 'input-reduce 'pipes/reductions
               ;;   (api/fn-call (api/symbol '->) [(api/fn-call (api/symbol 'if) [(api/symbol 'has-submit)
               ;;                                                                 (api/symbol 'merge-without-submit)
               ;;                                                                 (api/symbol 'merge-state)])])
               ;;   (api/map {}))


               (defncall 'is-complete 'and
                 (api/key-fn :input)
                 (api/key-fn :submit))

               (defncall 'only-complete '->
                 (api/fn-call (api/symbol 'only) [(api/symbol 'is-complete)]))


               ;; (defncall 'ev 'pipes/eval-line)

               (defncall 'make-eval '->
                 (api/key-fn :input))


               (defncall 'translate-str 'str
                 (api/string "translate(")
                 (api/key-fn :x)
                 (api/string ",")
                 (api/key-fn :y)
                 (api/string ")"))

               (defncall 'oasis 'pipes/debug)
               (defmap 'repl
                 {(api/keyword :repl)
                  (api/map {(api/keyword :oasis.gui/order)
                            (api/integer 10)
                            (api/keyword :oasis.gui/element)
                            (api/vector [(api/keyword :form) (api/map {(api/keyword :on-submit) (api/keyword :submit)})
                                         (api/vector [(api/keyword :input) (api/map {(api/keyword :on-change) (api/keyword :change)
                                                                                     (api/keyword :id) (api/string "input-repl")
                                                                                     (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "auto")})})])])})})

               (defncall 'header '->
                 (api/map {(api/keyword :header)
                           (api/map {(api/keyword :oasis.gui/order)
                                     (api/integer 1)
                                     (api/keyword :oasis.gui/element)
                                     (api/vector [(api/keyword :h1)
                                                  (api/string "Oasis")])})}))

               (defmap 'css-background
                 {(api/keyword :width) (api/string "1200px")
                  (api/keyword :height) (api/string "800px")
                  (api/keyword :position) (api/string "absolute")
                  (api/keyword :z-index) (api/integer -100)
                  (api/keyword :background-color) (api/string "#AABBDD")})

               (defncall 'calculate-y '->
                 (api/vector [(api/integer 100) (api/key-fn :position)])
                 (api/symbol 'mult)
                 (api/vector [(api/integer 10) (api/symbol 'id)])
                 (api/symbol 'sum))

               (defncall 'menu-transform '->
                 (api/map {(api/keyword :x) (api/integer 50)
                           (api/keyword :y) (api/symbol 'calculate-y)})
                 (api/symbol 'translate-str))

               (defncall 'source-id 'str
                 (api/string "source/")
                 (api/key-fn :name))

               (defncall 'render-menu-entry '->
                 (api/vector [(api/keyword :g)
                              (api/map {(api/keyword :transform) (api/symbol 'menu-transform)})
                              (api/vector [(api/keyword :circle)
                                           (api/map {(api/keyword :id) (api/symbol 'source-id)
                                                     (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "all")})
                                                     (api/keyword :cx) (api/integer 0)
                                                     (api/keyword :cy) (api/integer 45)
                                                     (api/keyword :r) (api/integer 45)
                                                     (api/keyword :fill) (api/string "#eee")})])
                              (api/vector [(api/keyword :circle)
                                           (api/map {(api/keyword :cx) (api/integer 0)
                                                     (api/keyword :cy) (api/integer 45)
                                                     (api/keyword :r) (api/integer 40)
                                                     (api/keyword :stroke-width) (api/integer 2)
                                                     (api/keyword :stroke) (api/fn-call (api/symbol '->) [(api/symbol 'get-color)
                                                                                                          (api/key-fn :element-highlight-stroke)])
                                                     (api/keyword :fill) (api/string "#eee")})])
                              (api/vector [(api/keyword :text)
                                           (api/map {(api/keyword :height) (api/integer 20)
                                                     (api/keyword :width) (api/string "100%")
                                                     (api/keyword :text-anchor) (api/keyword :middle)
                                                     (api/keyword :x) (api/integer 0)
                                                     (api/keyword :y) (api/integer 35)
                                                     (api/keyword :dy) (api/integer 14)})
                                           (api/key-fn :name)])]))

               (defncall 'render-source-menu '->
                 (api/fn-call (api/symbol 'map) [(api/symbol 'render-menu-entry)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :g)])])
                 (api/vector [(api/keyword :g)
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :id) (api/string "menu-source")
                                                     (api/keyword :height) (api/string "100%")
                                                     (api/keyword :width) (api/integer 100)
                                                     (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#shadow)")
                                                                                    (api/keyword :pointer-events) (api/string "all")})
                                                     (api/keyword :fill) (api/string "#ccc")})])
                              (api/symbol 'id)])
                 (api/map {(api/keyword :source-menu) (api/symbol 'id)}))

               (defncall 'render-sink-menu '->
                 (api/fn-call (api/symbol 'map) [(api/symbol 'render-menu-entry)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :g)])])
                 (api/vector [(api/keyword :g)
                              (api/map {(api/keyword :transform) (api/string "translate(1100,0)")})
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :id) (api/string "menu-sink")
                                                     (api/keyword :height) (api/string "100%")
                                                     (api/keyword :width) (api/integer 100)
                                                     (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#leftshadow)")
                                                                                    (api/keyword :pointer-events) (api/string "all")})
                                                     (api/keyword :fill) (api/string "#ccc")})])
                              (api/symbol 'id)])
                 (api/map {(api/keyword :sink-menu) (api/symbol 'id)}))

               (defncall 'render-action-menu '->
                 (api/map {(api/keyword :action-menu)
                           (api/vector [(api/keyword :g)
                                        (api/map {(api/keyword :transform) (api/string "translate(0,700)")})
                                        (api/vector [(api/keyword :rect)
                                                     (api/map {(api/keyword :id) (api/string "menu-action")
                                                               (api/keyword :height) (api/integer 100)
                                                               (api/keyword :width) (api/string "100%")
                                                               (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#upshadow)")
                                                                                              (api/keyword :pointer-events) (api/string "all")})
                                                               (api/keyword :fill) (api/string "#ccc")})])
                                        (api/vector [(api/keyword :text)
                                                     (api/map {(api/keyword :height) (api/integer 20)
                                                               (api/keyword :width) (api/string "100%")
                                                               (api/keyword :text-anchor) (api/keyword :middle)
                                                               (api/keyword :x) (api/integer 0)
                                                               (api/keyword :y) (api/integer 35)
                                                               (api/keyword :dy) (api/integer 14)})
                                                     (api/string "actions:")])])}))

               (defncall 'source-menu 'pipes/debug)
               (defncall 'source-menu-items 'pipes/debug)

               (defncall 'source-menu-const '->
                 (api/vector [(api/string "debug")
                              (api/string "ui")
                              (api/string "http")])
                 (api/symbol 'many))

               (defncall 'sink-menu 'pipes/debug)
               (defncall 'sink-menu-items 'pipes/debug)

               (defncall 'sink-menu-const '->
                 (api/vector [(api/string "debug")
                              (api/string "log")
                              (api/string "ui")
                              (api/string "http")])
                 (api/symbol 'many))

               (defncall 'map-menu-item '->
                 (api/map {(api/keyword :position)
                           (api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                          (api/symbol 'count)])
                           (api/keyword :name)
                           (api/key-fn :next)}))

               (defncall 'source-menu-map 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/vector [(api/key-fn :state) (api/symbol 'map-menu-item)])
                               (api/symbol 'flatten)])
                 (api/vector []))

               (defncall 'sink-menu-map 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/vector [(api/key-fn :state) (api/symbol 'map-menu-item)])
                               (api/symbol 'flatten)])
                 (api/vector []))


               (defncall 'is-mouse-move '->
                 (api/key-fn :next)
                 (api/key-fn :samak.mouse/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :move)]))

               (defncall 'calculate-mouse-delta-x '->
                 (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                             (api/key-fn :samak.mouse/page-x)])
                              (api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                             (api/key-fn :mouse)
                                                             (api/key-fn :position)
                                                             (api/key-fn :x)
                                                             (api/fn-call (api/symbol 'negate) [])])])


                 (api/symbol 'sum))

               (defncall 'calculate-mouse-delta-y '->
                 (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                             (api/key-fn :samak.mouse/page-y)])
                              (api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                             (api/key-fn :mouse)
                                                             (api/key-fn :position)
                                                             (api/key-fn :y)
                                                             (api/fn-call (api/symbol 'negate) [])])])


                 (api/symbol 'sum))

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
                 (api/fn-call (api/symbol '=) [(api/keyword :down)]))

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
                 (api/fn-call (api/symbol '=) [(api/keyword :up)]))

               (defncall 'handle-mouse-up '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/map {(api/keyword :drag)
                                                         (api/map {(api/keyword :active) (api/keyword :false)
                                                                   (api/keyword :end) (api/keyword :end)
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
                               (api/fn-call (api/symbol 'concat) [(api/map {})])
                               (api/map {(api/keyword :mouse) (api/symbol 'id)})])
                 (api/map {(api/keyword :mouse) (api/map {})}))

               (defncall 'mouse-state 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                 )

               (defncall 'is-drag '->
                 (api/key-fn :mouse)
                 (api/key-fn :drag)
                 (api/key-fn :active)
                 (api/fn-call (api/symbol '=) [(api/keyword :true)]))

               (defncall 'is-drag-end '->
                 (api/key-fn :mouse)
                 (api/key-fn :drag)
                 (api/key-fn :end)
                 (api/fn-call (api/symbol '=) [(api/keyword :end)]))

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
               ;;                 (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
               ;;   (api/map {}))


               ;; keyboard handling

               (defncall 'is-kb-mark-up '->
                 (api/key-fn :which)
                 (api/fn-call (api/symbol '=) [(api/integer 119)]))  ;; W

               (defncall 'is-kb-mark-down '->
                 (api/key-fn :which)
                 (api/fn-call (api/symbol '=) [(api/integer 115)]))  ;; S

               (defncall 'is-kb-insert '->
                 (api/key-fn :which)
                 (api/fn-call (api/symbol '=) [(api/integer 102)]))  ;; F


               (defncall 'construct-cursor '->
                 (api/map {(api/keyword :command) (api/keyword :cursor)
                           (api/keyword :data) (api/symbol 'id)}))

               (defncall 'construct-up '->
                 (api/keyword :up)
                 (api/symbol 'construct-cursor))

               (defncall 'construct-down '->
                 (api/keyword :down)
                 (api/symbol 'construct-cursor))

               (defncall 'construct-insert '->
                 (api/map {(api/keyword :command) (api/keyword :insert)
                           (api/keyword :type) (api/keyword :immediate)
                           (api/keyword :data) (api/keyword :none)}))

               (defncall 'filter-cursor '->
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-mark-down)
                                                    (api/symbol 'construct-down)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-mark-up)
                                                    (api/symbol 'construct-up)])
                 (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                    (api/symbol 'ignore)]))
               (defncall 'is-kb-menu '->
                 (api/key-fn :which)
                 (api/fn-call (api/symbol '=) [(api/integer 94)])) ;; ^

               (defncall 'construct-menu '->
                 (api/map {(api/keyword :command) (api/keyword :menu)
                           (api/keyword :data) (api/keyword :none)}))

               (defncall 'is-kb-load '->
                 (api/key-fn :which)
                 (api/fn-call (api/symbol '=) [(api/integer 108)])) ;; L

               (defncall 'construct-load '->
                 (api/map {(api/keyword :command) (api/keyword :load)
                           (api/keyword :data) (api/keyword :none)}))

               (defncall 'filter-menu '->
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-menu)
                                                    (api/symbol 'construct-menu)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-load)
                                                    (api/symbol 'construct-load)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-insert)
                                                    (api/symbol 'construct-insert)])
                 (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                    (api/symbol 'ignore)]))

               (defncall 'is-command '->
                 (api/key-fn :command))


               (defncall 'handle-sink '->
                 (api/fn-call (api/symbol 'drop) [(api/integer 7)]) ;; pipes/
                 (api/fn-call (api/symbol 'join) [])
                 (api/map {(api/keyword :command) (api/keyword :create-sink)
                           (api/keyword :data) (api/map {(api/keyword :name) (api/symbol 'id)})}))

               (defncall 'is-sink '->
                 (api/fn-call (api/symbol 'index-of) [(api/string "source/")])
                 (api/fn-call (api/symbol '=) [(api/integer 0)]))

               (defncall 'handle-mouse-click '->
                 (api/key-fn :source)
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-sink)
                                                    (api/symbol 'handle-sink)]))

               (defncall 'is-mouse-click '->
                 (api/vector [(api/key-fn :source) (api/key-fn :target)])
                 (api/fn-call (api/symbol 'unique) [])
                 (api/symbol 'count)
                 (api/fn-call (api/symbol '=) [(api/integer 1)]))

               (defncall 'get-pipe-name '->
                 (api/fn-call (api/symbol 'drop) [(api/integer 5)]) ;; pipe/
                 (api/fn-call (api/symbol 'join) []))

               (defncall 'handle-mouse-connect '->
                 (api/map {(api/keyword :command) (api/keyword :connect)
                           (api/keyword :data) (api/map {(api/keyword :source) (api/fn-call (api/symbol '->) [(api/key-fn :source) (api/symbol 'get-pipe-name)])
                                                         (api/keyword :sink) (api/fn-call (api/symbol '->) [(api/key-fn :target) (api/symbol 'get-pipe-name)])})}))

               (defncall 'is-pipe '->
                 (api/fn-call (api/symbol 'index-of) [(api/string "pipe/")])
                 (api/fn-call (api/symbol '=) [(api/integer 0)]))

               (defncall 'is-source-source '->
                 (api/key-fn :source)
                 (api/symbol 'is-pipe))

               (defncall 'is-target-sink '->
                 (api/key-fn :target)
                 (api/symbol 'is-pipe))

               (defncall 'both-pipe '->
                 (api/fn-call (api/symbol 'and) [(api/symbol 'is-source-source)
                                                 (api/symbol 'is-target-sink)]))

               (defncall 'interpret-drag '->
                 (api/key-fn :mouse)
                 (api/key-fn :drag)
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mouse-click)
                                                    (api/symbol 'handle-mouse-click)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'both-pipe)
                                                    (api/symbol 'handle-mouse-connect)])
                 (api/fn-call (api/symbol 'unless) [(api/symbol 'is-command)
                                                    (api/map {(api/keyword :command) (api/keyword :noop)})]))

               (defncall 'is-scroll '->
                 (api/key-fn :mouse)
                 (api/key-fn :drag)
                 (api/key-fn :button)
                 (api/fn-call (api/symbol '=) [(api/integer 2)]))

               (defncall 'filter-scroll 'only (api/symbol 'is-scroll))

               (defncall 'scroll-state 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                 )

               (defncall 'construct-view '->
                 (api/key-fn :mouse)
                 (api/map {(api/keyword :x) (api/fn-call (api/symbol '->) [(api/key-fn :delta)
                                                                           (api/key-fn :x)])
                           (api/keyword :y) (api/fn-call (api/symbol '->) [(api/key-fn :delta)
                                                                           (api/key-fn :y)])})
                 (api/map {(api/keyword :view) (api/symbol 'id)}))

               (defncall 'center-view 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/map {(api/keyword :view) (api/map {(api/keyword :x) (api/integer 150)
                                                                       (api/keyword :y) (api/integer 50)})})])
                 (api/map {}))

               (defncall 'view-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/key-fn :next)])
                 (api/map {(api/keyword :view) (api/map {(api/keyword :x) (api/integer 150)
                                                         (api/keyword :y) (api/integer 50)})}))


               (defncall 'view-events 'pipes/debug)

               (defncall 'view-delta 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/map {(api/keyword :x)
                                         (api/fn-call (api/symbol '->) [(api/vector [(api/fn-call (api/symbol '->)
                                                                                                 [(api/key-fn :next)
                                                                                                  (api/key-fn :view)
                                                                                                  (api/key-fn :x)])
                                                                                    (api/fn-call (api/symbol '->)
                                                                                                 [(api/key-fn :state)
                                                                                                  (api/key-fn :view)
                                                                                                  (api/key-fn :x)])])
                                                                        (api/symbol 'sum)])
                                         (api/keyword :y)
                                         (api/fn-call (api/symbol '->) [(api/vector [(api/fn-call (api/symbol '->)
                                                                                                 [(api/key-fn :next)
                                                                                                  (api/key-fn :view)
                                                                                                  (api/key-fn :y)])
                                                                                    (api/fn-call (api/symbol '->)
                                                                                                 [(api/key-fn :state)
                                                                                                  (api/key-fn :view)
                                                                                                  (api/key-fn :y)])])
                                                                        (api/symbol 'sum)])
                                         })
                               (api/map {(api/keyword :view) (api/symbol 'id)})])
                 (api/map {(api/keyword :view) (api/map {(api/keyword :x) (api/integer 150)
                                                         (api/keyword :y) (api/integer 50)})}))


               (defncall 'view-state 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                 )
               (defncall 'view-deltas 'pipes/debug ;; (api/keyword :oasis.spec/mouse-state)
                 )

               ;; keep evaluations in state reduction

               (defncall 'is-node-func '->
                 (api/key-fn :caravan/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :caravan/func)]))

               (defncall 'is-node-sink '->
                 (api/key-fn :caravan/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :caravan/sink)]))

               (defncall 'is-def 'or
                 (api/symbol 'is-node-func)
                 (api/symbol 'is-node-sink))

               (defncall 'eval-id '->
                 (api/fn-call (api/symbol 'if) [(api/symbol 'is-def)
                                                (api/key-fn :caravan/name)
                                                (api/symbol 'id)])) ;;FIXME

               (defncall 'index-eval '->
                 (api/vector [(api/vector [(api/symbol 'eval-id) (api/symbol 'id)])])
                 (api/vector [(api/map {}) (api/symbol 'id)])
                 (api/fn-call (api/symbol 'into) []))

               (defncall 'eval-events 'pipes/debug ;; (api/keyword :oasis.spec/eval-state)
                 )

               (defncall 'eval-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})])])
                 (api/map {}))

               (defncall 'eval-raw 'pipes/debug ;; (api/keyword :oasis.spec/eval-state)
                 )

               (defncall 'tag-eval '->
                 (api/map {(api/keyword :eval) (api/symbol 'id)}))

               (defncall 'eval-state 'pipes/debug ;; (api/keyword :oasis.spec/eval-state)
                 )

               ;; commands

               (defncall 'editor-commands 'pipes/debug)
               (defncall 'editor-events 'pipes/debug)
               (defncall 'editor-cooked 'pipes/debug)
               (defncall 'editor-immediate 'pipes/debug)
               (defncall 'editor-state 'pipes/debug)
               (defncall 'select-events 'pipes/debug)

               (defncall 'is-immediate-command '->
                 (api/key-fn :type)
                 (api/fn-call (api/symbol '=) [(api/keyword :immediate)]))
               (defncall 'filter-immediate 'only (api/symbol 'is-immediate-command))


               (defncall 'tag-editor '->
                 (api/map {(api/keyword :editor) (api/symbol 'id)}))

               (defncall 'is-create-sink '->
                 (api/key-fn :command)
                 (api/fn-call (api/symbol '=) [(api/keyword :create-sink)]))

               (defncall 'handle-create-sink '->
                 (api/key-fn :data)
                 (api/fn-call (api/symbol 'create-sink) [])
                 (api/map {(api/keyword :result) (api/symbol 'id)}))

               (defncall 'is-connect '->
                 (api/key-fn :command)
                 (api/fn-call (api/symbol '=) [(api/keyword :connect)]))

               (defncall 'handle-connect '->
                 (api/key-fn :data)
                 (api/fn-call (api/symbol 'connect) [])
                 (api/map {(api/keyword :result) (api/symbol 'id)}))

               (defncall 'is-select '->
                 (api/key-fn :command)
                 (api/fn-call (api/symbol '=) [(api/keyword :select)]))

               (defncall 'handle-select '->
                 (api/key-fn :data)
                 (api/map {(api/keyword :selected)
                           (api/fn-call (api/symbol '->) [(api/key-fn :target)
                                                          (api/key-fn :id)])}))

               (defncall 'is-insert '->
                 (api/key-fn :command)
                 (api/fn-call (api/symbol '=) [(api/keyword :insert)]))

               (defncall 'handle-insert '->
                 (api/key-fn :data)
                 (api/map {(api/keyword :inserted) (api/keyword :none)}))

               (defncall 'is-cursor '->
                 (api/key-fn :command)
                 (api/fn-call (api/symbol '=) [(api/keyword :cursor)]))

               (defncall 'handle-cursor '->
                 (api/key-fn :data)
                 (api/map {(api/keyword :mark)
                           (api/symbol 'id)}))

               (defncall 'is-menu '->
                 (api/key-fn :command)
                 (api/fn-call (api/symbol '=) [(api/keyword :menu)]))

               (defncall 'handle-menu '->
                 (api/key-fn :data)
                 (api/map {(api/keyword :menu) (api/keyword :none)}))

               (defncall 'is-load '->
                 (api/key-fn :command)
                 (api/fn-call (api/symbol '=) [(api/keyword :load)]))

               (defncall 'handle-load '->
                 (api/key-fn :data)
                 (api/fn-call (api/symbol 'spy) [(api/string "load")])
                 (api/symbol 'load-node)
                 (api/map {(api/keyword :load) (api/keyword :none)}))


               (defncall 'handle-commands '->
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
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-command)
                                                    (api/symbol 'ignore)]))

               (defncall 'is-next-mark '->
                 (api/key-fn :next)
                 (api/key-fn :mark))

               (defncall 'calc-down '->
                 (api/key-fn :state)
                 (api/key-fn :mark)
                 (api/symbol 'inc))

               (defncall 'calc-up '->
                 (api/key-fn :state)
                 (api/key-fn :mark)
                 (api/symbol 'dec)
                 (api/vector [(api/symbol 'id) (api/integer 1)])
                 (api/fn-call (api/symbol 'max) []))

               (defncall 'calculate-mark '->
                 (api/fn-call (api/symbol 'if)[(api/fn-call (api/symbol '->) [(api/symbol 'is-next-mark)
                                                                              (api/fn-call (api/symbol '=) [(api/keyword :down)])])
                                               (api/symbol 'calc-down)
                                               (api/symbol 'calc-up)]))

               (defncall 'handle-mark '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/map {(api/keyword :mark)
                                                         (api/symbol 'calculate-mark)})}))

               (defncall 'reset-mark '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/map {(api/keyword :mark) (api/integer 1)
                                                         (api/keyword :selected) (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                                                                (api/key-fn :selected)])})}))

               (defncall 'editor-state-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-mark)
                                                                  (api/symbol 'handle-mark)])
                               (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :selected)])
                                                                  (api/symbol 'reset-mark)])
                               (api/vector [(api/key-fn :state)
                                            (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
                 (api/map {(api/keyword :mark) (api/integer 1)}))

               ;; global state

               (defncall 'state-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
                 (api/map {}))

               (defncall 'state 'pipes/debug ;; (api/keyword :oasis.spec/state)
                 )

               (defncall 'tag-layout '->
                 (api/map {(api/keyword :layout) (api/symbol 'id)}))

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
                 (api/fn-call (api/symbol 'unique) [])
                 (api/symbol 'count)
                 (api/fn-call (api/symbol '=) [(api/integer 1)]))


               (defncall 'cell-rawy '->
                 (api/vector [(api/integer 20)
                              (api/key-fn :counter)])
                 (api/symbol 'mult))

               (defncall 'cell-y '->
                 (api/vector [(api/integer 10)
                              (api/symbol 'cell-rawy)])
                 (api/symbol 'sum))

               (defncall 'cell-x '->
                 (api/vector [(api/integer 15)
                              (api/key-fn :level)])
                 (api/symbol 'mult))

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


               (defncall 'type-pos '->
                 (api/map {(api/keyword :x)
                           (api/fn-call (api/symbol '->) [(api/vector [(api/integer 190)
                                                                       (api/symbol 'cell-x)])
                                                          (api/symbol 'sum)])
                           (api/keyword :y)
                           (api/integer 0)})
                 (api/symbol 'translate-str))

               (defncall 'is-marked-cell '->
                 (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                             (api/key-fn :mark)
                                                             (api/fn-call (api/symbol 'negate) [])])
                              (api/fn-call (api/symbol '->) [(api/key-fn :exp)
                                                             (api/key-fn :counter)])])
                 (api/symbol 'sum)
                 (api/fn-call (api/symbol '=) [(api/integer 0)]))

               (defncall 'is-active-cell '->
                 (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                                (api/key-fn :selected)])
                                                 (api/symbol 'is-marked-cell)]))

               (defncall 'is-active-branch '->
                 (api/key-fn :exp)
                 (api/key-fn :counter)
                 (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '>) [(api/integer 100)])
                                                 (api/fn-call (api/symbol '<) [(api/integer 101)])]))

               (defncall 'get-fill '->
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-active-cell)
                                                    (api/fn-call (api/symbol '->) [(api/symbol 'get-color)
                                                                                   (api/key-fn :cell-active)])])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-active-branch)
                                                    (api/fn-call (api/symbol '->) [(api/symbol 'get-color)
                                                                                   (api/key-fn :cell-seclight)])])
                 (api/fn-call (api/symbol 'incase) [(api/key-fn :exp)
                                                    (api/fn-call (api/symbol '->) [(api/symbol 'get-color)
                                                                                   (api/key-fn :cell-background)])]))

               (defncall 'make-cell '->
                 (api/symbol 'id))


               (defncall 'calc-def-height '->
                 (api/key-fn :caravan/ast)
                 (api/symbol 'count)
                 (api/vector [(api/symbol 'id)
                              (api/integer 20)])
                 (api/symbol 'mult)
                 (api/vector [(api/symbol 'id)
                              (api/integer 50)])
                 (api/symbol 'sum))

               (defncall 'detect-pipe-node '->
                 (api/key-fn :caravan/type)
                 (api/fn-call (api/symbol '=) [(api/keyword :caravan/sink)]))

               (defncall 'format-def '->
                 (api/map {(api/keyword :id) (api/symbol 'def-name)
                           (api/keyword :name) (api/key-fn :caravan/name)
                           (api/keyword :type) (api/key-fn :caravan/type)
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
                 (api/fn-call (api/symbol '=) [(api/keyword :caravan/pipe)]))

               (defncall 'filter-nodes 'filter (api/symbol 'is-def))
               (defncall 'filter-connections 'filter (api/symbol 'is-pipe-eval))

               (defncall 'format-defs '->
                 (api/key-fn :defs)
                 (api/fn-call (api/symbol 'map) [(api/symbol 'format-def)]))

               (defncall 'extract-connection '->
                 (api/vector [(api/map {(api/keyword :from) (api/key-fn :caravan/source)

                                        (api/keyword :to) (api/key-fn :caravan/func)})
                              (api/map {(api/keyword :from) (api/key-fn :caravan/func)
                                        (api/keyword :to) (api/key-fn :caravan/sink)})]))


               (defncall 'format-pipes '->
                 (api/key-fn :pipes)
                 (api/fn-call (api/symbol 'mapcat) [(api/symbol 'extract-connection)])
                 (api/fn-call (api/symbol 'map) [(api/symbol 'format-pipe)]))

               (defncall 'format-state '->
                 (api/key-fn :eval)
                 (api/fn-call (api/symbol 'vals) [])
                 (api/map {(api/keyword :defs) (api/symbol 'filter-nodes)
                           (api/keyword :pipes) (api/symbol 'filter-connections)})
                 (api/map {(api/keyword :id) (api/string "root")
                           (api/keyword :children) (api/symbol 'format-defs)
                           (api/keyword :edges) (api/symbol 'format-pipes)}))


               (defncall 'lay-in 'pipes/debug)

               ;; handling complex state sideeffects

               (defncall 'get-selected-fn-name '->
                 (api/key-fn :state)
                 (api/key-fn :editor)
                 (api/key-fn :selected)
                 (api/fn-call (api/symbol 'drop) [(api/integer 7)]) ;; func/d/
                 (api/fn-call (api/symbol 'join) []))

               (defncall 'get-mark '->
                 (api/key-fn :state)
                 (api/key-fn :editor)
                 (api/key-fn :mark))

               (defncall 'insert-call '->
                 (api/fn-call (api/symbol 'spy) [(api/string "insert-call")])
                 (api/map {(api/keyword :name) (api/symbol 'get-selected-fn-name)
                           (api/keyword :cell) (api/symbol 'get-mark)
                           (api/keyword :uuid) (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                              (api/key-fn :data)])})
                 (api/fn-call (api/symbol 'add-cell) []))

               (defncall 'insert-at-pos '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/map {(api/keyword :editor)
                                                         (api/fn-call (api/symbol '->)
                                                                      [(api/key-fn :state)
                                                                       (api/key-fn :editor)
                                                                       (api/fn-call (api/symbol 'dissoc) [(api/vector [(api/keyword :inserted)])])])
                                                         (api/keyword :result)
                                                         (api/symbol 'insert-call)})}))


               (defncall 'is-insert-state '->
                 (api/key-fn :command)
                 (api/fn-call (api/symbol '=) [(api/keyword :insert)]))


               (defncall 'handle-state 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'is-insert-state)])
                                                                  (api/symbol 'insert-at-pos)])
                               (api/vector [(api/key-fn :state)
                                            (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
                 (api/map {}))


               ;; graphing of nodes

               (defncall 'get-func-stroke '->
                 (api/fn-call (api/symbol 'if) [(api/symbol 'is-selected)
                                                (api/string "#5577ff")
                                                (api/string "darkgrey")]))

               (defncall 'graph-exp '->
                 (api/vector [(api/keyword :g)
                              (api/map {(api/keyword :transform) (api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/symbol 'cell-pos)])})
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :transform) (api/string "translate(-14,-15)")
                                                     (api/keyword :width) (api/integer 198)
                                                     (api/keyword :height) (api/integer 20)
                                                     (api/keyword :style) (api/map {(api/keyword :fill) (api/symbol 'get-fill)
                                                                                    (api/keyword :stroke) (api/string "#f0f0f0")})})])
                              (api/vector [(api/keyword :g)
                                           (api/vector [(api/keyword :text)
                                                        (api/map {(api/keyword :transform)
                                                                  (api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/symbol 'text-pos)])})
                                                        (api/fn-call (api/symbol 'str) [(api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/key-fn :value)])])])
                                           (api/vector [(api/keyword :text)
                                                        (api/map {(api/keyword :transform)
                                                                  (api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/symbol 'type-pos)])
                                                                  (api/keyword :font-style)
                                                                  (api/string "italic")
                                                                  (api/keyword :fill)
                                                                  (api/string "darkgrey")})
                                                        (api/fn-call (api/symbol 'str) [(api/fn-call (api/symbol '->) [(api/key-fn :exp) (api/key-fn :type)])])])])]))

               (defncall 'reduce-exps '->
                 )

               (defncall 'graph-body '->
                 (api/fn-call (api/symbol 'map) [(api/symbol 'graph-exp)])
                 ;; (api/fn-call (api/symbol 'reduce) [(api/symbol 'reduce-exps) (api/symbol 'id)])
                 (api/vector [(api/vector [(api/keyword :g) (api/map {(api/keyword :style) (api/map {(api/keyword :font-family) (api/string "monospace")})})]) (api/symbol 'id)])
                 (api/fn-call (api/symbol 'into) []))

               (defncall 'merge-exp '->
                 (api/map {(api/keyword :exp) (api/fn-call (api/symbol 'nth) [(api/integer 0)])
                           (api/keyword :context) (api/fn-call (api/symbol 'nth) [(api/integer 1)])}))

               (defncall 'make-body-context '->
                 (api/map {(api/keyword :mark) (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                              (api/key-fn :mark)])
                           (api/keyword :selected) (api/symbol 'is-selected)}))

               (defncall 'merge-body '->
                 (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :value)])
                              (api/fn-call (api/symbol '->) [(api/vector [(api/integer 100) ;; FIXME
                                                                          (api/symbol 'make-body-context)])
                                                             (api/fn-call (api/symbol 'repeat) [])])])
                 (api/fn-call (api/symbol 'zip) [])
                 (api/fn-call (api/symbol 'map) [(api/symbol 'merge-exp)]))

               (defncall 'graph-func '->
                 (api/vector [(api/keyword :g)
                              (api/map {(api/keyword :transform)
                                        (api/fn-call (api/symbol '->) [(api/key-fn :node)
                                                                       (api/symbol 'translate-str)])})
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :id) (api/symbol 'func-id)
                                                     (api/keyword :on-click) (api/keyword :click)
                                                     (api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :width)])
                                                     (api/keyword :height) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :height)])
                                                     (api/keyword :style) (api/map {(api/keyword :fill) (api/string "#fff")
                                                                                    (api/keyword :stroke) (api/symbol 'get-func-stroke)
                                                                                    (api/keyword :filter) (api/string "url(#shadow)")
                                                                                    (api/keyword :pointer-events) (api/string "all")})})])
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :transform) (api/string "translate(200,0)")
                                                     (api/keyword :width) (api/integer 100)
                                                     (api/keyword :height) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :height)])
                                                     (api/keyword :style) (api/map {(api/keyword :fill) (api/string "#eee")
                                                                                    (api/keyword :stroke) (api/string "darkgrey")})})])
                              (api/vector [(api/keyword :rect)
                                           (api/map {(api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :node)(api/key-fn :width)])
                                                     (api/keyword :height) (api/integer 30)
                                                     (api/keyword :style) (api/map {(api/keyword :fill) (api/symbol 'get-func-stroke)
                                                                                    (api/keyword :stroke) (api/string "darkgrey")})})])
                              (api/vector [(api/keyword :text)
                                           (api/map {(api/keyword :x) (api/integer 150)
                                                     (api/keyword :y) (api/integer 20)
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
                                           (api/fn-call (api/symbol '->) [(api/symbol 'merge-body) (api/symbol 'graph-body)])])]))

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
                                                     (api/keyword :style) (api/map {(api/keyword :fill) (api/string "#fff")
                                                                                    (api/keyword :stroke) (api/string "darkgrey")
                                                                                    (api/keyword :filter) (api/string "url(#shadow)")
                                                                                    (api/keyword :pointer-events) (api/string "all")})})])
                              (api/vector [(api/keyword :text)
                                           (api/map {(api/keyword :x) (api/integer 50)
                                                     (api/keyword :y) (api/integer 43)
                                                     (api/keyword :dy) (api/integer 14)
                                                     (api/keyword :text-anchor) (api/keyword :middle)
                                                     (api/keyword :font-weight) (api/string "bold")})
                                           (api/key-fn :name)])]))

               (defncall 'is-pipe-node '->
                 (api/key-fn :node)
                 (api/key-fn :type)
                 (api/fn-call (api/symbol '=) [(api/string "sink")]))

               (defncall 'is-func-node '->
                 (api/key-fn :node)
                 (api/key-fn :value))

               (defncall 'graph-node '->
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-pipe-node)
                                                    (api/symbol 'graph-pipe)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-func-node)
                                                    (api/symbol 'graph-func)]))

               (defncall 'merge-node '->
                 (api/map {(api/keyword :node) (api/fn-call (api/symbol 'nth) [(api/integer 0)])
                           (api/keyword :context) (api/fn-call (api/symbol 'nth) [(api/integer 1)])}))

               (defncall 'graph-nodes '->
                 (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :layout)
                                                             (api/key-fn :children)])
                              (api/fn-call (api/symbol '->) [(api/vector [(api/integer 100)
                                                                          (api/key-fn :context)])
                                                             (api/fn-call (api/symbol 'repeat) [])])])
                 (api/fn-call (api/symbol 'zip) [])
                 (api/fn-call (api/symbol 'map) [(api/symbol 'merge-node)])
                 (api/fn-call (api/symbol 'map) [(api/symbol 'graph-node)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :g)])]))

               (defncall 'graph-connection '->
                 (api/key-fn :sections)
                 (api/fn-call (api/symbol 'nth) [(api/integer 0)])
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
                 (api/fn-call (api/symbol 'map) [(api/symbol 'graph-connection)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :g)])]))


               (defncall 'graph '->
                 (api/map {(api/keyword :layout) (api/key-fn :layout)
                           (api/keyword :view) (api/key-fn :view)
                           (api/keyword :context) (api/key-fn :editor)})
                 (api/map {(api/keyword :graph)
                           (api/vector [(api/keyword :g)
                                        (api/map {(api/keyword :transform) (api/fn-call (api/symbol '->) [(api/key-fn :view) (api/symbol 'translate-str)])})
                                        (api/symbol 'graph-nodes)
                                        (api/symbol 'graph-connections)])}))


               ;; reduce elements to latest version of GUI element

               (defncall 'elements-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'into) []) ])
                 (api/map {}))

               (defncall 'svg-elements-reduce 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'concat) [(api/map {})]) ])
                 (api/map {}))

               (defncall 'reducer 'pipes/debug (api/keyword :oasis.spec/gui))
               (defncall 'render 'pipes/debug (api/keyword :oasis.spec/render)
                 )

               ;; render elements to hiccup
               (defncall 'render-elements '->
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "render")])

                 (api/fn-call (api/symbol 'vals) [])
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "vals")])
                 ;; (api/fn-call (api/symbol 'sort-by [(api/symbol 'id)]))
                 (api/fn-call (api/symbol 'map) [(api/key-fn :oasis.gui/element)])
                 (api/fn-call (api/symbol 'concat) [(api/vector [(api/keyword :div)])]))

               (defncall 'svg-reduced 'pipes/debug  ;; (api/keyword :oasis.spec/render)
                 )

               (defncall 'svg-defs '->
                 (api/vector [(api/keyword :defs)
                              (api/vector [(api/keyword :filter)
                                           (api/map {(api/keyword :id) (api/string "shadow")})
                                           (api/vector [(api/keyword :feDropShadow)
                                                        (api/map {(api/keyword :dx) (api/string "3")
                                                                  (api/keyword :dy) (api/string "3")
                                                                  (api/keyword :flood-color) (api/string "#333333")
                                                                  (api/keyword :flood-opacity) (api/string "0.3")
                                                                  (api/keyword :stdDeviation) (api/string "3")})])])
                              (api/vector [(api/keyword :filter)
                                           (api/map {(api/keyword :id) (api/string "leftshadow")})
                                           (api/vector [(api/keyword :feDropShadow)
                                                        (api/map {(api/keyword :dx) (api/string "-3")
                                                                  (api/keyword :dy) (api/string "3")
                                                                  (api/keyword :flood-color) (api/string "#333333")
                                                                  (api/keyword :flood-opacity) (api/string "0.3")
                                                                  (api/keyword :stdDeviation) (api/string "3")})])])
                              (api/vector [(api/keyword :filter)
                                           (api/map {(api/keyword :id) (api/string "upshadow")})
                                           (api/vector [(api/keyword :feDropShadow)
                                                        (api/map {(api/keyword :dx) (api/string "0")
                                                                  (api/keyword :dy) (api/string "-3")
                                                                  (api/keyword :flood-color) (api/string "#333333")
                                                                  (api/keyword :flood-opacity) (api/string "0.3")
                                                                  (api/keyword :stdDeviation) (api/string "3")})])])]))

               ;; render SVG components
               (defncall 'svg-render 'pipes/debug)
               (defncall 'render-svg '->
                 (api/map {(api/keyword :svg)
                           (api/map {(api/keyword :oasis.gui/order)
                                     (api/integer 2)
                                     (api/keyword :oasis.gui/element)
                                     (api/vector [(api/keyword :svg)
                                                  (api/map {(api/keyword :width) (api/integer 1200)
                                                            (api/keyword :height) (api/integer 800)})
                                                  (api/symbol 'svg-defs)
                                                  (api/vector [(api/keyword :rect)
                                                               (api/map {(api/keyword :id) (api/string "background")
                                                                         (api/keyword :width) (api/integer 1200)
                                                                         (api/keyword :height) (api/integer 800)
                                                                         (api/keyword :fill) (api/string "#eee")
                                                                         (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "all")})})
                                                               ])
                                                  (api/key-fn :graph)
                                                  (api/key-fn :source-menu)
                                                  (api/key-fn :sink-menu)
                                                  (api/key-fn :action-menu)])})}))

               (pipe 'd 'log)
               (pipe 'ui 'log)

               (red 'mouse 'mouse-reduce 'mouse-state)

               (pipe 'keyboard 'filter-cursor 'editor-commands)
               (pipe 'keyboard 'filter-menu 'editor-commands)
               (pipe 'keyboard 'log-keyboard)

                ;;                (pipe 'reduced-events 'only-complete 'events)
                ;;                (pipe 'raw-events 'input-reduce 'reduced-events)

               ;; (pipe 'events 'log-events)
                ;;                (pipe 'events 'make-eval 'ev)
               ;; (pipe 'events 'make-eval 'log)

                ;;                (pipe 'ui 'filter-input 'raw-events)
                ;;                (pipe 'ui 'filter-submit 'raw-events)

               (pipe 'ui 'filter-select 'editor-commands)
               ;; (pipe 'ui 'filter-select 'select-events)
               ;; (pipe 'select-events 'editor-commands)

               (pipe 'n 'index-eval 'eval-events)
               (red 'eval-events 'eval-reduce 'eval-raw)
               (pipe 'eval-raw 'tag-eval 'eval-state)


               (pipe 'mouse-state 'filter-drag 'drag-events)

               (pipe 'drag-events 'filter-drag-end 'drag-state)

               (pipe 'drag-state 'log-mouse)
               (pipe 'drag-state 'interpret-drag 'editor-commands)

               (pipe 'editor-commands 'handle-commands 'editor-events)
               (red 'editor-events 'editor-state-reduce 'editor-cooked)
               (pipe 'editor-cooked 'tag-editor 'editor-state)
               (pipe 'editor-state 'log-editor)

               (pipe 'editor-commands 'filter-immediate 'editor-immediate)
               (pipe 'editor-immediate 'log-command)

               (pipe 'mouse-state 'filter-scroll 'scroll-state)
               (pipe 'scroll-state 'construct-view 'view-deltas)
               (red 'view-deltas 'view-delta 'view-events)
               (red 'view-events 'view-reduce 'view-state)

               (red 'editor-state 'state-reduce 'state)
               (red 'eval-state 'state-reduce 'state)
               (red 'view-state 'state-reduce 'state)
               ;; (red 'mouse-state 'state-reduce 'state)
               (pipe 'state 'log-state)

               (pipe 'eval-state 'format-state 'layout)
               (pipe 'eval-state 'format-state 'log-layout)

               (pipe 'layout 'tag-layout 'layout-state)
                               ;; (pipe 'layout-state 'log-layout)
               (red 'layout-state 'state-reduce 'state)

               (red 'select-events 'center-view 'view-events)
               (red 'layout-state 'center-view 'view-events)

               (red 'editor-state 'handle-state 'log-command)
               (red 'editor-immediate 'handle-state 'log-command)
                               ;; (red 'editor-commands 'handle-state 'log-command)

               (pipe 'state 'graph 'svg-render)
               (red 'render 'elements-reduce 'reducer)
               ;; (red 'render 'elements-reduce 'log-render)

               ;; (pipe 'reducer 'render-elements 'log)
               ;; (pipe 'reducer 'log)

               (pipe 'reducer 'render-elements 'ui)

               (red 'svg-render 'svg-elements-reduce 'svg-reduced)
               (pipe 'svg-reduced 'render-svg 'render)

               (pipe 'oasis 'source-menu-const 'source-menu-items)
               (red 'source-menu-items 'source-menu-map 'source-menu)
               (pipe 'source-menu 'render-source-menu 'svg-render)

               (pipe 'oasis 'sink-menu-const 'sink-menu-items)
               (red 'sink-menu-items 'sink-menu-map 'sink-menu)
               (pipe 'sink-menu 'render-sink-menu 'svg-render)

                ;;                (pipe 'state 'render-action-menu 'svg-render)

                               (pipe 'oasis 'header 'render)
                ;;                (pipe 'oasis 'repl 'render)
               (pipe 'oasis 'view-events)
               (pipe 'oasis 'log)

               ]]
               oasis))

(defn store [db]
  (db/parse-tree->db! db (start)))
