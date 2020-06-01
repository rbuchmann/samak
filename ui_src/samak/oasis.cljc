(ns ^:figwheel-no-load samak.oasis
  #?@
  (:clj
   [(:require
     [clojure.spec.alpha :as s]
     [samak.api :as api]
     [samak.code-db :as db]
     [samak.nodes :as n]
     [samak.pipes :as pipes]
     [samak.runtime.stores :as stores]
     samak.spec
     [samak.stdlib :as std])]
   :cljs
   [(:require
     [cljs.spec.alpha :as s]
     [samak.api :as api]
     [samak.code-db :as db]
     [samak.nodes :as n]
     [samak.pipes :as pipes]
     [samak.runtime.stores :as stores]
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
   (api/pipe (api/symbol in) (api/symbol out)))
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

(def oasis1   [(defncall 'oasis-layout 'pipes/layout)
               (defncall 'oasis-eval 'pipes/eval-notify)

               (defncall 'm-caravan 'modules/caravan)
               (defncall 'caravan-actions '->
                 (api/symbol 'm-caravan)
                 (api/key-fn :sinks)
                 (api/key-fn :actions))
               (defncall 'caravan-commands '->
                 (api/symbol 'm-caravan)
                 (api/key-fn :sources)
                 (api/key-fn :commands))

               (defncall 'log 'pipes/log)

               (defncall 'log-command 'pipes/log (api/string "cmd: "))
               (defncall 'log-layout 'pipes/log (api/string "layout: "))
               (defncall 'log-events 'pipes/log (api/string "events: "))
               (defncall 'log-editor 'pipes/log (api/string "editor: "))
               (defncall 'log-mouse 'pipes/log (api/string "mouse: "))
               (defncall 'log-hover 'pipes/log (api/string "hover: "))
               (defncall 'log-keyboard 'pipes/log (api/string "keyboard: "))
               (defncall 'log-caravan 'pipes/log (api/string "caravan: "))

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
                              [(api/symbol 'is-submit)
                               (api/symbol 'handle-submit)
                               (api/symbol 'ignore)]))

               (defncall 'is-resize '->
                 (api/fn-call (api/symbol '=) [(api/key-fn :data)
                                               (api/keyword :resize)]))

               (defncall 'filter-resize '->
                 (api/fn-call (api/symbol 'if) [(api/symbol 'is-resize)
                                                (api/map {(api/keyword :resize) (api/map {(api/keyword :width) (api/key-fn :width)
                                                                                          (api/keyword :height) (api/fn-call (api/symbol '-) [(api/key-fn :height) (api/integer 56)])})})
                                                (api/symbol 'ignore)]))

               (defncall 'is-lmb-click-event '->
                 (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '->) [(api/key-fn :data)
                                                                                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :click)])])
                                                 (api/fn-call (api/symbol '->) [(api/key-fn :event)
                                                                                (api/key-fn :button)
                                                                                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :primary)])])]))

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

               (defncall 'fn-name-from-select '->
                 (api/fn-call (api/symbol 'str-split) [(api/symbol '_) (api/string "/")]) ;; func/
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-valid-target)
                                                    (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])]))


               ;; repl

               (defmap 'repl
                 {(api/keyword :repl)
                  (api/map {(api/keyword :oasis.gui/order)
                            (api/integer 10)
                            (api/keyword :oasis.gui/element)
                            (api/vector [(api/keyword :form) (api/map {(api/keyword :on-submit) (api/keyword :submit)})
                                         (api/vector [(api/keyword :input) (api/map {(api/keyword :on-change) (api/keyword :change)
                                                                                     (api/keyword :id) (api/string "input/repl")
                                                                                     (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "auto")})})])])})})

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

               (defncall 'get-mouse-y '-
                 (api/key-fn :samak.mouse/page-y)
                 (api/integer 56))

               (defncall 'calculate-mouse-delta-y '->
                 (api/fn-call (api/symbol '+) [(api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                              (api/symbol 'get-mouse-y)])
                                               (api/fn-call (api/symbol '-) [(api/fn-call (api/symbol '->) [(api/key-fn :state)
                                                                                                            (api/key-fn :mouse)
                                                                                                            (api/key-fn :position)
                                                                                                            (api/key-fn :y)
                                                                                                            ])])]))

               (defncall 'handle-mouse-move '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/map {(api/keyword :position)
                                                         (api/map {(api/keyword :x) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :samak.mouse/page-x)])
                                                                   (api/keyword :y) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'get-mouse-y)])})
                                                         (api/keyword :delta)
                                                         (api/map {(api/keyword :x)
                                                                   (api/symbol 'calculate-mouse-delta-x)
                                                                   (api/keyword :y)
                                                                   (api/symbol 'calculate-mouse-delta-y)})
                                                         (api/keyword :drag)
                                                         (api/fn-call (api/symbol '->) [(api/key-fn :state) (api/key-fn :mouse) (api/key-fn :drag)
                                                                                        (api/map {(api/keyword :active) (api/key-fn :active)
                                                                                                  (api/keyword :source) (api/key-fn :source)
                                                                                                  (api/keyword :begin) (api/keyword :false)
                                                                                                  (api/keyword :button) (api/key-fn :button)})])})}))

               (defncall 'is-mouse-down '->
                 (api/key-fn :next)
                 (api/key-fn :samak.mouse/type)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :down)]))

               (defncall 'handle-mouse-down '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/fn-call (api/symbol '->)
                                                            [(api/key-fn :next)
                                                             (api/map {(api/keyword :drag) (api/map {(api/keyword :begin) (api/keyword :true)
                                                                                                     (api/keyword :active) (api/keyword :true)
                                                                                                     (api/keyword :button) (api/key-fn :samak.mouse/button)
                                                                                                     (api/keyword :source) (api/key-fn :samak.mouse/target)})

                                                                       (api/keyword :position) (api/map {(api/keyword :x) (api/key-fn :samak.mouse/page-x)
                                                                                                      (api/keyword :y) (api/symbol 'get-mouse-y)})
                                                                       (api/keyword :start) (api/map {(api/keyword :x) (api/key-fn :samak.mouse/page-x)
                                                                                                      (api/keyword :y) (api/symbol 'get-mouse-y)})})])}))

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
                 (api/key-fn :drag)
                 (api/key-fn :end)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :end)]))

               (defncall 'is-drag-start '->
                 (api/key-fn :drag)
                 (api/key-fn :begin)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :true)]))

               (defncall 'is-drag-or-end 'or
                 (api/symbol 'is-drag)
                 (api/fn-call (api/symbol '->) [(api/key-fn :mouse) (api/symbol 'is-drag-end)]))

               (defncall 'is-drag-end-or-start 'or
                 (api/fn-call (api/symbol '->) [(api/key-fn :mouse) (api/symbol 'is-drag-start)])
                 (api/fn-call (api/symbol '->) [(api/key-fn :mouse) (api/symbol 'is-drag-end)]))

               (defncall 'filter-drag-end-or-start 'only (api/symbol 'is-drag-end-or-start))

               (defncall 'filter-drag 'only (api/symbol 'is-drag-or-end))

               (defncall 'drag-events 'pipes/debug)
               (defncall 'drag-state 'pipes/debug)

               ;; (defncall 'drag-reduce 'pipes/reductions
               ;;   (api/fn-call (api/symbol '->)
               ;;                [(api/vector [(api/key-fn :state) (api/key-fn :next)])
               ;;                 (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]) ])
               ;;   (api/map {}))


               ;; keyboard handling

               (defncall 'is-phase-up '->
                 (api/key-fn :phase)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :up)]))

               (defncall 'is-target-input '->
                 (api/key-fn :type)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "INPUT")]))

               (defncall 'is-key-common '->
                 (api/key-fn :which)
                 (api/fn-call (api/symbol '>) [(api/integer 31) (api/symbol '_)]))

               (defncall 'keyboard-filtered 'pipes/debug)

               (defncall 'filter-key-input 'except (api/fn-call (api/symbol 'and) [(api/symbol 'is-target-input)
                                                                                   (api/symbol 'is-key-common)]))

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


               ;; (defncall 'construct-mode '->
               ;;   (api/map {(api/keyword :command) (api/keyword :mode)
               ;;             (api/keyword :data) (api/symbol '_)}))

               (defncall 'construct-activity '->
                 (api/map {(api/keyword :command) (api/keyword :activity)
                           (api/keyword :data) (api/symbol '_)}))

               (defncall 'construct-insert-mode '->
                 (api/keyword :insert)
                 (api/symbol 'construct-activity))

               (defncall 'construct-edit-mode '->
                 (api/keyword :edit)
                 (api/symbol 'construct-activity))

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
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-phase-up)
                                                    (api/fn-call (api/symbol '->) [(api/map {(api/keyword :key) (api/keyword :ignore)})])])
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
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "o")]))

               (defncall 'construct-load '->
                 (api/map {(api/keyword :command) (api/keyword :load)
                           (api/keyword :type) (api/keyword :immediate)
                           (api/keyword :data) (api/keyword :load)}))

               (defncall 'is-kb-test '->
                 (api/key-fn :key)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "t")]))

               (defncall 'construct-test '->
                 (api/map {(api/keyword :command) (api/keyword :test)
                           (api/keyword :type) (api/keyword :immediate)
                           (api/keyword :data) (api/keyword :test)}))

               (defncall 'is-kb-trace '->
                 (api/key-fn :key)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "l")]))

               (defncall 'construct-trace '->
                 (api/map {(api/keyword :command) (api/keyword :trace)
                           (api/keyword :type) (api/keyword :immediate)
                           (api/keyword :data) (api/keyword :trace)}))

               (defncall 'filter-menu '->
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-phase-up)
                                                    (api/fn-call (api/symbol '->) [(api/map {(api/keyword :key) (api/keyword :ignore)})])])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-menu)
                                                    (api/symbol 'construct-menu)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-load)
                                                    (api/symbol 'construct-load)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-test)
                                                    (api/symbol 'construct-test)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-trace)
                                                    (api/symbol 'construct-trace)])
                 (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                    (api/symbol 'ignore)]))


               (defncall 'is-kb-move-left '->
                 (api/key-fn :key)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "ArrowLeft")]))

               (defncall 'is-kb-move-right '->
                 (api/key-fn :key)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "ArrowRight")]))

               (defncall 'is-kb-move-up '->
                 (api/key-fn :key)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "ArrowUp")]))

               (defncall 'is-kb-move-down '->
                 (api/key-fn :key)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "ArrowDown")]))

               (defncall 'construct-move-left '->
                 (api/map {(api/keyword :command) (api/keyword :move)
                           (api/keyword :x) (api/integer 50)
                           (api/keyword :y) (api/integer 0)
                           (api/keyword :zoom) (api/integer 1)}))

               (defncall 'construct-move-right '->
                 (api/map {(api/keyword :command) (api/keyword :move)
                           (api/keyword :x) (api/integer -50)
                           (api/keyword :y) (api/integer 0)
                           (api/keyword :zoom) (api/integer 1)}))

               (defncall 'construct-move-up '->
                 (api/map {(api/keyword :command) (api/keyword :move)
                           (api/keyword :x) (api/integer 0)
                           (api/keyword :y) (api/integer 50)
                           (api/keyword :zoom) (api/integer 1)}))

               (defncall 'construct-move-down '->
                 (api/map {(api/keyword :command) (api/keyword :move)
                           (api/keyword :x) (api/integer 0)
                           (api/keyword :y) (api/integer -50)
                           (api/keyword :zoom) (api/integer 1)}))

               (defncall 'is-kb-zoom-in '->
                 (api/key-fn :key)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "+")]))

               (defncall 'is-kb-zoom-out '->
                 (api/key-fn :key)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "-")]))

               (defncall 'construct-zoom-out '->
                 (api/map {(api/keyword :command) (api/keyword :zoom)
                           (api/keyword :zoom) (api/float 0.9)}))

               (defncall 'construct-zoom-in '->
                 (api/map {(api/keyword :command) (api/keyword :zoom)
                           (api/keyword :zoom) (api/float 1.1)}))



               (defncall 'filter-view '->
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-move-left)
                                                    (api/symbol 'construct-move-left)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-move-right)
                                                    (api/symbol 'construct-move-right)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-move-up)
                                                    (api/symbol 'construct-move-up)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-move-down)
                                                    (api/symbol 'construct-move-down)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-phase-up)
                                                    (api/fn-call (api/symbol '->) [(api/map {(api/keyword :key) (api/keyword :ignore)})])])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-zoom-in)
                                                    (api/symbol 'construct-zoom-in)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-kb-zoom-out)
                                                    (api/symbol 'construct-zoom-out)])
                 (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                    (api/symbol 'ignore)]))

               (defncall 'make-zoom 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/map {(api/keyword :zoom) (api/fn-call (api/symbol '*) [(api/fn-call (api/symbol '->) [(api/key-fn :state) (api/key-fn :zoom)])
                                                                                           (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :zoom)])])})])
                 (api/map {(api/keyword :zoom) (api/integer 1)}))

               (defncall 'make-view-move 'pipes/reductions
                 (api/fn-call (api/symbol '->)
                              [(api/key-fn :next)
                               ;; (api/map {(api/keyword :x) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :x)])
                               ;;           (api/keyword :y) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :zoom)])})
                               ])
                 (api/map {(api/keyword :x) (api/integer 0)
                           (api/keyword :y) (api/integer 0)}))

               (defncall 'is-command '->
                 (api/key-fn :command))


               (defncall 'is-sink '->
                 (api/key-fn :type)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "sink")]))

               (defncall 'handle-sink '->
                 (api/key-fn :name)
                 (api/map {(api/keyword :command) (api/keyword :create-sink)
                           (api/keyword :data) (api/map {(api/keyword :name) (api/symbol '_)})}))

               (defncall 'is-source '->
                 (api/key-fn :type)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "source")]))

               (defncall 'handle-source '->
                 (api/key-fn :name)
                 (api/map {(api/keyword :command) (api/keyword :create-sink)
                           (api/keyword :data) (api/map {(api/keyword :name) (api/symbol '_)})}))

               (defncall 'is-func '->
                 (api/key-fn :type)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/string "func")]))

               (defncall 'handle-func '->
                 (api/key-fn :name)
                 (api/map {(api/keyword :command) (api/keyword :select)
                           (api/keyword :data) (api/fn-call (api/symbol 'str) [(api/string "func/")(api/symbol '_)])}))

               (defncall 'handle-mouse-click '->
                 (api/key-fn :source)
                 (api/symbol 'make-target)
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-source)
                                                    (api/symbol 'handle-source)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-sink)
                                                    (api/symbol 'handle-sink)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-func)
                                                    (api/symbol 'handle-func)])
                 (api/fn-call (api/symbol 'unless) [(api/key-fn :command)
                                                    (api/map {(api/keyword :command) (api/keyword :mode)
                                                              (api/keyword :data) (api/keyword :back)})]))

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

               (defncall 'is-start '->
                 (api/fn-call (api/symbol 'and) [(api/symbol 'is-lmb-event)
                                                 (api/symbol 'is-source-source)
                                                 (api/fn-call (api/symbol '=) [(api/key-fn :begin) (api/keyword :true)])]))

               (defncall 'handle-drag-start '->
                 (api/map {(api/keyword :command) (api/keyword :activity)
                           (api/keyword :data) (api/keyword :dragging)}))

               (defncall 'interpret-drag '->
                 (api/key-fn :mouse)
                 (api/key-fn :drag)
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-lmb-click)
                                                    (api/symbol 'handle-mouse-click)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'is-start)
                                                    (api/symbol 'handle-drag-start)])
                 (api/fn-call (api/symbol 'incase) [(api/symbol 'both-pipe)
                                                    (api/symbol 'handle-mouse-connect)])
                 (api/fn-call (api/symbol 'unless) [(api/symbol 'is-command)
                                                    (api/map {(api/keyword :command) (api/keyword :activity)
                                                              (api/keyword :data) (api/keyword :scouting)})]))

               (defncall 'handle-caravan-command '->
                 (api/symbol 'construct-back))

               (defncall 'is-scroll '->
                 (api/key-fn :mouse)
                 (api/key-fn :drag)
                 (api/key-fn :button)
                 (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :secondary)]))

               (defncall 'filter-scroll 'only (api/fn-call (api/symbol 'and) [(api/symbol 'is-scroll)
                                                                              (api/symbol 'is-drag-or-end)]))

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
               (defncall 'zoom-events 'pipes/debug)

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
                              [(api/map {(api/keyword :org) (api/symbol '_)
                                         ;; (api/keyword :log) (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/fn-call (api/symbol 'spy) [(api/string "eval red")])])
                                         })
                               (api/key-fn :org)
                               (api/vector [(api/key-fn :state) (api/key-fn :next)])
                               (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
                 (api/map {}))

               (defncall 'eval-raw 'pipes/debug ;; (api/keyword :oasis.spec/eval-state)
                 )

               (defncall 'tag-eval '->
                 ;; (api/fn-call (api/symbol 'spy) [(api/string "evaled")])
                 (api/map {(api/keyword :eval) (api/symbol '_)}))

               (defncall 'eval-state 'pipes/debug ;; (api/keyword :oasis.spec/eval-state)
                 )])
(def oasis2 [
              ;; commands

              (defncall 'editor-commands 'pipes/debug)
              (defncall 'editor-events 'pipes/debug)
              (defncall 'editor-cooked 'pipes/debug)
              (defncall 'editor-immediate 'pipes/debug)
              (defncall 'editor-actions 'pipes/debug)
              (defncall 'editor-state 'pipes/debug)
              (defncall 'select-events 'pipes/debug)

              (defncall 'be-commands 'pipes/debug)

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
                (api/map {(api/keyword :result) (api/symbol '_)
                          (api/keyword :mode) (api/keyword :back)}))

              (defncall 'is-select '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :select)]))

              (defncall 'handle-select '->
                (api/key-fn :data)
                (api/map {(api/keyword :selected) (api/symbol '_)}))

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
                (api/map {(api/keyword :mode) (api/keyword :load)
                          (api/keyword :load) (api/keyword :none)}))

              (defncall 'is-test '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :test)]))

              (defncall 'handle-test '->
                (api/key-fn :data)
                (api/map {(api/keyword :mode) (api/keyword :test)
                          (api/keyword :test) (api/keyword :none)}))

              (defncall 'is-trace '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :trace)]))

              (defncall 'handle-trace '->
                (api/key-fn :data)
                (api/map {(api/keyword :mode) (api/keyword :trace)
                          (api/keyword :trace) (api/keyword :none)}))

              (defncall 'is-mode-change '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :mode)]))

              (defncall 'handle-mode '->
                (api/key-fn :data)
                (api/map {(api/keyword :mode) (api/symbol '_)}))

               (defncall 'is-activity-change '->
                (api/key-fn :command)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :activity)]))

              (defncall 'handle-activity '->
                (api/key-fn :data)
                (api/map {(api/keyword :activity) (api/symbol '_)}))


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
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-test)
                                                   (api/symbol 'handle-test)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-trace)
                                                   (api/symbol 'handle-trace)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mode-change)
                                                   (api/symbol 'handle-mode)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-activity-change)
                                                   (api/symbol 'handle-activity)])
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

               (defncall 'is-mode-focus '->
                (api/key-fn :state)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :focus)]))

              (defncall 'is-activity-insert '->
                (api/key-fn :state)
                (api/key-fn :activity)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :insert)]))

               (defncall 'is-activity-edit '->
                (api/key-fn :state)
                (api/key-fn :activity)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :edit)]))

              (defncall 'process-cursor '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mode-focus)
                                                   (api/symbol 'change-mark)])
                ;; (api/fn-call (api/symbol 'incase) [(api/symbol 'is-mode-insert)
                ;;                                    (api/symbol 'change-mark)])
                )

              ;; convert and layout nodes

              (defncall 'def-name 'str
                (api/string "d/")
                (api/key-fn :caravan/name))

              (defncall 'detect-pipe-node '->
                (api/key-fn :caravan/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/sink)]))

              (defncall 'calc-def-height '->
                (api/key-fn :caravan/ast)
                (api/fn-call (api/symbol 'count) [(api/symbol '_)])
                (api/fn-call (api/symbol '*) [(api/symbol '_) (api/integer 20)])
                (api/fn-call (api/symbol '+) [(api/symbol '_) (api/integer 50)]))

             (defncall 'format-def '->
               (api/map {(api/keyword :id) (api/fn-call (api/symbol '->) [(api/key-fn :def) (api/symbol 'def-name)])
                         (api/keyword :name) (api/fn-call (api/symbol '->) [(api/key-fn :def) (api/key-fn :caravan/name)])
                         (api/keyword :type) (api/fn-call (api/symbol '->) [(api/key-fn :def) (api/key-fn :caravan/type)])
                         ;; (api/keyword :display) (api/key-fn :caravan/display)
                         (api/keyword :value) (api/fn-call (api/symbol '->) [(api/key-fn :def) (api/key-fn :caravan/ast)])
                         (api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :def) (api/fn-call (api/symbol 'if) [(api/symbol 'detect-pipe-node)
                                                                                                                              (api/integer 100)
                                                                                                                              (api/integer 300)])])
                         (api/keyword :height) (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol '->) [(api/key-fn :def) (api/symbol 'detect-pipe-node)])
                                                                              (api/integer 100)
                                                                              (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol '>) [(api/key-fn :context) (api/key-fn :zoom) (api/integer 1)])
                                                                                                             (api/fn-call (api/symbol '->) [(api/key-fn :def) (api/symbol 'calc-def-height)])
                                                                                                             (api/integer 30)])])
                         (api/keyword :size) (api/fn-call (api/symbol '->) [(api/key-fn :def) (api/fn-call (api/symbol 'if) [(api/symbol 'detect-pipe-node)
                                                                                                                             (api/integer 100)
                                                                                                                             (api/symbol 'calc-def-height)])])}))

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
                                                        (api/keyword :mode) (api/keyword :focus)
                                                        (api/keyword :activity) (api/keyword :focus)
                                                        (api/keyword :selected) (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                                                               (api/key-fn :selected)])
                                                        (api/keyword :ast) (api/symbol 'selected-source)
                                                        (api/keyword :formatted) (api/fn-call (api/symbol '->) [(api/map {(api/keyword :def) (api/symbol 'selected-source)})
                                                                                                                (api/symbol 'format-def)])})}))

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
                                                        (api/keyword :ast) (api/symbol 'selected-source-change)
                                                        (api/keyword :formatted) (api/fn-call (api/symbol '->) [(api/map {(api/keyword :def) (api/symbol 'selected-source-change)})
                                                                                                                (api/symbol 'format-def)])})}))

              (defncall 'is-next-mode '->
                (api/key-fn :next)
                (api/key-fn :mode))

              (defncall 'mode-set-navigate '->
                (api/map {(api/keyword :state) (api/key-fn :state)
                          (api/keyword :next) (api/map {(api/keyword :mode) (api/keyword :navigate)
                                                        (api/keyword :activity) (api/keyword :scouting)})}))

              (defncall 'is-next-back '->
                (api/key-fn :next)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :back)]))

              (defncall 'is-next-edit '->
                (api/key-fn :next)
                (api/key-fn :activity)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :edit)]))

              (defncall 'is-next-focus '->
                (api/key-fn :next)
                (api/key-fn :mode)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :focus)]))

              (defncall 'mode-set-focus '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/map {(api/keyword :mode) (api/keyword :focus)
                                                         (api/keyword :activity) (api/keyword :focus)})}))

              (defncall 'mode-set-back '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-activity-insert)
                                                   (api/symbol 'mode-set-focus)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-activity-edit)
                                                   (api/symbol 'mode-set-focus)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-back) ;; FIXME: also unset mark and selected
                                                   (api/symbol 'mode-set-navigate)]))

              (defncall 'is-cell-editable '->
                (api/key-fn :state)
                (api/map {(api/keyword :map) (api/fn-call (api/symbol '->) [(api/key-fn :ast)
                                                                            (api/key-fn :caravan/ast)])
                          (api/keyword :key) (api/fn-call (api/symbol '->) [(api/key-fn :mark)
                                                                            (api/fn-call (api/symbol 'dec) [(api/symbol '_)])])})
                (api/fn-call (api/symbol 'lookup)  [(api/key-fn :map)
                                                    (api/key-fn :key)
                                                    (api/keyword :none)])
                (api/key-fn :type)
                (api/fn-call (api/symbol 'or) [(api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/str)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/kw)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/acc)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/int)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/float)])
                                               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/func)])])
                )

               (defncall 'activity-set-edit '->
                (api/fn-call (api/symbol 'unless) [(api/symbol 'is-cell-editable)
                                                   (api/symbol 'mode-set-focus)]))

              (defncall 'process-mode '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-focus)
                                                   (api/symbol 'mode-set-focus)])
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-back)
                                                   (api/symbol 'mode-set-back)]))

               (defncall 'is-next-activity '->
                (api/key-fn :next)
                (api/key-fn :activity))

               (defncall 'process-activity '->
                (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-edit)
                                                   (api/symbol 'activity-set-edit)]))

               (defncall 'is-next-drag-end '->
                 (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                (api/key-fn :mouse)
                                                (api/key-fn :drag)
                                                (api/key-fn :end)]))

               (defncall 'process-drag '->
                 (api/map {(api/keyword :state) (api/key-fn :state)
                           (api/keyword :next) (api/map {(api/keyword :activity)
                                                         (api/fn-call (api/symbol 'if) [(api/symbol 'is-next-drag-end)
                                                                                        (api/keyword :scouting)
                                                                                        (api/keyword :scrolling)])})}))

              (defncall 'editor-state-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-mode)
                                                                 (api/symbol 'process-mode)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-activity)
                                                                 (api/symbol 'process-activity)])
                              (api/fn-call (api/symbol 'incase) [(api/symbol 'is-next-cursor)
                                                                 (api/symbol 'process-cursor)])
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :selected)])
                                                                 (api/symbol 'process-select)])
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :eval)])
                                                                 (api/symbol 'process-eval)])
                              (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :mouse)])
                                                                 (api/symbol 'process-drag)])
                              (api/vector [(api/key-fn :state)
                                           (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
                (api/map {(api/keyword :mode) (api/keyword :navigate)
                          (api/keyword :activity) (api/keyword :scouting)
                          (api/keyword :mark) (api/integer 1)
                          (api/keyword :eval) (api/map {})
                          (api/keyword :ast) (api/map {})
                          (api/keyword :formatted) (api/map {})
                          (api/keyword :hovered) (api/map {})
                          (api/keyword :hover) (api/vector [])}))

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

             (defncall 'tag-menu-source '->
               (api/map {(api/keyword :source-menu) (api/symbol '_)}))

             (defncall 'reduce-menu-source 'pipes/reductions
               (api/fn-call (api/symbol '->)
                            [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                             (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
               (api/map {(api/keyword :items) (api/vector [])
                         (api/keyword :hover) (api/map {})}))

             (defncall 'tag-sink-menu '->
               (api/map {(api/keyword :sink-menu) (api/symbol '_)}))

             (defncall 'reduce-menu-sink 'pipes/reductions
               (api/fn-call (api/symbol '->)
                            [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                             (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
               (api/map {(api/keyword :items) (api/vector [])
                         (api/keyword :hover) (api/map {})
                         (api/keyword :resize) (api/map {})}))


              ;; global state

              (defncall 'load-state 'pipes/debug ;; (api/keyword :oasis.spec/state)
                )
              (defncall 'loaded-state 'pipes/debug ;; (api/keyword :oasis.spec/state)
                )

              (defncall 'load-reduce 'pipes/reductions
                (api/fn-call (api/symbol '->)
                             [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                              (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]) ])
                (api/map {(api/keyword :zoom) (api/integer 1)}))

             (defncall 'filter-load 'except
               (api/fn-call (api/symbol '->) [
                                              (api/fn-call (api/symbol '->) [(api/key-fn :mode) (api/key-fn :mode)])
                                              (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :load)])]))


              (defncall 'tag-layout '->
                (api/map {(api/keyword :layout) (api/key-fn :success)}))

              (defncall 'layout-state 'pipes/debug ;; (api/keyword :oasis.spec/state)
                )


              (defncall 'make-cell '->
                (api/symbol '_))



              (defncall 'pipe-name 'str
                (api/string "p/")
                (api/key-fn :from)
                (api/string "-")
                (api/key-fn :to))


              (defncall 'format-pipe '->
                (api/map {(api/keyword :id) (api/symbol 'pipe-name)
                          (api/keyword :from) (api/key-fn :from)
                          (api/keyword :to) (api/key-fn :to)
                          (api/keyword :source) (api/fn-call (api/symbol 'str)
                                                             [(api/string "d/")
                                                              (api/key-fn :from)])
                          (api/keyword :target) (api/fn-call (api/symbol 'str)
                                                             [(api/string "d/")
                                                              (api/key-fn :to)])}))



              (defncall 'is-pipe-eval '->
                (api/key-fn :caravan/type)
                (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/pipe)]))

             (defncall 'filter-nodes '->
               (api/key-fn :eval)
               (api/fn-call (api/symbol 'filter) [(api/symbol 'is-def)
                                                  (api/symbol '_)]))

              (defncall 'filter-connections '->
                (api/key-fn :eval)
                (api/fn-call (api/symbol 'filter) [(api/symbol 'is-pipe-eval)
                                                  (api/symbol '_)]))

             (defncall 'merge-defs '->
               (api/map {(api/keyword :def) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])
                         (api/keyword :context) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])}))

              (defncall 'format-defs '->
                (api/fn-call (api/symbol 'myzip) [(api/key-fn :defs)
                                                  (api/fn-call (api/symbol 'repeat) [(api/fn-call (api/symbol 'count) [(api/key-fn :defs)])
                                                                                     (api/key-fn :context)])])
                (api/fn-call (api/symbol 'map) [(api/symbol 'merge-defs) (api/symbol '_)])
                (api/fn-call (api/symbol 'map) [(api/symbol 'format-def) (api/symbol '_)]))

             (defncall 'extract-connection '->
               (api/fn-call (api/symbol 'if) [(api/key-fn :caravan/func)
                                              (api/vector [(api/map {(api/keyword :from) (api/key-fn :caravan/source)

                                                                     (api/keyword :to) (api/key-fn :caravan/func)})
                                                           (api/map {(api/keyword :from) (api/key-fn :caravan/func)
                                                                     (api/keyword :to) (api/key-fn :caravan/sink)})])
                                              (api/vector [(api/map {(api/keyword :from) (api/key-fn :caravan/source)

                                                                     (api/keyword :to) (api/key-fn :caravan/sink)})])]))

             (defncall 'format-pipes '->
               (api/fn-call (api/symbol 'mapcat) [(api/symbol 'extract-connection) (api/key-fn :pipes)])
               (api/fn-call (api/symbol 'map) [(api/symbol 'format-pipe) (api/symbol '_)]))

             (defncall 'format-state '->
               (api/map {(api/keyword :eval) (api/fn-call (api/symbol '->) [(api/key-fn :eval)
                                                                            (api/fn-call (api/symbol 'vals) [(api/symbol '_)])])
                         (api/keyword :context) (api/symbol '_)})
                (api/map {(api/keyword :defs) (api/symbol 'filter-nodes)
                          (api/keyword :pipes) (api/symbol 'filter-connections)
                          (api/keyword :context) (api/key-fn :context)})
               (api/map {(api/keyword :id) (api/string "root")
                         (api/keyword :layoutOptions) (api/map {(api/string "elk.algorithm") (api/string "layered")})
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
                                                                              (api/fn-call (api/symbol 'inc) [(api/symbol '_)])])})
               (api/symbol 'swap-at-pos))

             (defncall 'leap-at-pos '->
               (api/map {(api/keyword :state) (api/key-fn :state)
                         (api/keyword :next) (api/key-fn :next)
                         (api/keyword :target) (api/fn-call (api/symbol '->) [(api/symbol 'get-mark)
                                                                              (api/fn-call (api/symbol 'dec) [(api/symbol '_)])])})
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

             (defncall 'should-load '->
               (api/key-fn :next)
               (api/key-fn :data)
               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :load)]))

             (defncall 'load '->
               (api/map {(api/keyword :state) (api/key-fn :state)
                         (api/keyword :next) (api/map {(api/keyword :call)
                                                       (api/map {(api/keyword :action)
                                                                 (api/keyword :load)})})}))

             (defncall 'should-test '->
               (api/key-fn :next)
               (api/key-fn :data)
               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :test)]))

             (defncall 'test '->
               (api/map {(api/keyword :state) (api/key-fn :state)
                         (api/keyword :next) (api/map {(api/keyword :call)
                                                       (api/map {(api/keyword :action)
                                                                 (api/keyword :test)})})}))

             (defncall 'should-trace '->
               (api/key-fn :next)
               (api/key-fn :data)
               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :trace)]))

             (defncall 'trace '->
               (api/map {(api/keyword :state) (api/key-fn :state)
                         (api/keyword :next) (api/map {(api/keyword :call)
                                                       (api/map {(api/keyword :action)
                                                                 (api/keyword :trace)})})}))

             (defncall 'should-insert '->
               (api/key-fn :next)
               (api/symbol 'is-insert-state))

             (defncall 'is-editor-activity-insert '->
               (api/key-fn :state)
               (api/key-fn :editor)
               (api/key-fn :activity)
               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :insert)]))

             (defncall 'should-edit '->
               (api/key-fn :next)
               (api/key-fn :data)
               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :accept)]))

             (defncall 'is-editor-activity-edit '->
               (api/key-fn :state)
               (api/key-fn :editor)
               (api/key-fn :activity)
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

             (defncall 'action-at-pos '->
               (api/fn-call (api/symbol 'incase) [(api/symbol 'should-fall)
                                                  (api/symbol 'fall-at-pos)])
               (api/fn-call (api/symbol 'incase) [(api/symbol 'should-leap)
                                                  (api/symbol 'leap-at-pos)])
               (api/fn-call (api/symbol 'incase) [(api/symbol 'should-indent)
                                                  (api/symbol 'indent-at-pos)])
               (api/fn-call (api/symbol 'incase) [(api/symbol 'should-cut)
                                                  (api/symbol 'cut-at-pos)]))

             (defncall 'handle-state 'pipes/reductions
               (api/fn-call (api/symbol '->)
                            [(api/symbol 'reset-call)
                             ;; (api/fn-call (api/symbol 'spy) [(api/string "handle-state")])

                             (api/fn-call (api/symbol 'incase) [(api/symbol 'should-load)
                                                                (api/symbol 'load)])
                             (api/fn-call (api/symbol 'incase) [(api/symbol 'should-test)
                                                                (api/symbol 'test)])
                             (api/fn-call (api/symbol 'incase) [(api/symbol 'should-trace)
                                                                (api/symbol 'trace)])
                             (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol 'and) [(api/symbol 'should-insert)
                                                                                                (api/symbol 'is-editor-activity-insert)])
                                                                (api/symbol 'insert-at-pos)])
                             (api/fn-call (api/symbol 'incase) [(api/symbol 'is-editor-activity-edit)
                                                                (api/symbol 'action-at-pos)])

                             (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol 'and) [(api/symbol 'should-edit)
                                                                                                (api/symbol 'is-editor-activity-edit)])
                                                                (api/symbol 'edit-at-pos)])
                             (api/vector [(api/key-fn :state)
                                          (api/key-fn :next)])
                             (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
               (api/map {}))

             (defncall 'add-insert-actions '->
               (api/fn-call (api/symbol 'assoc-in) [(api/symbol '_)
                                                    (api/vector [(api/keyword :next)
                                                                 (api/keyword :actions)])
                                                    (api/vector [(api/string "Q back")
                                                                 (api/string "1 string")
                                                                 (api/string "2 number")
                                                                 (api/string "3 keyword")
                                                                 (api/string "4 symbol")
                                                                 (api/string "5 float")
                                                                 (api/string "6 table")
                                                                 (api/string "7 list")
                                                                 (api/string "8 accessor")
                                                                 (api/string "9 function")])]))

             (defncall 'is-change-insert '->
               (api/key-fn :editor)
               (api/key-fn :activity)
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
               (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-hover-source)
                                                   (api/vector [(api/string "LMB construct")])])
               (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-hover-sink)
                                                   (api/vector [(api/string "LMB construct")])])
               (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-hover-node)
                                                   (api/vector [(api/string "DRAG connect")])])
               (api/fn-call (api/symbol 'incase)  [(api/symbol 'is-hover-func)
                                                   (api/vector [(api/string "LMB select")])]))

             (defncall 'construct-mouse-actions '->
               (api/vector [(api/symbol 'construct-lmb-action)
                            (api/string "RMB pan")])
               (api/fn-call (api/symbol 'remove) [(api/key-fn :type) (api/symbol '_)]))


             (defncall 'construct-key-actions '->
               (api/vector [(api/string "+- zoom")])
               (api/fn-call (api/symbol 'remove) [(api/key-fn :editor) (api/symbol '_)]))

             (defncall 'add-nav-actions '->
               (api/fn-call (api/symbol 'assoc-in) [(api/symbol '_)
                                                    (api/vector [(api/keyword :next)
                                                                 (api/keyword :actions)])
                                                    (api/fn-call (api/symbol 'flatten)
                                                                 [(api/vector [(api/symbol 'construct-key-actions)
                                                                               (api/symbol 'construct-mouse-actions)])])]))

             (defncall 'is-change-navigate '->
               (api/key-fn :editor)
               (api/key-fn :mode)
               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :navigate)]))

             (defncall 'add-edit-actions '->
               (api/fn-call (api/symbol 'assoc-in) [(api/symbol '_)
                                                    (api/vector [(api/keyword :next)
                                                                 (api/keyword :actions)])
                                                    (api/vector [(api/string "Q back")
                                                                 (api/string "1 edit")])]))

             (defncall 'is-change-edit '->
               (api/key-fn :editor)
               (api/key-fn :activity)
               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :edit)]))

             (defncall 'add-focus-actions '->
               (api/fn-call (api/symbol 'spy) [(api/string "focus")])
               (api/fn-call (api/symbol 'assoc-in) [(api/symbol '_)
                                                    (api/vector [(api/keyword :next)
                                                                 (api/keyword :actions)])
                                                    (api/vector [(api/string "WS navigate")
                                                                 (api/string "F insert")
                                                                 (api/string "D indent")
                                                                 (api/string "Shift-WS Swap")
                                                                 (api/string "X cut")
                                                                 (api/string "Q back")])]))

             (defncall 'is-change-focus '->
               (api/key-fn :editor)
               (api/key-fn :mode)
               (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :focus)]))

             (defncall 'mode-data 'pipes/reductions
               (api/fn-call (api/symbol '->)
                            [
               ;; (api/fn-call (api/symbol 'spy) [(api/string "mode")])

                             (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'is-change-insert)])
                                                                (api/symbol 'add-insert-actions)])
                             (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'is-change-navigate)])
                                                                (api/symbol 'add-nav-actions)])
                             (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'is-change-edit)])
                                                                (api/symbol 'add-edit-actions)])
                             (api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '->) [(api/key-fn :next) (api/symbol 'is-change-focus)])
                                                                (api/symbol 'add-focus-actions)])

                             (api/fn-call (api/symbol 'assoc-in) [(api/symbol '_)
                                                                  (api/vector [(api/keyword :next)
                                                                               (api/keyword :mode)])
                                                                  (api/fn-call (api/symbol '->) [(api/key-fn :next) (api/key-fn :editor) (api/key-fn :mode)])])
                             (api/fn-call (api/symbol 'update-in) [(api/symbol '_) (api/vector [(api/keyword :next) (api/keyword :editor)]) (api/keyword :removed)])

                             (api/vector [(api/key-fn :state)
                                          (api/key-fn :next)])
                             (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]) ])
               (api/map {(api/keyword :mode) (api/keyword :unknown)
                         (api/keyword :actions) (api/vector [])}))

             (defncall 'tag-mode '->
               (api/map {(api/keyword :mode) (api/symbol '_)}))

             (defncall 'init-view '->
               (api/map {(api/keyword :zoom) (api/integer 1)
                         (api/keyword :x) (api/integer 150)
                         (api/keyword :y) (api/integer 50)}))

             (defncall 'm-caravan-actions 'caravan-actions (api/integer 42))
             ])

(def oasis-core-defs
  [(defncall 'oasis-core-init 'pipes/debug)
   (defncall 'oasis-kb 'pipes/debug)
   (defncall 'oasis-mouse 'pipes/debug)
   (defncall 'oasis-core-out 'pipes/debug)
   (api/defmodule 'oasis-core (api/map {;;(api/keyword :depends) (api/map {(api/keyword :caravan) (api/symbol 'modules/caravan)})
                                        (api/keyword :sources) (api/map {;; (api/keyword :caravan) (api/symbol 'm-caravan)
                                                                         (api/keyword :eval) (api/symbol 'oasis-eval)
                                                                         (api/keyword :layout) (api/symbol 'oasis-layout)
                                                                         (api/keyword :init) (api/symbol 'oasis-core-init)
                                                                         (api/keyword :kb) (api/symbol 'oasis-kb)
                                                                         (api/keyword :mouse) (api/symbol 'oasis-mouse)
                                                                         (api/keyword :state) (api/symbol 'oasis-core-out)})
                                        (api/keyword :sinks) (api/map {(api/keyword :state) (api/symbol 'oasis-core-out)})}))])


(def oasis-core-net
  [              ;; networks

   (pipe 'oasis-mouse 'mouse-reduce)
   (pipe 'mouse-reduce 'mouse-state)
   (pipe 'oasis-mouse 'target-reduce)
   (pipe 'target-reduce 'target-events)

   (pipe 'target-events 'only-different 'hover-events)
   (pipe 'hover-events 'tag-hover 'hover-state)

   (pipe 'oasis-kb 'filter-key-input 'keyboard-filtered)
   (pipe 'keyboard-filtered 'filter-edit 'editor-commands)
   (pipe 'keyboard-filtered 'filter-menu 'editor-commands)
   ;; (pipe 'keyboard-filtered 'log-keyboard)

   (pipe 'keyboard-filtered 'filter-view 'view-commands)
   (pipe 'view-commands 'make-zoom)
   (pipe 'make-zoom 'zoom-events)
   (pipe 'zoom-events 'view-events)
   (pipe 'view-commands 'view-deltas)

   (pipe 'raw-events 'input-reduce)
   (pipe 'input-reduce 'reduced-events)
   (pipe 'raw-events 'tag-events 'events)

   ;; (pipe 'oasis-ev 'filter-input 'raw-events)
   ;; (pipe 'oasis-ev 'filter-submit 'raw-events)

   ;; (pipe 'oasis-ui-out 'filter-resize 'events)

   ;; (pipe 'select-events 'editor-commands)

   (pipe 'oasis-eval 'eval-events)

   ;; (pipe 'eval-events 'log-events)
   (pipe 'eval-events 'eval-reduce)
   (pipe 'eval-reduce 'eval-raw)
   (pipe 'eval-raw 'tag-eval 'eval-state)

   (pipe 'mouse-state 'filter-drag 'drag-events)

   (pipe 'drag-events 'filter-drag-end-or-start 'drag-state) ;; FIXME reduce
   ;; (pipe 'drag-state 'log-mouse)
   (pipe 'drag-state 'interpret-drag 'editor-commands)
   ;; (pipe 'drag-events 'log-mouse)

   (api/pipe (api/fn-call (api/symbol 'caravan-commands) [(api/integer 42)])
             (api/symbol 'handle-caravan-command)
             (api/symbol 'editor-commands))
   ;; (pipe 'caravan 'handle-caravan-command 'editor-commands)
   (pipe 'editor-commands 'handle-commands 'editor-events)
   (pipe 'hover-state 'editor-events)
   (pipe 'scroll-state 'editor-events)
   (pipe 'editor-events 'editor-state-reduce)
   (pipe 'editor-state-reduce 'editor-cooked)
   (pipe 'editor-cooked 'tag-editor 'editor-state)
   ;; (pipe 'editor-state 'log-editor)

   (pipe 'editor-commands 'filter-immediate 'editor-immediate)
   ;; (pipe 'editor-immediate 'log-command)

   (pipe 'mouse-state 'filter-scroll 'scroll-state)
   (pipe 'scroll-state 'construct-view 'view-deltas)
   (pipe 'view-deltas 'view-delta)
   (pipe 'view-delta 'view-events)
   (pipe 'view-events 'view-reduce)
   (pipe 'view-reduce 'view-raw)
   (pipe 'view-raw 'tag-view 'view-state)

   (pipe 'oasis-core-init 'source-menu-const 'source-menu-items)
   (pipe 'source-menu-items 'source-menu-map)
   (pipe 'source-menu-map 'source-menu)
   (pipe 'source-menu 'tag-items 'source-menu-events)
   (pipe 'hover-state 'source-menu-events)
   (pipe 'source-menu-events 'reduce-menu-source)
   (pipe 'reduce-menu-source 'source-menu-state)
   (pipe 'source-menu-state 'tag-menu-source 'state-dedupe)

   (pipe 'oasis-core-init 'sink-menu-const 'sink-menu-items)
   (pipe 'sink-menu-items 'sink-menu-map)
   (pipe 'sink-menu-map 'sink-menu)
   (pipe 'sink-menu 'tag-items 'sink-menu-events)
   (pipe 'hover-state 'sink-menu-events)
   (pipe 'events 'sink-menu-events)
   (pipe 'sink-menu-events 'reduce-menu-sink)
   (pipe 'reduce-menu-sink 'sink-menu-state)
   (pipe 'sink-menu-state 'tag-sink-menu 'state-dedupe)

   (pipe 'editor-state 'state-dedupe)
   (pipe 'loaded-state 'state-dedupe)
   (pipe 'view-state 'state-dedupe)
   (pipe 'drag-events 'state-dedupe)
   (pipe 'hover-state 'state-dedupe)
   (pipe 'mode-state 'state-dedupe)
   (pipe 'events 'state-dedupe)
   (pipe 'state-dedupe 'filter-state 'oasis-core-out)

   (pipe 'mode-state 'load-reduce)
   (pipe 'hover-state 'load-reduce)
   (pipe 'eval-state 'load-reduce)
   (pipe 'zoom-events 'load-reduce)
   (pipe 'load-reduce 'load-state)
   (pipe 'load-state 'filter-load 'loaded-state)

   (pipe 'loaded-state 'format-state 'oasis-layout)
   ;; (pipe 'eval-state 'format-state 'log-layout)

   (pipe 'eval-state 'edit-information 'editor-events)

   (pipe 'oasis-layout 'tag-layout 'layout-state)
   (pipe 'layout-state 'log-layout)
   (pipe 'layout-state 'state-dedupe)

   ;; (pipe 'select-events 'center-view)
   ;; (pipe 'layout-state 'center-view)
   ;; (pipe 'center-view 'view-events)

   (pipe 'editor-state 'mode-data)
   (pipe 'mode-data 'mode-raw)
   (pipe 'mode-raw 'tag-mode 'mode-state)

   (pipe 'editor-state 'editor-actions)
   (pipe 'editor-immediate 'editor-actions)
   (pipe 'mode-state 'editor-actions)
   (pipe 'events 'editor-actions)

   (pipe 'editor-actions 'handle-state)
   (pipe 'handle-state 'be-commands)
   ;; (pipe 'be-commands 'log-command)
   (api/pipe (api/symbol 'be-commands)
             (api/symbol 'filter-call)
             (api/symbol 'm-caravan-actions))


   (pipe 'oasis-core-init 'init-view 'view-events)

   (api/pipe (api/fn-call (api/symbol 'caravan-commands) [(api/integer 42)])
             (api/symbol 'log-caravan))
   ])

(def oasis-render-defs
  [
   (defncall 'oasis-render-in 'pipes/debug)
   (defncall 'oasis-render-out 'pipes/debug)

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
      (api/keyword :pipe-glow) (api/string "#6684e1")
      (api/keyword :pipe-stroke) (api/string "#a6a28c")
      (api/keyword :pipe-drag) (api/string "#b65611")
      (api/keyword :edge-in) (api/string "#6684e1")
      (api/keyword :edge-out) (api/string "#b65611")
      (api/keyword :edge-neutral) (api/string "#a6a28c")
      (api/keyword :graph-background) (api/string "#20201d")
      (api/keyword :shadow-flood) (api/string "#292824")
      (api/keyword :menu-entry-bg) (api/string "#999580")
      (api/keyword :menu-entry-active-bg) (api/string "#a6a28c")
      (api/keyword :menu-entry-text) (api/string "#fefbec")
      (api/keyword :menu-bar-bg) (api/string "#6e6b5e")
      (api/keyword :menu-bar-text) (api/string "#a6a28c")
      })

   (defmap 'get-font
     {(api/string "str") (api/string "serif")})

   (defmap 'get-syntax-color
     {(api/keyword :caravan/str) (api/map {(api/keyword :cell-content) (api/string "#60ac39")
                                           (api/keyword :cell-active-content) (api/string "#60ac39")})
      (api/keyword :caravan/kw) (api/map {(api/keyword :cell-content) (api/string "#b65611")
                                          (api/keyword :cell-active-content) (api/string "#b65611")})
      (api/keyword :caravan/int) (api/map {(api/keyword :cell-content) (api/string "#1fad83")
                                           (api/keyword :cell-active-content) (api/string "#1fad83")})
      (api/keyword :caravan/float) (api/map {(api/keyword :cell-content) (api/string "#1fad83")
                                             (api/keyword :cell-active-content) (api/string "#1fad83")})
      (api/keyword :caravan/acc) (api/map {(api/keyword :cell-content) (api/string "#ae9513")
                                           (api/keyword :cell-active-content) (api/string "#ae9513")})
      (api/keyword :caravan/func) (api/map {(api/keyword :cell-content) (api/string "#6684e1")
                                            (api/keyword :cell-active-content) (api/string "#6684e1")})
      (api/keyword :caravan/table) (api/map {(api/keyword :cell-content) (api/string "#d43552")
                                             (api/keyword :cell-active-content) (api/string "#d43552")})
      (api/keyword :caravan/list) (api/map {(api/keyword :cell-content) (api/string "#b854d4")
                                            (api/keyword :cell-active-content) (api/string "#b854d4")})})


   (defncall 'translate-str 'str
     (api/string "translate(")
     (api/fn-call (api/symbol 'or) [(api/key-fn :x) (api/integer 0)])
     (api/string ",")
     (api/fn-call (api/symbol 'or) [(api/key-fn :y) (api/integer 0)])
     (api/string ")"))

   (defncall 'translate-graph-str 'str
     (api/string "matrix(")
     (api/fn-call (api/symbol '*) [(api/fn-call (api/symbol 'nth) [(api/key-fn :matrix) (api/integer 0)])
                                   (api/key-fn :zoom)])
     (api/string ",")
     (api/fn-call (api/symbol '*) [(api/fn-call (api/symbol 'nth) [(api/key-fn :matrix) (api/integer 1)])
                                   (api/key-fn :zoom)])
     (api/string ",")
     (api/fn-call (api/symbol '*) [(api/fn-call (api/symbol 'nth) [(api/key-fn :matrix) (api/integer 2)])
                                   (api/key-fn :zoom)])
     (api/string ",")
     (api/fn-call (api/symbol '*) [(api/fn-call (api/symbol 'nth) [(api/key-fn :matrix) (api/integer 3)])
                                   (api/key-fn :zoom)])
     (api/string ",")
     (api/key-fn :x)
     (api/string ",")
     (api/key-fn :y)
     (api/string ")"))

   (defncall 'translate-dialog '->
     (api/map {(api/keyword :x)
               (api/integer 150)
               (api/keyword :y)
               (api/integer 50)
               (api/keyword :zoom)
               (api/float 1.5)
               (api/keyword :matrix)
               (api/vector [(api/float  1.0)
                            (api/float  0.0)
                            (api/float  0.0)
                            (api/float  1.0)])})
     (api/symbol 'translate-graph-str))

   (defncall 'translate-func '->
     (api/fn-call (api/symbol 'into) [(api/symbol '_)
                                      (api/map {(api/keyword :zoom)
                                                (api/float 1.5)
                                                ;; (api/keyword :x)
                                                ;; (api/integer 150)
                                                ;; (api/keyword :y)
                                                ;; (api/integer -60)
                                                (api/keyword :matrix)
                                                (api/vector [(api/float 1.0)
                                                             (api/float 0.0)
                                                             (api/float 0.0)
                                                             (api/float 1.0)])})])
     (api/symbol 'translate-graph-str))

   (defncall 'translate-blur '->
     (api/fn-call (api/symbol 'into) [(api/symbol '_)
                                      (api/map {(api/keyword :zoom)
                                                (api/float 2.0)
                                                (api/keyword :x)
                                                (api/fn-call (api/symbol '-) [(api/key-fn :x) (api/integer 100)])
                                                ;; (api/keyword :y)
                                                ;; (api/integer -50)
                                                (api/keyword :matrix)
                                                (api/vector [(api/float 1.0)
                                                             (api/float 0.0)
                                                             (api/float 0.0)
                                                             (api/float 1.0)])})])
     (api/symbol 'translate-graph-str))

   (defncall 'translate-graph '->
     (api/fn-call (api/symbol 'assoc) [(api/symbol '_) (api/keyword :matrix)
                                       (api/vector [(api/float  1.0)
                                                    (api/float -0.5)
                                                    (api/float  1.0)
                                                    (api/float  0.5)])])
     (api/symbol 'translate-graph-str))

   (defncall 'translate-ident '->
     (api/fn-call (api/symbol 'into) [(api/symbol '_)
                                      (api/map {(api/keyword :zoom)
                                                (api/float 1.0)
                                                (api/keyword :matrix)
                                                (api/vector [(api/float 1.0)
                                                             (api/float 0.0)
                                                             (api/float 0.0)
                                                             (api/float 1.0)])})])
     (api/symbol 'translate-graph-str))

   ;; FIXME: menu

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
                  (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                 (api/key-fn :hover)
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
                                         (api/keyword :r) (api/integer 40)
                                         (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#shadow)")
                                                                        (api/keyword :pointer-events) (api/string "all")})
                                         (api/keyword :fill) (api/fn-call (api/symbol 'get-color) [(api/keyword :menu-entry-bg)])
                                         (api/keyword :stroke) (api/fn-call (api/symbol 'get-color) [(api/keyword :menu-entry-text)])})])
                  (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :circle)
                                                                (api/map {(api/keyword :cx) (api/integer 0)
                                                                          (api/keyword :cy) (api/integer 45)
                                                                          (api/keyword :r) (api/integer 35)
                                                                          (api/keyword :stroke-width) (api/integer 2)
                                                                          (api/keyword :stroke) (api/fn-call (api/symbol '->) [(api/keyword :element-highlight-stroke)
                                                                                                                               (api/symbol 'get-color)])
                                                                          (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/symbol 'get-entry-bg)
                                                                                                                             (api/symbol 'get-color)])})])
                                                   (api/symbol 'get-animation-style)])
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
                                         (api/keyword :r) (api/integer 40)
                                         (api/keyword :fill-opacity) (api/integer 0)})])]))

   (defncall 'get-menu-fill '->
     (api/keyword :menu-bar-bg)
     (api/symbol 'get-color))

   (defncall 'get-menu-fg '->
     (api/keyword :menu-bar-text)
     (api/symbol 'get-color))


   (defncall 'tag-item-context '->
     (api/map {(api/keyword :item) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])
               (api/keyword :context) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])}))

   (defncall 'get-menu-items '->
     (api/key-fn :items)
     (api/fn-call (api/symbol 'map) [(api/symbol 'tag-item-context) (api/symbol '_)])
     (api/fn-call (api/symbol 'map) [(api/symbol 'render-menu-entry) (api/symbol '_)])
     (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :g)]) (api/symbol '_)])
     )


   (defncall 'render-source-menu '->
     (api/key-fn :source-menu)
     (api/map {(api/keyword :items) (api/key-fn :items)
               (api/keyword :context) (api/map {(api/keyword :hover) (api/key-fn :hover)
                                                (api/keyword :resize) (api/key-fn :resize)})})
     (api/map {(api/keyword :items) (api/fn-call (api/symbol 'myzip) [(api/key-fn :items)
                                                                      (api/fn-call (api/symbol 'repeat) [(api/fn-call (api/symbol 'count) [(api/key-fn :items)])
                                                                                                         (api/key-fn :context)])])
               (api/keyword :context) (api/key-fn :context)})
     (api/vector [(api/keyword :g)
                  (api/vector [(api/keyword :rect)
                               (api/map {(api/keyword :id) (api/string "menu/source")
                                         (api/keyword :height) (api/string "100%")
                                         (api/keyword :width) (api/integer 100)
                                         (api/keyword :stroke) (api/symbol 'get-menu-fg)
                                         (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#shadow)")
                                                                        (api/keyword :pointer-events) (api/string "all")})
                                         (api/keyword :fill-opacity) (api/float 0.8)
                                         (api/keyword :fill) (api/symbol 'get-menu-fill)})])
                  (api/symbol 'get-menu-items)])
     (api/map {(api/keyword :source-menu) (api/symbol '_)}))

   (defncall 'get-sink-position '->
     (api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :resize) (api/key-fn :width)])
     (api/fn-call (api/symbol '-) [(api/symbol '_) (api/integer 100)])
     (api/map {(api/keyword :x) (api/symbol '_)})
     (api/symbol 'translate-str))

   (defncall 'render-sink-menu '->
     (api/key-fn :sink-menu)
     (api/map {(api/keyword :items) (api/key-fn :items)
               (api/keyword :context) (api/map {(api/keyword :hover) (api/key-fn :hover)
                                                (api/keyword :resize) (api/key-fn :resize)})})
     (api/map {(api/keyword :items) (api/fn-call (api/symbol 'myzip) [(api/key-fn :items)
                                                                      (api/fn-call (api/symbol 'repeat) [(api/fn-call (api/symbol 'count) [(api/key-fn :items)])
                                                                                                         (api/key-fn :context)])])
               (api/keyword :context) (api/key-fn :context)})
     (api/vector [(api/keyword :g)
                  (api/map {(api/keyword :transform) (api/symbol 'get-sink-position)})
                  (api/vector [(api/keyword :rect)
                               (api/map {(api/keyword :id) (api/string "menu/sink")
                                         (api/keyword :height) (api/string "100%")
                                         (api/keyword :width) (api/integer 100)
                                         (api/keyword :stroke) (api/symbol 'get-menu-fg)
                                         (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#leftshadow)")
                                                                        (api/keyword :pointer-events) (api/string "all")})
                                         (api/keyword :fill-opacity) (api/float 0.8)
                                         (api/keyword :fill) (api/symbol 'get-menu-fill)})])
                  (api/symbol 'get-menu-items)])
     (api/map {(api/keyword :sink-menu) (api/symbol '_)}))

   (defncall 'get-menu-state '->
     (api/vector [(api/string "mode: ")
                  (api/fn-call (api/symbol '->) [(api/key-fn :editor)
                                                 (api/key-fn :mode)])
                  (api/string "/")
                  (api/fn-call (api/symbol '->) [(api/key-fn :editor)
                                                 (api/key-fn :activity)])])
     (api/fn-call (api/symbol 'str-join) [(api/string " ") (api/symbol '_)]))

   (defncall 'render-menu-action '->
     ;; (api/fn-call (api/symbol 'spy) [(api/string "render action")])
     (api/symbol '_))

   (defncall 'get-menu-actions '->
     (api/fn-call (api/symbol '->) [(api/key-fn :mode)
                                    (api/key-fn :actions)
                                    (api/fn-call (api/symbol 'map) [(api/symbol 'render-menu-action) (api/symbol '_)])])
     (api/fn-call (api/symbol 'str-join) [(api/string ", ") (api/symbol '_)])
     (api/vector [(api/string "type: ") (api/symbol '_)])
     (api/fn-call (api/symbol 'str-join) [(api/string "") (api/symbol '_)]))

   (defncall 'get-action-position '->
     (api/fn-call (api/symbol '->) [(api/key-fn :resize) (api/key-fn :height)])
     (api/fn-call (api/symbol '-) [(api/symbol '_) (api/integer 50)])
     (api/map {(api/keyword :y) (api/symbol '_)})
     (api/symbol 'translate-str))

   (defncall 'render-action-menu '->
     (api/map {(api/keyword :action-menu)
               (api/vector [(api/keyword :g)
                            (api/map {(api/keyword :transform) (api/symbol 'get-action-position)})
                            (api/vector [(api/keyword :rect)
                                         (api/map {(api/keyword :id) (api/string "menu/action")
                                                   (api/keyword :height) (api/integer 50)
                                                   (api/keyword :width) (api/string "100%")
                                                   (api/keyword :stroke) (api/symbol 'get-menu-fg)
                                                   (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#upshadow)")
                                                                                  (api/keyword :pointer-events) (api/string "all")})
                                                   (api/keyword :fill-opacity) (api/float 0.8)
                                                   (api/keyword :fill) (api/symbol 'get-menu-fill)})])
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


   ;; state

   (defncall 'state-reduce 'pipes/reductions
     (api/fn-call (api/symbol '->)
                  [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                   (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)]) ])
     (api/map {}))

   (defncall 'state-dedupe 'pipes/reductions
     (api/fn-call (api/symbol '->)
                  [(api/map {(api/keyword :next) (api/key-fn :next)
                             (api/keyword :state) (api/fn-call (api/symbol '->) [(api/key-fn :state) (api/key-fn :state)])
                             (api/keyword :key) (api/fn-call (api/symbol 'first) [(api/fn-call (api/symbol 'keys) [(api/key-fn :next)])])})
                   (api/map {(api/keyword :next) (api/key-fn :next)
                             (api/keyword :state) (api/key-fn :state)
                             (api/keyword :existing) (api/fn-call (api/symbol 'lookup) [(api/key-fn :state) (api/key-fn :key) (api/keyword :not-found)])})
                   (api/map {(api/keyword :dupe) (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol '=) [(api/key-fn :existing) (api/key-fn :next)])
                                                                                (api/keyword :dupe)
                                                                                (api/keyword :unique)])
                             (api/keyword :state) (api/fn-call (api/symbol 'into) [(api/map {}) (api/vector [(api/key-fn :state) (api/key-fn :next)])])})])
     (api/map {(api/keyword :state) (api/map {})}))

   (defncall 'filter-state '->
     (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol '=) [(api/key-fn :dupe) (api/keyword :unique)])
                                    (api/key-fn :state)
                                    (api/fn-call (api/symbol '->) [(api/fn-call (api/symbol 'spy) [(api/string "DROP")])
                                                                   (api/symbol 'ignore)])]))

   (defncall 'condensed-state 'pipes/debug ;; (api/keyword :oasis.spec/state)
     )

   (defncall 'only-resize '->
     (api/map {(api/keyword :resize) (api/key-fn :resize)}))


   ;; cell handling

   (defncall 'func-id 'str
     (api/string "func/")
     (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :name)]))

   (defncall 'is-same '->
     (api/fn-call (api/symbol 'distinct) [(api/symbol '_)])
     (api/fn-call (api/symbol 'count) [(api/symbol '_)])
     (api/fn-call (api/symbol '=) [(api/symbol '_) (api/integer 1)]))

   (defncall 'is-selected '->
     (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :selected)])
                  (api/symbol 'func-id)])
     (api/symbol 'is-same))

   (defncall 'is-hovered '->
     (api/vector [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :hovered) (api/key-fn :name)])
                  (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :name)])])
     (api/symbol 'is-same))

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
     ;; (api/fn-call (api/symbol 'spy) [(api/string "edited")])
     (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                    (api/key-fn :activity)
                                                                    (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :edit)])])
                                     (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                    (api/key-fn :selected)])
                                     (api/symbol 'is-marked-cell)]))

   (defncall 'is-active-branch '->
     (api/key-fn :exp)
     (api/key-fn :counter)
     (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '>) [(api/symbol '_) (api/integer 100)])
                                     (api/fn-call (api/symbol '<) [(api/symbol '_) (api/integer 101)])]))

   ;; graphing of nodes

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

   (defncall 'get-func-stroke '->
     (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol 'or) [(api/symbol 'is-selected) (api/key-fn :expanded)])
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
               (api/keyword :activity) (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                      (api/key-fn :editor)
                                                                      (api/key-fn :activity)])
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

   (defncall 'graph-func-body '->
     (api/vector [(api/keyword :g)
                  (api/vector [(api/keyword :rect)
                               (api/map {(api/keyword :transform) (api/string "translate(200,0)")
                                         (api/keyword :width) (api/integer 100)
                                         (api/keyword :height) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :size)])
                                         (api/keyword :style) (api/map {(api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :cell-type-fill) (api/symbol 'get-color)])
                                                                        (api/keyword :stroke) (api/string "darkgrey")})})])
                  (api/vector [(api/keyword :g)
                               (api/map {(api/keyword :transform)
                                         (api/fn-call (api/symbol '->) [(api/map {(api/keyword :x)
                                                                                  (api/integer 15)
                                                                                  (api/keyword :y)
                                                                                  (api/integer 25)})
                                                                        (api/symbol 'translate-str)])})
                               (api/fn-call (api/symbol '->) [(api/symbol 'merge-body)
                                                              (api/symbol 'graph-body)])])]))

   (defncall 'graph-func-header '->
     (api/vector [(api/keyword :g)
                  (api/vector [(api/keyword :rect)
                               (api/map {(api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :width)])
                                         (api/keyword :height) (api/integer 30)
                                         (api/keyword :style) (api/map {(api/keyword :fill) (api/symbol 'get-func-stroke)
                                                                        (api/keyword :stroke) (api/string "darkgrey")})})])
                  (api/vector [(api/keyword :text)
                               (api/map {(api/keyword :x) (api/integer 150)
                                         (api/keyword :y) (api/integer 20)
                                         (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :node-name-stroke) (api/symbol 'get-color)])
                                         (api/keyword :text-anchor) (api/keyword :middle)
                                         (api/keyword :font-weight) (api/string "bold")})
                               (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :name)])])]))

   (defncall 'graph-func-single '->
     (api/vector [(api/keyword :g)
                  (api/vector [(api/keyword :rect)
                               (api/map {(api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :width)])
                                         (api/keyword :height) (api/fn-call (api/symbol 'if) [(api/key-fn :expanded)
                                                                                              (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :size)])
                                                                                              (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :height)])])
                                         (api/keyword :style) (api/map {(api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :node-gutter) (api/symbol 'get-color)])
                                                                        (api/keyword :stroke) (api/symbol 'get-func-stroke)
                                                                        (api/keyword :filter) (api/string "url(#shadow)")
                                                                        (api/keyword :pointer-events) (api/string "all")})})])
                  (api/fn-call (api/symbol 'if) [(api/key-fn :expanded) (api/symbol 'graph-func-body) (api/string "")])
                  (api/symbol 'graph-func-header)
                  (api/vector [(api/keyword :rect)
                               (api/map {(api/keyword :id) (api/symbol 'func-id)
                                         (api/keyword :fill-opacity) (api/integer 0)

                                         (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "all")})
                                         (api/keyword :width) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :width)])
                                         (api/keyword :height) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :height)])})])]))

   (defncall 'graph-func '->
     (api/vector [(api/keyword :g)
                  (api/vector [(api/keyword :g)
                               (api/map {(api/keyword :transform)
                                         (api/fn-call (api/symbol '->) [(api/key-fn :node)
                                                                        (api/symbol 'translate-ident)])})
                               (api/fn-call (api/symbol '->) [(api/map {(api/keyword :node) (api/key-fn :node)
                                                                        (api/keyword :context) (api/key-fn :context)
                                                                        (api/keyword :expanded) (api/fn-call (api/symbol '>) [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :view) (api/key-fn :zoom)])
                                                                                                                              (api/integer 1)])})
                                                              (api/symbol 'graph-func-single)])])
                  ;; (api/fn-call (api/symbol 'incase) [(api/symbol 'is-hovered) (api/vector [(api/keyword :g)
                  ;;                                                                           (api/map {(api/keyword :filter) (api/string "url(#blur)")
                  ;;                                                                                     (api/keyword :opacity) (api/string "0.6")
                  ;;                                                                      (api/keyword :transform)
                  ;;                                                                      (api/fn-call (api/symbol '->) [(api/key-fn :node)
                  ;;                                                                                                     (api/symbol 'translate-blur)])})
                  ;;                                                            (api/symbol 'graph-func-single)])])
                  (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol 'and) [(api/symbol 'is-hovered)
                                                                                 (api/fn-call (api/symbol '->) [(api/key-fn :context)
                                                                                                                (api/symbol 'is-change-navigate)])])
                                                 (api/vector [(api/keyword :g)
                                                              (api/map {(api/keyword :opacity) (api/string "1")
                                                                        (api/keyword :transform)
                                                                        (api/fn-call (api/symbol '->) [(api/key-fn :node)
                                                                                                       (api/symbol 'translate-func)])})
                                                              (api/fn-call (api/symbol '->) [(api/map {(api/keyword :node) (api/key-fn :node)
                                                                                                       (api/keyword :context) (api/key-fn :context)
                                                                                                       (api/keyword :expanded) (api/keyword :expand)})
                                                                                             (api/symbol 'graph-func-single)])])
                                                 (api/string "")])
                  ]))

   (defncall 'graph-focused '->
     (api/key-fn :context)
     (api/vector [(api/keyword :g)
                  (api/map {(api/keyword :id) (api/string "focused")})
                  (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '->) [(api/key-fn :editor) (api/key-fn :formatted)])
                                                                                 (api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :editor) (api/key-fn :mode)])
                                                                                                               (api/keyword :focus)])])
                                                 (api/vector [(api/keyword :g)
                                                              (api/map {(api/keyword :opacity) (api/string "1")
                                                                        (api/keyword :transform)
                                                                        (api/fn-call (api/symbol '->) [(api/key-fn :editor)
                                                                                                       (api/key-fn :formatted)
                                                                                                       (api/symbol 'translate-dialog)])})
                                                              (api/fn-call (api/symbol '->) [(api/map {(api/keyword :node) (api/fn-call (api/symbol '->) [(api/key-fn :editor) (api/key-fn :formatted)])
                                                                                                       (api/keyword :context) (api/symbol '_)
                                                                                                       (api/keyword :expanded) (api/keyword :expand)})
                                                                                             (api/symbol 'graph-func-single)])])
                                                 (api/string "no-focus")])]))

   (defncall 'pipe-id '->
     (api/fn-call (api/symbol 'str) [(api/string "pipe/")
                                     (api/key-fn :name)]))

   (defncall 'graph-pipe-single '->
     (api/vector [(api/keyword :g)
                  (api/map {(api/keyword :transform)
                            (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/symbol 'translate-str)]) })
                  (api/fn-call (api/symbol 'if) [(api/key-fn :hovered)
                                                 (api/vector [(api/keyword :circle)
                                                              (api/map {(api/keyword :cx) (api/integer 50)
                                                                        (api/keyword :cy) (api/integer 50)
                                                                        (api/keyword :r) (api/integer 50)
                                                                        (api/keyword :style) (api/map {(api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :pipe-glow) (api/symbol 'get-color)])
                                                                                                       (api/keyword :filter) (api/string "url(#blur)")
                                                                                                       (api/keyword :pointer-events) (api/string "all")})})])
                                                 (api/vector [(api/keyword :g)])])
                  (api/vector [(api/keyword :circle)
                               (api/map {(api/keyword :id) (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/symbol 'pipe-id)])
                                         (api/keyword :cx) (api/integer 50)
                                         (api/keyword :cy) (api/integer 50)
                                         (api/keyword :r) (api/integer 50)
                                         (api/keyword :style) (api/map {(api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :pipe-fill) (api/symbol 'get-color)])
                                                                        (api/keyword :stroke) (api/fn-call (api/symbol '->) [(api/keyword :pipe-stroke) (api/symbol 'get-color)])
                                                                        (api/keyword :pointer-events) (api/string "all")})})])
                  (api/fn-call (api/symbol 'if) [(api/key-fn :highlighted)
                                                 (api/vector [(api/keyword :circle)
                                                              (api/map {(api/keyword :cx) (api/integer 53)
                                                                        (api/keyword :cy) (api/integer 47)
                                                                        (api/keyword :r) (api/integer 50)
                                                                        (api/keyword :style) (api/map {(api/keyword :stroke) (api/fn-call (api/symbol '->) [(api/keyword :pipe-drag) (api/symbol 'get-color)])
                                                                                                       (api/keyword :stroke-width) (api/string "3px")
                                                                                                       (api/keyword :filter) (api/string "url(#blurbit)")
                                                                                                       (api/keyword :fill-opacity) (api/float 0.0)
                                                                                                       (api/keyword :pointer-events) (api/string "none")})})])
                                                 (api/vector [(api/keyword :g)])])
                  (api/vector [(api/keyword :text)
                               (api/map {(api/keyword :transform) (api/string "rotate(90 50 50)")
                                         (api/keyword :x) (api/integer 50)
                                         (api/keyword :y) (api/integer 43)
                                         (api/keyword :dy) (api/integer 14)
                                         (api/keyword :fill)
                                         (api/fn-call (api/symbol '->) [(api/keyword :node-name-stroke) (api/symbol 'get-color)])
                                         (api/keyword :text-anchor) (api/keyword :middle)
                                         (api/keyword :font-weight) (api/string "bold")
                                         (api/keyword :pointer-events) (api/string "none")})
                               (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :name)])])]))

   (defncall 'is-dragging '->
     (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :editor) (api/key-fn :mode)])
                                                                   (api/keyword :navigate)])
                                     (api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :editor) (api/key-fn :activity)])
                                                                   (api/keyword :dragging)])]))

   (defncall 'is-pipe-drag-begin '->
     (api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :mouse) (api/key-fn :drag) (api/key-fn :source)])
                                   (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/symbol 'pipe-id)])]))

   (defncall 'is-pipe-target '->
     (api/key-fn :node)
     (api/key-fn :type)
     (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/sink)]))

   (defncall 'is-pipe-hovered '->
     (api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :hovered) (api/key-fn :type)])
                                                                   (api/string "pipe")])
                                     (api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :hovered) (api/key-fn :name)])
                                                                   (api/fn-call (api/symbol '->) [(api/key-fn :node) (api/key-fn :name)])])]))

   (defncall 'graph-pipe '->
     (api/map {(api/keyword :node) (api/key-fn :node)
               (api/keyword :highlighted) (api/fn-call (api/symbol 'and) [(api/symbol 'is-dragging)
                                                                          (api/fn-call (api/symbol 'or) [(api/symbol 'is-pipe-drag-begin)
                                                                                                         (api/symbol 'is-pipe-target)])])
               (api/keyword :hovered) (api/symbol 'is-pipe-hovered)})
     (api/symbol 'graph-pipe-single))

   (defncall 'is-pipe-node '->
     (api/key-fn :node)
     (api/key-fn :type)
     (api/fn-call (api/symbol '=) [(api/symbol '_) (api/keyword :caravan/sink)]))

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

   (defncall 'graph-coords 'str
     (api/key-fn :x)
     (api/string " ")
     (api/key-fn :y))

   (defncall 'bends '->
     (api/fn-call (api/symbol 'str) [(api/string "L ") (api/symbol 'graph-coords)]))

   (defncall 'is-incoming '->
     (api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :hovered) (api/key-fn :name)])
                                   (api/fn-call (api/symbol '->) [(api/key-fn :edge) (api/key-fn :to)])]))

   (defncall 'is-outgoing '->
     (api/fn-call (api/symbol 'and) [(api/key-fn :edge)
                                     (api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :context) (api/key-fn :hovered) (api/key-fn :name)])
                                                                   (api/fn-call (api/symbol '->) [(api/key-fn :edge) (api/key-fn :from)])])]))

   (defncall 'get-edge-color '->
     (api/fn-call (api/symbol 'incase) [(api/symbol 'is-incoming)
                                        (api/fn-call (api/symbol '->) [(api/keyword :edge-in) (api/symbol 'get-color)])])
     (api/fn-call (api/symbol 'incase) [(api/symbol 'is-outgoing)
                                        (api/fn-call (api/symbol '->) [(api/keyword :edge-out) (api/symbol 'get-color)])])
     (api/fn-call (api/symbol 'incase) [(api/key-fn :edge)
                                        (api/fn-call (api/symbol '->) [(api/keyword :edge-neutral) (api/symbol 'get-color)])]))

   (defncall 'graph-connection '->
     (api/vector [(api/keyword :g)
                  (api/vector [(api/keyword :path)
                               (api/map {(api/keyword :style) (api/map {(api/keyword :stroke) (api/symbol 'get-edge-color)
                                                                        (api/keyword :stroke-width) (api/integer 3)
                                                                        (api/keyword :fill) (api/string "transparent")})
                                         (api/keyword :d) (api/fn-call (api/symbol 'str) [(api/string "M ") (api/fn-call (api/symbol '->) [(api/key-fn :edge) (api/key-fn :section) (api/key-fn :startPoint) (api/symbol 'graph-coords)])
                                                                                          (api/fn-call (api/symbol 'str-join) [(api/string " ") (api/fn-call (api/symbol 'map) [(api/symbol 'bends) (api/fn-call (api/symbol '->) [(api/key-fn :edge) (api/key-fn :section) (api/key-fn :bendPoints)])])])
                                                                                          (api/string " L ") (api/fn-call (api/symbol '->) [(api/key-fn :edge) (api/key-fn :section) (api/key-fn :endPoint) (api/symbol 'graph-coords)])])})])]))

   (defncall 'merge-connection '->
     (api/map {(api/keyword :edge) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])
               (api/keyword :context) (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 1)])}))

   (defncall 'build-edge '->
     (api/map {(api/keyword :from) (api/key-fn :from)
               (api/keyword :to) (api/key-fn :to)
               (api/keyword :section) (api/fn-call (api/symbol '->) [(api/key-fn :sections)
                                                                     (api/fn-call (api/symbol 'nth) [(api/symbol '_) (api/integer 0)])])}))

   (defncall 'prepare-edges '->
     (api/fn-call (api/symbol 'map) [(api/symbol 'build-edge)
                                     (api/fn-call (api/symbol '->) [(api/key-fn :layout)
                                                                    (api/key-fn :edges)])]))

   (defncall 'graph-connections '->
     (api/fn-call (api/symbol 'myzip) [(api/symbol 'prepare-edges)
                                       (api/fn-call (api/symbol 'repeat) [(api/fn-call (api/symbol '->) [(api/key-fn :layout)
                                                                                                         (api/key-fn :edges)
                                                                                                         (api/fn-call (api/symbol 'count) [(api/symbol '_)])])
                                                                          (api/key-fn :context)])])
     (api/fn-call (api/symbol 'map) [(api/symbol 'merge-connection) (api/symbol '_)])
     (api/fn-call (api/symbol 'map) [(api/symbol 'graph-connection) (api/symbol '_)])
     (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :g)]) (api/symbol '_)]))

   (defncall 'build-context '->
     (api/map {(api/keyword :editor) (api/key-fn :editor)
               (api/keyword :mouse) (api/key-fn :mouse)
               (api/keyword :view) (api/key-fn :view)
               (api/keyword :selected) (api/fn-call (api/symbol '->) [(api/key-fn :editor)
                                                                      (api/key-fn :selected)])
               (api/keyword :hovered) (api/fn-call (api/symbol '->) [(api/key-fn :editor)
                                                                     (api/key-fn :hover)
                                                                     (api/key-fn :current)])
               (api/keyword :events) (api/key-fn :events)}))

   (defncall 'graph-background '->
     (api/vector [(api/keyword :rect)
                  (api/map {(api/keyword :id) (api/string "back/ground")
                            (api/keyword :width) (api/integer 2000)
                            (api/keyword :height) (api/integer 2000)
                            ;; (api/keyword :x) (api/integer -1000)
                            ;; (api/keyword :y) (api/integer -1000)
                            (api/keyword :x) (api/fn-call (api/symbol '->) [(api/key-fn :view) (api/key-fn :x) (api/fn-call (api/symbol '-) [(api/symbol '_)])])
                            (api/keyword :y) (api/fn-call (api/symbol '->) [(api/key-fn :view) (api/key-fn :y) (api/fn-call (api/symbol '-) [(api/symbol '_)])])
                            (api/keyword :fill) (api/string "url(#grid)")
                            ;; (api/fn-call (api/symbol '->) [(api/keyword :graph-background)
                            ;;   (api/symbol 'get-color)])
                            (api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "all")})})]))

   (defncall 'graph '->
     ;; (api/fn-call (api/symbol 'spy) [(api/string "graph")])
     (api/map {(api/keyword :layout) (api/key-fn :layout)
               (api/keyword :view) (api/key-fn :view)
               (api/keyword :context) (api/symbol 'build-context)})
     (api/map {(api/keyword :graph)
               (api/vector [(api/keyword :g)
                            (api/map {(api/keyword :transform) (api/fn-call (api/symbol '->) [(api/key-fn :view) (api/symbol 'translate-graph)])})
                            (api/symbol 'graph-background)
                            (api/symbol 'graph-connections)
                            (api/symbol 'graph-nodes)])}))

   (defncall 'graph-focus '->
     (api/map {(api/keyword :context) (api/symbol 'build-context)})
     (api/map {(api/keyword :graph-focused)
               (api/vector [(api/keyword :g)
                            (api/symbol 'graph-focused)])}))

   (defncall 'graph-loading '->
     (api/vector [(api/keyword :g)
                  (api/map {(api/keyword :transform) (api/string "translate(200,100)")})
                  (api/vector [(api/keyword :rect)
                               (api/map {(api/keyword :id) (api/string "ui/dialog")
                                         (api/keyword :width) (api/integer 500)
                                         (api/keyword :height) (api/integer 300)
                                         (api/keyword :stroke) (api/symbol 'get-menu-fg)
                                         (api/keyword :style) (api/map {(api/keyword :filter) (api/string "url(#shadow)")
                                                                        (api/keyword :pointer-events) (api/string "all")})
                                         (api/keyword :fill-opacity) (api/float 0.8)
                                         (api/keyword :fill) (api/symbol 'get-menu-fill)})])
                  (api/vector [(api/keyword :text)
                               (api/map {(api/keyword :x) (api/integer 250)
                                         (api/keyword :y) (api/integer 150)
                                         (api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :node-name-stroke) (api/symbol 'get-color)])
                                         (api/keyword :text-anchor) (api/keyword :middle)
                                         (api/keyword :font-weight) (api/string "bold")
                                         (api/keyword :font-size) (api/string "200%")})
                               (api/string "Loading...")])]))

   (defncall 'graph-dialog '->
     (api/map {(api/keyword :graph-dialog)
               (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :editor) (api/key-fn :mode)])
                                                                            (api/keyword :load)])
                                              (api/symbol 'graph-loading)
                                              (api/vector [(api/keyword :g)])])}))

   (defncall 'graph-link '->
     (api/key-fn :mouse)
     (api/vector [(api/keyword :g)
                  (api/vector [(api/keyword :line)
                               (api/map {(api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "none")
                                                                        (api/keyword :stroke) (api/string "orange")
                                                                        (api/keyword :stroke-width) (api/integer 3)})

                                         (api/keyword :stroke-linecap) (api/string "round")
                                         (api/keyword :filter) (api/string "url(#blurbit)")
                                         (api/keyword :x1) (api/fn-call (api/symbol '->)[(api/key-fn :start) (api/key-fn :x)])
                                         (api/keyword :y1) (api/fn-call (api/symbol '->)[(api/key-fn :start) (api/key-fn :y)])
                                         (api/keyword :x2) (api/fn-call (api/symbol '->)[(api/key-fn :position) (api/key-fn :x)])
                                         (api/keyword :y2) (api/fn-call (api/symbol '->)[(api/key-fn :position) (api/key-fn :y)])})])
                  (api/vector [(api/keyword :line)
                               (api/map {(api/keyword :style) (api/map {(api/keyword :pointer-events) (api/string "none")
                                                                        (api/keyword :stroke) (api/string "orange")
                                                                        (api/keyword :stroke-width) (api/integer 1)})

                                         (api/keyword :x1) (api/fn-call (api/symbol '->)[(api/key-fn :start) (api/key-fn :x)])
                                         (api/keyword :y1) (api/fn-call (api/symbol '->)[(api/key-fn :start) (api/key-fn :y)])
                                         (api/keyword :x2) (api/fn-call (api/symbol '->)[(api/key-fn :position) (api/key-fn :x)])
                                         (api/keyword :y2) (api/fn-call (api/symbol '->)[(api/key-fn :position) (api/key-fn :y)])})])
                  ;; (api/vector [(api/keyword :g)
                  ;;              (api/map {(api/keyword :transform)
                  ;;                        (api/fn-call (api/symbol '->) [(api/key-fn :mouse)(api/key-fn :position)(api/symbol 'translate-str)]) })
                  ;;              (api/vector [(api/keyword :circle)
                  ;;                           (api/map {(api/keyword :cx) (api/integer -25)
                  ;;                                     (api/keyword :cy) (api/integer -25)
                  ;;                                     (api/keyword :r) (api/integer 50)
                  ;;                                     (api/keyword :style) (api/map {(api/keyword :fill) (api/fn-call (api/symbol '->) [(api/keyword :pipe-glow) (api/symbol 'get-color)])
                  ;;                                                                    (api/keyword :filter) (api/string "url(#blur)")
                  ;;                                                                    (api/keyword :pointer-events) (api/string "all")})})])])
                  ]))


   (defncall 'graph-drag '->
     (api/map {(api/keyword :graph-drag)
               (api/fn-call (api/symbol 'if) [(api/fn-call (api/symbol 'and) [(api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :editor) (api/key-fn :activity)])
                                                                                                            (api/keyword :dragging)])
                                                                              (api/fn-call (api/symbol '=) [(api/fn-call (api/symbol '->) [(api/key-fn :mouse) (api/key-fn :drag) (api/key-fn :active)])
                                                                                                            (api/keyword :true)])])
                                              (api/symbol 'graph-link)
                                              (api/vector [(api/keyword :g) (api/map {(api/keyword :id) (api/string "top-drag")})])])}))

   ;; reduce elements to latest version of GUI element

   (defncall 'svg-elements-reduce 'pipes/reductions
     (api/fn-call (api/symbol '->)
                  [(api/vector [(api/key-fn :state) (api/key-fn :next)])
                   (api/fn-call (api/symbol 'into) [(api/map {}) (api/symbol '_)])])
     (api/map {(api/keyword :graph) (api/vector [(api/keyword :g)])
               (api/keyword :graph-focused) (api/vector [(api/keyword :g)])
               (api/keyword :sink-menu) (api/vector [(api/keyword :g)])
               (api/keyword :source-menu) (api/vector [(api/keyword :g)])
               (api/keyword :action-menu) (api/vector [(api/keyword :g)])
               (api/keyword :graph-drag) (api/vector [(api/keyword :g)])
               (api/keyword :graph-dialog) (api/vector [(api/keyword :g)])
               (api/keyword :resize) (api/map {})}))

   ;; render elements to hiccup

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
                                                      (api/keyword :flood-color) (api/fn-call (api/symbol 'get-color) [(api/keyword :shadow-flood)])
                                                      (api/keyword :flood-opacity) (api/string "0.3")
                                                      (api/keyword :stdDeviation) (api/string "3")})])])

                  (api/vector [(api/keyword :filter)
                               (api/map {(api/keyword :id) (api/string "blur")})
                               (api/vector [(api/keyword :feGaussianBlur)
                                            (api/map {(api/keyword :in) (api/string "fillPaint")
                                                      (api/keyword :stdDeviation) (api/string "10 10")})])])
                  (api/vector [(api/keyword :filter)
                               (api/map {(api/keyword :id) (api/string "blurbit")})
                               (api/vector [(api/keyword :feGaussianBlur)
                                            (api/map {(api/keyword :in) (api/string "fillPaint")
                                                      (api/keyword :stdDeviation) (api/string "2 2")})])])
                  (api/vector [(api/keyword :filter)
                               (api/map {(api/keyword :id) (api/string "redshadow")})
                               (api/vector [(api/keyword :feDropShadow)
                                            (api/map {(api/keyword :dx) (api/string "3")
                                                      (api/keyword :dy) (api/string "3")
                                                      (api/keyword :flood-color) (api/fn-call (api/symbol '->) [(api/string "red")])
                                                      (api/keyword :flood-opacity) (api/string "0.3")
                                                      (api/keyword :stdDeviation) (api/string "3")})])])
                  (api/vector [(api/keyword :filter)
                               (api/map {(api/keyword :id) (api/string "whiteshadow")})
                               (api/vector [(api/keyword :feDropShadow)
                                            (api/map {(api/keyword :dx) (api/string "3")
                                                      (api/keyword :dy) (api/string "3")
                                                      (api/keyword :flood-color) (api/fn-call (api/symbol '->) [(api/string "white")])
                                                      (api/keyword :flood-opacity) (api/string "0.3")
                                                      (api/keyword :stdDeviation) (api/string "3")})])])
                  (api/vector [(api/keyword :filter)
                               (api/map {(api/keyword :id) (api/string "leftshadow")})
                               (api/vector [(api/keyword :feDropShadow)
                                            (api/map {(api/keyword :dx) (api/string "-3")
                                                      (api/keyword :dy) (api/string "3")
                                                      (api/keyword :flood-color) (api/fn-call (api/symbol 'get-color) [(api/keyword :shadow-flood)])
                                                      (api/keyword :flood-opacity) (api/string "0.3")
                                                      (api/keyword :stdDeviation) (api/string "3")})])])
                  (api/vector [(api/keyword :filter)
                               (api/map {(api/keyword :id) (api/string "upshadow")})
                               (api/vector [(api/keyword :feDropShadow)
                                            (api/map {(api/keyword :dx) (api/string "0")
                                                      (api/keyword :dy) (api/string "-3")
                                                      (api/keyword :flood-color) (api/fn-call (api/symbol 'get-color) [(api/keyword :shadow-flood)])
                                                      (api/keyword :flood-opacity) (api/string "0.3")
                                                      (api/keyword :stdDeviation) (api/string "3")})])])]))

   (defncall 'get-height '->
     (api/key-fn :resize)
     (api/fn-call (api/symbol 'or) [(api/key-fn :height) (api/integer 1000)]))

   (defncall 'get-width '->
     (api/key-fn :resize)
     (api/fn-call (api/symbol 'or) [(api/key-fn :width) (api/integer 2000)]))


   ;; render SVG components
   (defncall 'svg-render 'pipes/debug)
   (defncall 'render-svg '->
     (api/map {(api/keyword :svg)
               (api/map {(api/keyword :oasis.gui/order)
                         (api/integer 2)
                         (api/keyword :oasis.gui/element)
                         (api/vector [(api/keyword :svg)
                                      (api/map {
                                                (api/keyword :viewBox) (api/fn-call (api/symbol 'str) [(api/string "0 0 ") (api/symbol 'get-width) (api/string " ") (api/symbol 'get-height)])
                                                })
                                      (api/symbol 'svg-defs)
                                      (api/key-fn :graph)
                                      (api/key-fn :graph-focused)
                                      (api/key-fn :source-menu)
                                      (api/key-fn :sink-menu)
                                      (api/key-fn :action-menu)
                                      (api/key-fn :graph-drag)
                                      (api/key-fn :graph-dialog)
                                      ])})}))


   (api/defmodule 'oasis-render (api/map {(api/keyword :sources) (api/map {(api/keyword :state) (api/symbol 'oasis-render-in)
                                                                           (api/keyword :svg-elems) (api/symbol 'oasis-render-out)})
                                          (api/keyword :sinks) (api/map {(api/keyword :svg-elems) (api/symbol 'oasis-render-out)})}))
   ])

(def oasis-render-net
  [
   (pipe 'oasis-render-in 'state-reduce)
   (pipe 'state-reduce 'condensed-state)

   (pipe 'condensed-state 'only-resize 'svg-render)
   (pipe 'condensed-state 'graph 'svg-render)
   (pipe 'condensed-state 'graph-drag 'svg-render)
   (pipe 'condensed-state 'graph-focus 'svg-render)
   (pipe 'condensed-state 'graph-dialog 'svg-render)
   (pipe 'condensed-state 'render-sink-menu 'svg-render)
   (pipe 'condensed-state 'render-source-menu 'svg-render)
   (pipe 'condensed-state 'render-action-menu 'svg-render)

   (pipe 'svg-render 'svg-elements-reduce)
   (pipe 'svg-elements-reduce 'svg-reduced)
   (pipe 'svg-reduced 'render-svg 'oasis-render-out)
   ])


(def oasis-ui-defs
  [
   (defncall 'oasis-ui-in 'pipes/debug)
   (defncall 'oasis-ui-out 'pipes/ui (api/integer 2))
   (defncall 'oasis-ui-mouse 'pipes/mouse (api/integer 2))
   (defncall 'oasis-ui-kb 'pipes/keyboard)
   (defncall 'render 'pipes/debug ;; (api/keyword :oasis.spec/render)
     )
   (defncall 'reducer 'pipes/debug ;; (api/keyword :oasis.spec/gui)
     )
   (defncall 'elements-reduce 'pipes/reductions
     (api/fn-call (api/symbol 'into) [(api/key-fn :state) (api/key-fn :next)])
     (api/map {}))
   (defncall 'render-elements '->
     ;; (api/fn-call (api/symbol 'spy) [(api/string "vals1")])
     (api/fn-call (api/symbol 'vals) [(api/symbol '_)])
     ;; (api/fn-call (api/symbol 'sort-by [(api/symbol '_)]))
     (api/fn-call (api/symbol 'map) [(api/key-fn :oasis.gui/element) (api/symbol '_)])
     (api/fn-call (api/symbol 'into) [(api/vector [(api/keyword :div) (api/map {(api/keyword :class) (api/string "fullscreen")})])
                                      (api/symbol '_)]))

   (api/defmodule 'oasis-ui (api/map {(api/keyword :sources) (api/map {(api/keyword :render) (api/symbol 'oasis-ui-in)
                                                                       (api/keyword :kb) (api/symbol 'oasis-ui-kb)
                                                                       (api/keyword :mouse) (api/symbol 'oasis-ui-mouse)})
                                      (api/keyword :sinks) (api/map {(api/keyword :mouse) (api/symbol 'oasis-ui-mouse)
                                                                     (api/keyword :kb) (api/symbol 'oasis-ui-kb)})
                                    (api/keyword :tests) (api/map {(api/keyword :t1) (api/map {(api/keyword :when) (api/map {(api/string "oasis-ui-in") (api/vector [(api/map {(api/keyword :header)
                                                                                                                                                                               (api/map {(api/keyword :oasis.gui/order)
                                                                                                                                                                                         (api/integer 1)
                                                                                                                                                                                         (api/keyword :oasis.gui/element)
                                                                                                                                                                                         (api/vector [(api/keyword :h1)
                                                                                                                                                                                                      (api/string "사막 Oasis")])})})])})
                                                                                               (api/keyword :then) (api/map {(api/string "oasis-ui-out")
                                                                                                                             (api/vector [(api/fn-call (api/symbol 'incase) [(api/fn-call (api/symbol '=) [(api/keyword :div)
                                                                                                                                                                                                           (api/fn-call (api/symbol 'first) [(api/symbol '_)])])
                                                                                                                                                                             (api/keyword :success)])])})})})}))
   ])

(def oasis-ui-net
  [(pipe 'oasis-ui-in 'render)
   (pipe 'render 'elements-reduce)
   (pipe 'elements-reduce 'reducer)

   (pipe 'reducer 'render-elements 'oasis-ui-out)
   ;; (pipe 'reducer 'render-elements 'log-render)
   ])

(def oasis-module-defs
  [
   (defncall 'm-ui '->
     (api/symbol 'oasis-ui))
   (defncall 'm-render-fn '->
     (api/symbol 'm-ui)
     (api/key-fn :sources)
     (api/key-fn :render))
   (defncall 'ui-render 'm-render-fn)
   (defncall 'm-mouse-fn '->
     (api/symbol 'm-ui)
     (api/key-fn :sinks)
     (api/key-fn :mouse))
   (defncall 'oasis-bug 'm-mouse-fn)
   (defncall 'm-kb-fn '->
     (api/symbol 'm-ui)
     (api/key-fn :sinks)
     (api/key-fn :kb))
   (defncall 'oasis-keyboard 'm-kb-fn)

   (defncall 'm-render '->
     (api/symbol 'oasis-render))
   (defncall 'm-state-fn '->
     (api/symbol 'm-render)
     (api/key-fn :sources)
     (api/key-fn :state))
   (defncall 'render-state 'm-state-fn)
   (defncall 'm-elems-fn '->
     (api/symbol 'm-render)
     (api/key-fn :sinks)
     (api/key-fn :svg-elems))
   (defncall 'render-elems 'm-elems-fn)

   (defncall 'm-core '->
     (api/symbol 'oasis-core))
   (defncall 'm-state-out-fn '->
     (api/symbol 'm-core)
     (api/key-fn :sinks)
     (api/key-fn :state))
   (defncall 'core-state 'm-state-out-fn)
   (defncall 'm-init-fn '->
     (api/symbol 'm-core)
     (api/key-fn :sources)
     (api/key-fn :init))
   (defncall 'core-init 'm-init-fn)
   (defncall 'm-core-kb-fn '->
     (api/symbol 'm-core)
     (api/key-fn :sources)
     (api/key-fn :kb))
   (defncall 'core-kb-in 'm-core-kb-fn)
   (defncall 'm-core-mouse-fn '->
     (api/symbol 'm-core)
     (api/key-fn :sources)
     (api/key-fn :mouse))
   (defncall 'core-mouse-in 'm-core-mouse-fn)

   (defncall 'log-state 'pipes/log (api/string "log-state: "))
   (defncall 'log-render 'pipes/log (api/string "render: "))
   (defncall 'log-foo 'pipes/log (api/string "foo: "))

   (defncall 'header '->
     (api/map {(api/keyword :header)
               (api/map {(api/keyword :oasis.gui/order)
                         (api/integer 1)
                         (api/keyword :oasis.gui/element)
                         (api/vector [(api/keyword :h1)
                                      (api/string "사막 Oasis")])})}))


   (defncall 'oasis-init 'pipes/debug)
   (api/defmodule 'oasis (api/map {(api/keyword :depends) (api/map {(api/keyword :oasis-core) (api/symbol 'oasis-core)

                                                                    (api/keyword :oasis-render) (api/symbol 'oasis-render)
                                                                    (api/keyword :oasis-ui) (api/symbol 'oasis-ui)
                                                                    })
                                   (api/keyword :sources) (api/map {(api/keyword :module-core) (api/symbol 'm-core)
                                                                    (api/keyword :module-core-out) (api/symbol 'core-state)
                                                                    (api/keyword :module-render) (api/symbol 'm-render)
                                                                    (api/keyword :module-render-out) (api/symbol 'render-elems)
                                                                    (api/keyword :module-ui) (api/symbol 'm-ui)
                                                                    (api/keyword :kb) (api/symbol 'oasis-keyboard)
                                                                    (api/keyword :mouse) (api/symbol 'oasis-bug)
                                                                    (api/keyword :main) (api/symbol 'oasis-init)
                                                                    })}))
   (api/defexp 'oasis-legacy (api/map {(api/keyword :sources) (api/map {(api/keyword :main) (api/symbol 'oasis-init)

                                                                        (api/keyword :mouse) (api/symbol 'oasis-mouse)
                                                                        (api/keyword :kb) (api/symbol 'oasis-kb)
                                                                        (api/keyword :eval) (api/symbol 'oasis-eval)
                                                                        (api/keyword :layout) (api/symbol 'oasis-layout)
                                                                        })
                                       ;; (api/keyword :sink) (api/vector [(api/symbol 'oasisp)])
                                       (api/keyword :tests) (api/map {(api/keyword ::test)
                                                                      (api/map {(api/keyword :when) (api/map {(api/string "init")
                                                                                                              (api/vector [(api/integer 1)])})
                                                                                (api/keyword :then) (api/map {(api/string "oasis-ui-out")
                                                                                                              (api/vector [(api/keyword :success)])})})})}))
   ])

(def oasis-module-net
  [
   (pipe 'oasis-init 'header 'ui-render)
   (pipe 'oasis-init 'core-init)
   ;; (pipe 'core-state 'log-state)
   (pipe 'core-state 'render-state)
   ;; (pipe 'render-elems 'log-render)
   (pipe 'render-elems 'ui-render)

   (pipe 'oasis-keyboard 'core-kb-in)
   (pipe 'oasis-bug 'core-mouse-in)
   ])


(defn start-ui []
  (into oasis-ui-defs (flatten oasis-ui-net)))

(def network (concat oasis-module-net oasis-ui-net oasis-core-net oasis-render-net))
(def oasis (into oasis1 (into oasis2 (into oasis-core-defs (into oasis-render-defs (into oasis-ui-defs oasis-module-defs))))))

(defn start []
  (into oasis (flatten network)))

(defn store [s]
  (stores/persist-tree! s oasis)
  (stores/persist-tree! s (flatten network))
  s)
