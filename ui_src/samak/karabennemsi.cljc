(ns ^:figwheel-no-load samak.karabennemsi
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

(def kbn-core-defs
  [
   (api/defexp 'carv-mod (api/fn-call (api/symbol 'modules/caravan) []))
   (api/defexp 'm-caravan (api/fn-call (api/symbol 'carv-mod) []))
   (defncall 'm-caravan-eval '->
     (api/symbol 'm-caravan)
     (api/key-fn :sources)
     (api/key-fn :eval))
   (defncall 'caravan-eval 'm-caravan-eval)
   (defncall 'm-caravan-trace '->
     (api/symbol 'm-caravan)
     (api/key-fn :sources)
     (api/key-fn :trace))
   (defncall 'caravan-trace 'm-caravan-trace)
   (defncall 'm-caravan-actions '->
     (api/symbol 'm-caravan)
     (api/key-fn :sinks)
     (api/key-fn :actions))
   (defncall 'caravan-actions 'm-caravan-actions)

   (defncall 'kbn-core-init 'pipes/debug (api/string "kbn-core-init"))

   (defncall 'kbn-core-out 'pipes/debug (api/string "kbn-core-out"))
   (defncall 'kbn-core-foo 'pipes/debug (api/string "kbn-core-foo"))
   (defncall 'kbn-core-traces 'pipes/debug (api/string "kbn-core-traces"))

   (defncall 'kbn-core-in 'pipes/debug (api/string "kbn-core-in"))

   (defncall 'log-core 'pipes/log (api/string "core: "))

   ;; (defncall 'kbn-core-load '->
   ;;   (api/map {(api/keyword :call) (api/map {(api/keyword :action) (api/keyword :load)
   ;;                                           (api/keyword :arguments) (api/keyword :base)})}))

   (defncall 'kbn-core-trace-counter 'pipes/reductions
     (api/fn-call (api/symbol '->) [(api/map {(api/keyword :test)
                                              (api/fn-call (api/symbol 'update-in) [(api/key-fn :state)
                                                                                    (api/fn-call (api/symbol '->) [(api/key-fn :next)
                                                                                                                   (api/key-fn :samak.pipes/content)
                                                                                                                   (api/key-fn :samak.trace/node)])
                                                                                    (api/symbol 'inc)])})])
     (api/map {}))

   (api/defmodule 'kbn-core (api/map {(api/keyword :depends) (api/map {(api/keyword :caravan) (api/symbol 'modules/caravan)})
                                      (api/keyword :sources) (api/map {(api/keyword :init) (api/symbol 'kbn-core-init)
                                                                       (api/keyword :caravan-eval) (api/symbol 'caravan-eval)
                                                                       (api/keyword :caravan-trace) (api/symbol 'caravan-trace)
                                                                       (api/keyword :caravan-actions) (api/symbol 'caravan-actions)
                                                                       (api/keyword :in) (api/symbol 'kbn-core-in)})
                                      (api/keyword :sinks) (api/map {(api/keyword :out) (api/symbol 'kbn-core-out)})}))])


(def core-net
  [
   (pipe 'kbn-core-init 'log-core)
   ;; (pipe 'kbn-core-init 'kbn-core-load 'caravan-actions)
   (pipe 'caravan-eval 'log-core)
   (pipe 'caravan-trace 'kbn-core-traces)
   (pipe 'kbn-core-traces 'kbn-core-trace-counter)
   (pipe 'kbn-core-trace-counter 'kbn-core-foo)
   (pipe 'kbn-core-foo 'kbn-core-out)
   (pipe 'kbn-core-foo 'log-core)
   (pipe 'kbn-core-init 'log-core)
   (pipe 'kbn-core-traces 'log-core)
   ])

(def kbn-head-defs
  [
   (defncall 'kbn-head-init 'pipes/debug (api/string "kbn-head-init"))
   (defncall 'log-head 'pipes/log (api/string "head: "))
   (defncall 'kbn-head-out 'pipes/debug (api/string "kbn-head-out"))
   (defncall 'kbn-head-in 'pipes/debug (api/string "kbn-head-in"))

   (defncall 'kbn-head-paint 'pipes/ui (api/integer 2))

   (api/defmodule 'kbn-head (api/map {(api/keyword :depends) (api/map {(api/keyword :ui) (api/symbol 'modules/ui)})
                                      (api/keyword :sources) (api/map {(api/keyword :init) (api/symbol 'kbn-head-init)
                                                                         (api/keyword :in) (api/symbol 'kbn-head-in)})
                                        (api/keyword :sinks) (api/map {(api/keyword :out) (api/symbol 'kbn-head-out)})}))])

(def head-net
  [
   (pipe 'kbn-head-init 'log-head)
   (pipe 'kbn-head-in 'kbn-head-paint)
   ])

(def kbn-render-defs
  [
   (defncall 'kbn-render-init 'pipes/debug (api/string "kbn-render-init"))
   (defncall 'log-render 'pipes/log (api/string "render init: "))
   (defncall 'kbn-render-out 'pipes/debug (api/string "kbn-render-out"))
   (defncall 'kbn-render-in 'pipes/debug (api/string "kbn-render-in"))

   (defncall 'kbn-reduce 'pipes/reductions
     (api/fn-call (api/symbol 'into) [(api/key-fn :state) (api/vector [(api/fn-call (api/symbol 'str) [(api/fn-call (api/symbol 'count) [(api/key-fn :state)]) (api/string " - ") (api/key-fn :next)])])])
     (api/vector []))
   (defncall 'kbn-render-svg '->
     (api/vector [(api/keyword :div) (api/fn-call (api/symbol 'str) [(api/symbol '_)])]))

   (api/defmodule 'kbn-render (api/map {(api/keyword :sources) (api/map {(api/keyword :init) (api/symbol 'kbn-render-init)
                                                                         (api/keyword :in) (api/symbol 'kbn-render-in)})
                                        (api/keyword :sinks) (api/map {(api/keyword :out) (api/symbol 'kbn-render-out)})}))])

(def render-net
  [
   (pipe 'kbn-render-init 'log-render)
   (pipe 'kbn-render-in 'kbn-reduce)
   (pipe 'kbn-reduce 'kbn-render-svg 'kbn-render-out)
   ])

(def kbn-ui-defs
  [
   (defncall 'kbn-ui-init 'pipes/debug (api/string "kbn-ui-init"))
   (defncall 'log-ui 'pipes/log (api/string "ui init: "))
   (defncall 'log-kb 'pipes/log (api/string "kb: "))
   (defncall 'kbn-ui-kb 'pipes/keyboard)
   (defncall 'kbn-ui-kb-out 'pipes/debug (api/string "kbn-ui-kb-out"))
   (defncall 'kbn-ui-out 'pipes/debug (api/string "kbn-ui-out"))
   (defncall 'kbn-ui-in 'pipes/debug (api/string "kbn-ui-in"))

   (api/defmodule 'kbn-ui (api/map {(api/keyword :sources) (api/map {(api/keyword :init) (api/symbol 'kbn-ui-init)
                                                                     (api/keyword :kb) (api/symbol 'kbn-ui-kb)
                                                                     (api/keyword :ui-in) (api/symbol 'kbn-ui-in)})
                                    (api/keyword :sinks) (api/map {(api/keyword :kb-out) (api/symbol 'kbn-ui-kb-out)
                                                                   (api/keyword :ui-out) (api/symbol 'kbn-ui-out)})}))])

(def ui-net
  [
   (pipe 'kbn-ui-init 'log-ui)
   (pipe 'kbn-ui-kb 'log-kb)
   (pipe 'kbn-ui-kb 'kbn-ui-kb-out)
;   (pipe 'kbn-ui-init 'kbn-ui-out)
   (pipe 'kbn-ui-in 'kbn-ui-out)
   ])

(def kbn-module-defs
  [
   (defncall 'm-kbn-render-init-fn '->
     (api/symbol 'kbn-render)
     (api/key-fn :sources)
     (api/key-fn :init))
   (defncall 'm-kbn-render-init 'm-kbn-render-init-fn)
   (defncall 'm-kbn-render-in-fn '->
     (api/symbol 'kbn-render)
     (api/key-fn :sources)
     (api/key-fn :in))
   (defncall 'm-kbn-render-in 'm-kbn-render-in-fn)
   (defncall 'm-kbn-render-out-fn '->
     (api/symbol 'kbn-render)
     (api/key-fn :sinks)
     (api/key-fn :out))
   (defncall 'm-kbn-render-out 'm-kbn-render-out-fn)

   (defncall 'm-kbn-ui-init-fn '->
     (api/symbol 'kbn-ui)
     (api/key-fn :sources)
     (api/key-fn :init))
   (defncall 'm-kbn-ui-init 'm-kbn-ui-init-fn)
   (defncall 'm-kbn-ui-in-fn '->
     (api/symbol 'kbn-ui)
     (api/key-fn :sources)
     (api/key-fn :ui-in))
   (defncall 'm-kbn-ui-in 'm-kbn-ui-in-fn)
   (defncall 'm-kbn-ui-out-fn '->
     (api/symbol 'kbn-ui)
     (api/key-fn :sinks)
     (api/key-fn :ui-out))
   (defncall 'm-kbn-ui-out 'm-kbn-ui-out-fn)
   (defncall 'm-kbn-kb-out-fn '->
     (api/symbol 'kbn-ui)
     (api/key-fn :sinks)
     (api/key-fn :kb-out))
   (defncall 'm-kbn-kb-out 'm-kbn-kb-out-fn)

   (defncall 'm-kbn-head-init-fn '->
     (api/symbol 'kbn-head)
     (api/key-fn :sources)
     (api/key-fn :init))
   (defncall 'm-kbn-head-init 'm-kbn-head-init-fn)
   (defncall 'm-kbn-head-in-fn '->
     (api/symbol 'kbn-head)
     (api/key-fn :sources)
     (api/key-fn :in))
   (defncall 'm-kbn-head-in 'm-kbn-head-in-fn)

   (defncall 'm-kbn-core-init-fn '->
     (api/symbol 'kbn-core)
     (api/key-fn :sources)
     (api/key-fn :init))
   (defncall 'm-kbn-core-init 'm-kbn-core-init-fn)
   (defncall 'm-kbn-core-actions-fn '->
     (api/symbol 'kbn-core)
     (api/key-fn :sources)
     (api/key-fn :in))
   (defncall 'm-kbn-core-actions 'm-kbn-core-actions-fn)
   (defncall 'm-kbn-core-out-fn '->
     (api/symbol 'kbn-core)
     (api/key-fn :sinks)
     (api/key-fn :out))
   (defncall 'm-kbn-core-out 'm-kbn-core-out-fn)

   (defncall 'log-foo 'pipes/log (api/string "foo: "))
   (defncall 'log-end 'pipes/log (api/string "end: "))
   (defncall 'kbn-init 'pipes/debug (api/string "kbn-init"))
   (api/defmodule 'kbn (api/map {(api/keyword :depends) (api/map {(api/keyword :kbn-render) (api/symbol 'kbn-render)
                                                                  (api/keyword :kbn-ui) (api/symbol 'kbn-ui)
                                                                  (api/keyword :kbn-head) (api/symbol 'kbn-head)
                                                                  (api/keyword :kbn-core) (api/symbol 'kbn-core)})
                                 (api/keyword :sources) (api/map {(api/keyword :main) (api/symbol 'kbn-init)
                                                                  (api/keyword :render-init) (api/symbol 'm-kbn-render-init)
                                                                  (api/keyword :ui-init) (api/symbol 'm-kbn-ui-init)
                                                                  (api/keyword :ui-out) (api/symbol 'm-kbn-ui-out)
                                                                  (api/keyword :kb-out) (api/symbol 'm-kbn-kb-out)
                                                                  (api/keyword :core-out) (api/symbol 'm-kbn-core-out)
                                                                  (api/keyword :render-out) (api/symbol 'm-kbn-render-out)
                                                                  })}))
   ])

(def module-net
  [
   (pipe 'kbn-init 'm-kbn-core-init)
   (pipe 'kbn-init 'm-kbn-render-init)
   (pipe 'kbn-init 'm-kbn-ui-init)
   (pipe 'm-kbn-core-out 'm-kbn-ui-in)
   (pipe 'm-kbn-ui-out 'm-kbn-render-in)
   ;; (pipe 'm-kbn-kb-out 'log-foo)
   ;; (pipe 'm-kbn-render-out 'log-end)
   (pipe 'm-kbn-render-out 'm-kbn-head-in)
   ])


(def code (into kbn-head-defs (into kbn-ui-defs (into kbn-render-defs (into kbn-core-defs kbn-module-defs)))))
(def network (concat head-net ui-net render-net module-net core-net))

(defn store [s]
  (stores/persist-tree! s code)
  (stores/persist-tree! s (flatten network))
  s)
