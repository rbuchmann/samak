(ns dev.render
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.edn :as edn]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [samak.helpers :as helpers]
     [samak.lisparser :as p]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.trace :as trace]
     [samak.caravan :as caravan]
     [samak.pipes :as pipes]
     [samak.runtime :as run]
     [samak.runtime.stores :as stores])]
   :cljs
   [(:require
     [clojure.string :as str]
     [cljs.reader :as edn]
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
     [samak.helpers :as helpers]
     [samak.lisparser :as p]
     [samak.ui_stdlib :as uistd]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.trace :as trace]
     [samak.caravan :as caravan]
     [samak.layout :as layout]
     [samak.pipes :as pipes]
     [samak.runtime :as run]
     [samak.runtime.stores :as stores])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))


(defn make-piped
  ""
  [name]
  {:target :pipe :named (keyword name)})

(def std-mock-symbols
  {'pipes/to-mouse (make-piped 'pipes/to-mouse)
   'pipes/to-keyboard (make-piped 'pipes/to-keyboard)
   'pipes/to-ui (make-piped 'pipes/to-ui)
   'pipes/to-log (make-piped 'pipes/to-log)})

(def renderer-symbols
  (merge builtins/samak-symbols
         std-mock-symbols
         #?(:cljs layout/layout-symbols)
         #?(:cljs uistd/ui-symbols)))

(def rt (atom {}))
(def config {:tracer {:backend :none
                      :url "/api/v2/"}})
(def tracer (atom {}))

(defn handle-update
  ""
  [msg pipe]
  (let [c (chan)]
    (a/tap (pipes/out-port pipe) c)
    (go-loop []
      (let [p (<! c)]
        (println "render got" msg p))
      (recur))))

(def sched
  (fn [broadcast]
    (println "sched")
    (let [to-rt (pipes/pipe (chan))]
      ;; (handle-update "out" broadcast)
      ;; (handle-update "in" to-rt)
      to-rt)))


(defn get-named-pipe
  [pipe-name event]
  (let [pipe-symbol (symbol (name pipe-name))
        pipe (run/get-definition-by-name @rt pipe-symbol)]
    (if (pipes/pipe? pipe)
      pipe
      (println (str "could not find pipe " pipe-symbol)))))

(def get-named-pipe-memo (memoize get-named-pipe))

(defn handle-render
  ""
  [c]
  (go-loop []
    (let [p (<! c)
          before (helpers/now)
          content (:samak.runtime/content p)]
      ;; (println "render in paket" p)
      (when (= :samak.runtime/paket (:samak.runtime/type p))
        (when-let [pipe (get-named-pipe-memo (:samak.runtime/target p))]
          (pipes/fire-raw! pipe content))
        (trace/trace ::render-in
                     (helpers/duration before (helpers/now))
                     content)))
    (recur)))

(def renderer
  ["(def ui (pipes/ui \"2\"))"
   "(def events-out (pipes/to-ui))"
   "(def mouse (pipes/mouse))"
   "(def mouse-out (pipes/to-mouse))"
   "(def kb (pipes/keyboard))"
   "(def kb-out (pipes/to-keyboard))"
   "(| ui events-out)"
   "(| mouse mouse-out)"
   "(| kb kb-out)"])

(defn eval-render
  ""
  []
  (let [code (str/join " " renderer)
        parsed (p/parse-all code)]
    (swap! rt #(reduce run/eval-expression! % (:value parsed)))))

(defn trace
  [src duration msg]
  (trace/trace src duration msg))

(defn start-render-runtime
  ""
  [in out]
  (reset! rt (run/make-runtime renderer-symbols sched))
  (reset! tracer (trace/init-tracer @rt (:tracer config)))
  (println "renderer started runtime" (:id @rt))
  (eval-render)
  (pipes/link! (:broadcast @rt) (pipes/sink out))
  ;; (pipes/link! (pipes/source in) (:scheduler @rt))
  (handle-render in)
  )
