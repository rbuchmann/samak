(ns samak.worker
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.edn :as edn]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [samak.api :as api]
     [samak.runtime :as run]
     [samak.stdlib :as std]
     [samak.builtins :as builtins]
     [samak.caravan :as caravan]
     [samak.test-programs :as test-programs]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
     [samak.lisparser :as p]
     [samak.pipes :as pipes]
     [samak.runtime.servers :as servers]
     [samak.runtime.stores :as stores])]
   :cljs
   [(:require
     [clojure.string :as str]
     [cljs.reader :as edn]
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
     [samak.api :as api]
     [samak.runtime :as run]
     [samak.stdlib :as std]
     [samak.builtins :as builtins]
     [samak.layout :as layout]
     [samak.caravan :as caravan]
     [samak.test-programs :as test-programs]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
     [samak.lisparser :as p]
     [samak.pipes :as pipes]
     [samak.runtime.servers :as servers]
     [samak.runtime.stores :as stores])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(defn make-piped
  ""
  [name]
  {:target :pipe :named (keyword name)})

(def ui-mock-symbols
  {'pipes/ui       (make-piped 'pipes/ui)
   'pipes/mouse    (make-piped 'pipes/mouse)
   'pipes/keyboard (make-piped 'pipes/keyboard)})

(def worker-symbols
  (merge builtins/samak-symbols
         caravan/symbols
         #?(:cljs layout/layout-symbols)
         ui-mock-symbols
         std/pipe-symbols))


(defn handle-update
  ""
  [msg pipe]
  (let [c (chan)]
    (a/tap (pipes/out-port pipe) c)
    (go-loop []
      (let [p (<! c)]
        (println "got" msg p))
      (recur))))

(def sched
  (fn [broadcast]
    (println "sched")
    (let [to-rt (pipes/pipe (chan))]
      ;; (handle-update "out" broadcast)
      ;; (handle-update "in" to-rt)
      to-rt)))

(def rt (atom {}))

(defn fire-event-into-named-pipe
  [pipe-name event]
  (let [pipe (run/get-definition-by-name @rt (symbol pipe-name))]
    (if (pipes/pipe? pipe)
      (do (let [arg (edn/read-string event)]
            (pipes/fire! pipe arg pipe-name))
          {})
      (println (str "could not find pipe " pipe-name)))))

(defn eval-test
  ""
  []
  (let [code (str/join " " test-programs/tw)
        parsed (p/parse-all code)]
    (swap! rt #(reduce run/eval-expression! % (:value parsed)))
    (fire-event-into-named-pipe "in" "5")))

(defn eval-oasis
  ""
  [length cb state [nr exp]]
  (let [progress (int (* (/ nr length) 100))]
    (when (= 0 (mod progress 10))
      (put! cb progress)
      (println (str progress "%")))
    (run/eval-expression! state exp)))

(defn run-oasis
  ""
  [state cb]
  (put! cb 100)
  (reset! rt state)
  (caravan/init @rt)
  (fire-event-into-named-pipe "init" "1")
  (println "oasis started")
  (let [parsed [(api/defexp 'start (api/fn-call (api/symbol 'pipes/debug) []))]]
    (doseq [expression parsed]
      (caravan/repl-eval expression)))
  (servers/get-defined (:server @rt)))


(defn start-oasis
  [cb]
  (let [c (chan)
        exps (oasis/start)
        numbered (map-indexed vector exps)
        cnt (count numbered)]
    (go-loop [state @rt]
      (let [part (<! c)]
        (if part
          (let [state (eval-oasis cnt cb state part)]
            (recur state))
          (run-oasis state cb))))
    (doall (map #(put! c %) numbered))
    (close! c)))

(def config {:tracer {:backend :none
                      :url "/api/v2/"}})
(def tracer (atom {}))

(defn trace
  [src duration msg]
  (trace/trace src duration msg))


(defn start-rt
  ""
  [load in out]
  (reset! rt (run/make-runtime worker-symbols sched))
  (reset! tracer (trace/init-tracer @rt (:tracer config)))
  (println "worker started runtime" (:id @rt))
  (pipes/link! (:broadcast @rt) (pipes/sink out))
  (pipes/link! (pipes/source in) (:scheduler @rt))
  (start-oasis load))
