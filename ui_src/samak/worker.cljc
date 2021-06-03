(ns samak.worker
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.edn :as edn]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [promesa.core :as p]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.runtime :as run]
     [samak.stdlib :as std]
     [samak.builtins :as builtins]
     [samak.caravan :as caravan]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
     [samak.pipes :as pipes]
     [samak.scheduler :as sched])]
   :cljs
   [(:require
     [clojure.string :as str]
     [cljs.reader :as edn]
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
     [promesa.core :as p]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.runtime :as run]
     [samak.stdlib :as std]
     [samak.builtins :as builtins]
     [samak.layout :as layout]
     [samak.caravan :as caravan]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
     [samak.pipes :as pipes]
     [samak.scheduler :as sched])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(def ui-mock-symbols
  {'modules/ui :blank
   'pipes/ui :blank
   'pipes/events :blank
   'pipes/mouse :blank
   'pipes/keyboard :blank})

(def worker-symbols
  (merge builtins/samak-symbols
         std/pipe-symbols
         caravan/symbols
         ui-mock-symbols
         #?(:cljs layout/layout-symbols)))


(defn handle-update
  ""
  [msg pipe]
  (let [c (chan)]
    (a/tap (pipes/out-port pipe) c)
    (go-loop []
      (let [p (<! c)]
        (println "got" msg p))
      (recur))))

(def scheduler
  (let [broadcast (pipes/pipe (chan) ::worker-broadcast)
        to-rt (pipes/pipe (chan) ::worker-scheduler)]
    (println "sched")
    ;; (handle-update "out" broadcast)
    ;; (handle-update "in" to-rt)
    (fn [] [to-rt broadcast])))

(def rt (atom {}))

(def config {:tracer {:backend :none
                      :url "/api/v2/"}})
(def tracer (atom {}))

(def progress (atom {}))
(def out (atom {}))

(defn trace
  [src duration msg]
  (trace/trace src duration msg))

(def named-modules (atom {}))

(defn get-named-pipe
  [rt pipe-name]
  (let [mod-name (sched/module-id (:module pipe-name))
        mod-alias (get @named-modules (:module pipe-name))
        mod (run/resolve-fn @rt (or mod-alias mod-name))
        pipe (get-in mod [(:type pipe-name) (:name pipe-name)])]
    (if (pipes/pipe? pipe)
      pipe
      (println "could not find pipe" mod-name "/" pipe-name "->" mod "/" pipe))))

(def get-named-pipe-memo (memoize get-named-pipe))

(defn start-oasis
  ""
  []
  (println "worker oasis")
  ;; (pipes/link! (pipes/source in) (:scheduler @rt))
  (p/let [id (sched/start-module rt {} 'oasis-core 'lone)]
    (swap! named-modules assoc :lone id)
    (caravan/init @rt)
    (println "worker started core")))

(defn handle-input
  ""
  [rt c]
  (go-loop []
    (let [p (<! c)
          before (helpers/now)
          content (:samak.runtime/content p)]
      (println (:id @rt) "in paket" p)
      (when (= :samak.runtime/paket (:samak.runtime/type p))
        (when-let [pipe (get-named-pipe-memo rt (:samak.runtime/target p))]
          (pipes/fire-raw! pipe content))
        (trace/trace ::worker-in
                     (helpers/duration before (helpers/now))
                     content)))
    (recur)))

(defn start-rt
  ""
  [load-c in-c out-c]
  (let [[to-rt to-out] (scheduler)
        in-mult (a/mult in-c)
        rt-c (chan)
        paket-c (chan)]
    (a/tap in-mult rt-c)
    (pipes/link! to-out (pipes/sink out-c))
    (pipes/link! (pipes/source rt-c) to-rt)

    (p/let [rt-inst (run/make-runtime worker-symbols scheduler {:store :remote :id "rt-worker"})]
      (reset! rt rt-inst)
      (println "worker rt done")
      (reset! tracer (trace/init-tracer @rt (:tracer config)))
      (println "worker started runtime" (:id @rt))
      (reset! progress load-c)
      (a/tap in-mult paket-c)
      (handle-input rt paket-c)
      (start-oasis)
      (put! @progress 100))))
