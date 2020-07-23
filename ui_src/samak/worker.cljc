(ns samak.worker
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.edn :as edn]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
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

(def sched
  (fn [broadcast]
    (println "sched")
    (let [to-rt (pipes/pipe (chan))]
      ;; (handle-update "out" broadcast)
      ;; (handle-update "in" to-rt)
      to-rt)))

(def rt (atom {}))

(def config {:tracer {:backend :none
                      :url "/api/v2/"}})
(def tracer (atom {}))

(defn trace
  [src duration msg]
  (trace/trace src duration msg))


(defn get-named-pipe
  [rt pipe-name]
  (let [mod-name (sched/module-id (:module pipe-name))
        mod (run/resolve-fn rt mod-name)
        pipe (get-in mod [(:type pipe-name) (:name pipe-name)])]
    (if (pipes/pipe? pipe)
      pipe
      (println "could not find pipe" mod-name "/" pipe-name "->" mod "/" pipe))))

(def get-named-pipe-memo (memoize get-named-pipe))

(defn handle-input
  ""
  [rt c]
  (go-loop []
    (let [p (<! c)
          before (helpers/now)
          content (:samak.runtime/content p)]
      ;; (println (:id rt) "in paket" p)
      (when (= :samak.runtime/paket (:samak.runtime/type p))
        (when-let [pipe (get-named-pipe-memo rt (:samak.runtime/target p))]
          (pipes/fire-raw! pipe content))
        (trace/trace ::render-in
                     (helpers/duration before (helpers/now))
                     content)))
    (recur)))


(defn start-rt
  ""
  [load in out]
  (reset! rt (run/make-runtime worker-symbols sched {}))
  (reset! tracer (trace/init-tracer @rt (:tracer config)))
  (oasis/store (:store @rt))
  (println "worker started runtime" (:id @rt))
  (pipes/link! (:broadcast @rt) (pipes/sink out))
  ;; (pipes/link! (pipes/source in) (:scheduler @rt))
  (sched/start-module rt {} 'oasis-core)
  (caravan/init @rt)
  (println "worker started core")
  (put! load 100)
  (handle-input @rt in))
