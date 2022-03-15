(ns dev.render
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.edn :as edn]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [promesa.core :as prom]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
     [samak.karabennemsi :as kbn]
     [samak.caravan :as caravan]
     [samak.lisparser :as p]
     [samak.test-programs :as test-programs]
     [samak.pipes :as pipes]
     [samak.scheduler :as sched]
     [samak.runtime :as run])]
   :cljs
   [(:require
     [clojure.string :as str]
     [cljs.reader :as edn]
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
     [promesa.core :as prom]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.lisparser :as p]
     [samak.ui_stdlib :as uistd]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
     [samak.karabennemsi :as kbn]
     [samak.caravan :as caravan]
     [samak.lisparser :as p]
     [samak.test-programs :as test-programs]
     [samak.layout :as layout]
     [samak.pipes :as pipes]
     [samak.scheduler :as sched]
     [samak.runtime :as run])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))


(def renderer-symbols
  (merge builtins/samak-symbols
         std/pipe-symbols
         caravan/symbols
         #?(:cljs layout/layout-symbols)
         #?(:cljs uistd/ui-symbols)))

(def rt (atom {}))
(def config {:tracer {:backend :none
                      :url "/api/v2/"}})
(def tracer (atom {}))

(def main-conf {:id "rt-main"
                :modules {"oasis-core" {:depends {}
                                        :sinks {:state (sched/make-pipe-id {:module :lone :type :sinks :name :state})}
                                        :sources {
                                                  :init (sched/make-pipe-id {:module :lone :type :sources :name :init})
                                                  :kb (sched/make-pipe-id {:module :lone :type :sources :name :kb})
                                                  :drag (sched/make-pipe-id {:module :lone :type :sources :name :drag})
                                                  :hover (sched/make-pipe-id {:module :lone :type :sources :name :hover})
                                                  :events (sched/make-pipe-id {:module :lone :type :sources :name :events})
                                                  }
                                        }}})


(defn handle-update
  ""
  [msg pipe]
  (let [c (chan)]
    (a/tap (pipes/out-port pipe) c)
    (go-loop []
      (let [p (<! c)]
        (println msg p))
      (recur))))

(def scheduler
  (let [broadcast (pipes/pipe (chan) ::main-broadcast)
        to-rt (pipes/pipe (chan) ::main-scheduler)]
    (println "sched")
    ;; (handle-update "out" broadcast)
    ;; (handle-update "in" to-rt)
    (fn [] [to-rt broadcast])))

(def scheduler2
  (let [broadcast (pipes/pipe (chan) ::preview-broadcast)
        to-rt (pipes/pipe (chan) ::preview-scheduler)]
    (println "sched2")
    (handle-update "out2" broadcast)
    (handle-update "in2" to-rt)
    (fn [] [to-rt broadcast])))

;; (defn eval-test
;;   ""
;;   []
;;   (let [code (str/join " " test-programs/tw)
;;         parsed (p/parse-all code)]
;;     (swap! rt #(reduce run/eval-expression! % (:value parsed)))
;;     (run/fire-into-named-pipe @rt 'in "5" 0)))

(defn run-oasis
  ""
  [id]
  (prom/let [res (run/fire-into-named-pipe @rt id 'kbn-init "1" 0)]
    (println "oasis started: " res))
  ;; (let [parsed [(api/defexp 'start (api/fn-call (api/symbol 'pipes/debug) []))]]
  ;;   (doseq [expression parsed]
  ;;     (caravan/repl-eval expression)))
  )

(defn get-named-pipe
  [rt pipe-name]
  (let [mod-name (sched/module-id (:module pipe-name))
        mod (run/resolve-fn rt mod-name)
        pipe (get-in mod [(:type pipe-name) (:name pipe-name)])]
    (if (pipes/pipe? pipe)
      pipe
      (println "could not find pipe" mod-name "/" pipe-name "->" mod "/" pipe))))

(def get-named-pipe-memo (memoize get-named-pipe))

(defn handle-render
  ""
  [rt c]
  (go-loop []
    (let [p (<! c)
          before (helpers/now)
          content (:samak.runtime/content p)]
      (println (:id rt) "in paket" p)
      (when (= :samak.runtime/paket (:samak.runtime/type p))
        (when-let [pipe (get-named-pipe-memo rt (:samak.runtime/target p))]
          (pipes/fire-raw! pipe content))
        (trace/trace ::render-in
                     (helpers/duration before (helpers/now))
                     content)))
    (recur)))

(defn trace
  [src duration msg]
  (trace/trace src duration msg))

(defn start-oasis
  ""
  [load]
  (println "loading oasis")
  (prom/let [net (sched/load-bundle @rt 'kbn)]
    (helpers/debounce
      (fn []
        (let [id (str "moasis-" (helpers/uuid))]
          (prom/do!
           (println "evaluating oasis")
           (sched/eval-module rt main-conf net nil id)
           (println "renderer loaded oasis")
           (helpers/debounce #(run-oasis id))))))))

(defn start-main
  ""
  [load]
  (println "start-main")
  (helpers/debounce #(start-oasis load)))

(defn start-preview-runtime
  ""
  [in-c out-c]
  (println "renderer starting preview")
  (let [[to-rt to-out] (scheduler2)
        in-mult (a/mult in-c)
        rt-c (chan)
        paket-c (chan)]
    (a/tap in-mult rt-c)
    (pipes/link! to-out (pipes/sink out-c))
    (pipes/link! (pipes/source rt-c) to-rt)
    (prom/let [rt-inst (run/make-runtime renderer-symbols scheduler2 {:store :remote :id "rt-preview"})
               rt-atom (atom rt-inst)]
      ;; (a/tap in-mult paket-c)
      ;; (handle-render rt-atom paket-c)
      (println "renderer started preview"))))

(defn start-render-runtime
  ""
  [load in out]
  (prom/let [rt-inst (run/make-runtime renderer-symbols scheduler main-conf)]
    (reset! rt rt-inst)
    (println "persisting oasis")
    (kbn/store (:store @rt))
    (println "persist done")
    (pipes/link! (:broadcast @rt) (pipes/sink out))
    (pipes/link! (pipes/source in) (:scheduler @rt))
    (reset! tracer (trace/init-tracer @rt (:tracer config)))
    (println "renderer started runtime" (:id @rt))))
