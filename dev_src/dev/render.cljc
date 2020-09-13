(ns dev.render
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.edn :as edn]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
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
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.lisparser :as p]
     [samak.ui_stdlib :as uistd]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
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
(def rt2 (atom {}))
(def config {:tracer {:backend :none
                      :url "/api/v2/"}})
(def tracer (atom {}))

(def main-conf {"oasis-core" {:depends {}
                              :sinks {:state (sched/make-pipe-id {:module :lone :type :sinks :name :state})}
                              :sources {
                                        :init (sched/make-pipe-id {:module :lone :type :sources :name :init})
                                        :kb (sched/make-pipe-id {:module :lone :type :sources :name :kb})
                                        :drag (sched/make-pipe-id {:module :lone :type :sources :name :drag})
                                        :hover (sched/make-pipe-id {:module :lone :type :sources :name :hover})
                                        :events (sched/make-pipe-id {:module :lone :type :sources :name :events})
                                        }
                              }})


(defn handle-update
  ""
  [msg pipe]
  (let [c (chan)]
    (a/tap (pipes/out-port pipe) c)
    (go-loop []
      (let [p (<! c)]
        (println msg p))
      (recur))))

(defn sched
  [id]
  (fn [broadcast]
    (println "sched" id)
    (let [to-rt (pipes/pipe (chan))]
      ;; (handle-update (str id " out:") broadcast)
      ;; (handle-update (str id " in:") to-rt)
      to-rt)))


(defn fire-event-into-named-pipe
  [rt pipe-name event]
  (let [pipe (run/get-definition-by-name rt (symbol pipe-name))]
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
    (fire-event-into-named-pipe @rt "in" "5")))

(defn run-oasis
  ""
  [rt]
  (fire-event-into-named-pipe rt "oasis-init" "1")
  (fire-event-into-named-pipe rt "oasis-init" "1")
  (println "oasis started")
  ;; (let [parsed [(api/defexp 'start (api/fn-call (api/symbol 'pipes/debug) []))]]
  ;;   (doseq [expression parsed]
  ;;     (caravan/repl-eval expression)))
  )

(defn eval-oasis
  [rt conf cb]
  (let [net (sched/load-bundle @rt 'oasis)]
    (sched/eval-module rt conf net nil)
    (println "eval done \\o/")))

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
  []
  (run-oasis @rt)
  )


(defn start-render-runtime
  ""
  [load in out]
  (reset! rt (run/make-runtime renderer-symbols (sched "main") main-conf))
  (reset! tracer (trace/init-tracer @rt (:tracer config)))
  (println "renderer started runtime" (:id @rt) @rt)
  (oasis/store (:store @rt))
  (eval-oasis rt main-conf load)
  (println "renderer started oasis")

  (pipes/link! (:broadcast @rt) (pipes/pipe out))
  (pipes/link! (pipes/source in) (:scheduler @rt))
  )
