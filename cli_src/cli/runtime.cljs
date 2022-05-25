(ns cli.runtime
  (:require [cljs.nodejs    :as nodejs]
            [clojure.string :as str]
            [clojure.core.async :as a]
            [promesa.core   :as prom]
            [cognitect.transit :as t]
            [metosin.transit.dates :as d]
            [samak.repl     :as repl]
            [samak.runtime  :as run]
            [samak.stdlib   :as std]
            [samak.builtins :as builtins]
            [samak.caravan  :as caravan]
            [samak.pipes    :as pipes]
            [samak.layout :as layout])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def ui-mock-symbols
  {'modules/ui :blank
   'pipes/ui :blank
   'pipes/events :blank
   'pipes/mouse :blank
   'pipes/keyboard :blank})

(def cli-symbols
  (merge builtins/samak-symbols
         std/pipe-symbols
         caravan/symbols
         ui-mock-symbols
         layout/layout-symbols
         ))

(defn make-scheduler []
  (let [broadcast (pipes/pipe (a/chan) ::main-broadcast)
        to-rt (pipes/pipe (a/chan) ::main-scheduler)]
    (println "sched")
    ;; (handle-update "out" broadcast)
    ;; (handle-update "in" to-rt)
    (fn [] [to-rt broadcast])))

(def worker_threads (nodejs/require "worker_threads"))

(defn make-worker
  ""
  [url]
    (println "make worker" (.-isMainThread worker_threads))
  ;; (println "returning worker: " worker_threads)
  (let [Worker (.-Worker worker_threads)]
    (Worker. url)))

(def json-reader (t/reader :json {:handlers d/readers}))
(defn make-handler
  ""
  [from-worker conf]
  (fn
    [event]
    (let [data (t/read json-reader event)]
      (println "recv from" (:id conf) ":" data)
      (condp = (:target data)
        (a/put! from-worker data)))))

(defn handle-send
  ""
  [worker c conf]
  (let [w (t/writer :json {:handlers d/writers})]
    (go-loop []
      (let [p (<! c)]
        (println "send to" (:id conf) p)
        (.postMessage worker (t/write w p)))
      (recur))))

(defn make-runtime-remote [conf]
  (prom/let [to-worker (a/chan)
             from-worker (a/chan)
             from-mult (a/mult from-worker)
             worker (make-worker "./cli/samak-cli-worker.js")]
    ;; race condition on finished worker code before messaging is set up
    (handle-send worker to-worker conf)
    (.on worker "message" (make-handler from-worker conf))
    {:to to-worker :from from-mult :id (:id conf) :conf conf}))

(defn init-runtime-remote [worker init]
  (let [p (prom/deferred)
        from (a/chan)]
    (a/tap (:from worker) from)
    (go-loop []
      (let [m (<! from)]
        (when (and (= (:target m) :bootstrap))
          (a/put! (:to worker) {:cmd :init :param init}))
        (when (and (= (:target m) :load) (= (:data m) 100))
          (prom/resolve! p true)))
      (recur))
    p))


(defn make-runtime [conf]
  (prom/let [in-main (pipes/pipe-chan ::to-main nil)
             out-main (pipes/pipe-chan ::from-main nil)
             out-mult (a/mult out-main)
             rt (run/make-runtime cli-symbols (make-scheduler) (merge {:modules {}} conf))
             w (first (:connects (:env conf)))]
    (when w
      (a/tap out-mult (:to w))
      (a/tap (:from w) in-main))
    (pipes/link! (pipes/source in-main) (:scheduler rt))
    (pipes/link! (:broadcast rt) (pipes/sink out-main))
    (prom/do!
     (println "init w")
     (when w (init-runtime-remote w :foo))
     (println "done w")
     (repl/init rt))))

(defn eval-lines
  ""
  [lines rt]
  (repl/eval-lines lines rt))
