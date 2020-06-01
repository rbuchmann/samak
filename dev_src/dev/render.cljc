(ns dev.render
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.edn :as edn]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [clojure.walk :as w]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.lisparser :as p]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
     [samak.caravan :as caravan]
     [samak.pipes :as pipes]
     [samak.runtime :as run]
     [samak.runtime.stores :as stores])]
   :cljs
   [(:require
     [clojure.string :as str]
     [cljs.reader :as edn]
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
     [clojure.walk :as w]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.lisparser :as p]
     [samak.ui_stdlib :as uistd]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.trace :as trace]
     [samak.oasis :as oasis]
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
         std/pipe-symbols
         caravan/symbols
         ;; std-mock-symbols
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
      (handle-update "out" broadcast)
      (handle-update "in" to-rt)
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
      (println "render in paket" p)
      (when (= :samak.runtime/paket (:samak.runtime/type p))
        (when-let [pipe (get-named-pipe-memo (:samak.runtime/target p))]
          (pipes/fire-raw! pipe content))
        (trace/trace ::render-in
                     (helpers/duration before (helpers/now))
                     content)))
    (recur)))

(def renderer
  ["(def mod (modules/ui \"2\"))"
   "(def ui ((-> mod :-sinks :-render) 42))"
   "(| ((-> mod :-sources :-events) 42) (pipes/to-ui))"
   "(| ((-> mod :-sources :-mouse) 42) (pipes/to-mouse))"
   "(| ((-> mod :-sources :-keyboard) 42) (pipes/to-keyboard))"])

;; (defn eval-render
;;   ""
;;   []
;;   (let [code (str/join " " renderer)
;;         parsed (p/parse-all code)]
;;     (swap! rt #(reduce run/eval-expression! % (:value parsed)))))

(defn fire-event-into-named-pipe
  [pipe-name event]
  (let [pipe (run/get-definition-by-name @rt (symbol pipe-name))]
    (if (pipes/pipe? pipe)
      (do (let [arg (edn/read-string event)]
            (pipes/fire! pipe arg pipe-name))
          {})
      (println (str "could not find pipe " pipe-name)))))

(defn eval-oasis
  ""
  [length cb state [nr exp]]
  (println "eval" nr exp)
  (let [progress (int (* (/ nr length) 100))]
    (when (= 0 (mod progress 10))
      (put! cb progress)
      (println (str progress "%")))
    (update state :server run/eval-all [exp])
    ))

(defn run-oasis
  ""
  [state cb]
  (put! cb 100)
  (reset! rt state)
  (caravan/init @rt)
  (fire-event-into-named-pipe "oasis-init" "1")
  (println "oasis started")
  (let [parsed [(api/defexp 'start (api/fn-call (api/symbol 'pipes/debug) []))]]
    (doseq [expression parsed]
      (caravan/repl-eval expression))))

(defn load-bundle
  ""
  [sym rt]
  (let [_ (print "  V" "Fetching bundle from DB: " sym)
        bundle (run/load-bundle rt sym)
        _ (println bundle)
        sources (map #(run/load-network rt %) bundle)
        net (reduce (fn [a, v]
                      (let [val (vals v)]
                        {:nodes (into (:nodes a) (flatten [(map :xf val) (map :ends val)]))
                         :pipes (into (:pipes a) (map :db/id (flatten (map :pipes val))))
                         }))
                    {:nodes (into [] bundle)
                     :pipes []}
                    sources)]
    net
))

(defn load-ast
  "loads an ast given by its entity id from the database"
  [rt id]
  (w/postwalk (fn [form]
                  (if-let [sub-id (when (and (map? form) (= (keys form) [:db/id]))
                                    (:db/id form))]
                    (run/load-by-id rt sub-id)
                    form))
              (run/load-by-id rt id)))

(defn start-oasis
  [cb]
  (let [c (chan)
        net (load-bundle 'oasis @rt)
        _ (println net)
        exps (into (into [] (distinct (:nodes net))) (distinct (:pipes net)))
        _ (println exps)
        source (map #(load-ast @rt %) exps)
        ;; _ (println source)
        numbered (map-indexed vector source)
        cnt (count numbered)]
    (go-loop [state @rt]
      (let [part (<! c)]
        (if part
          (let [state (eval-oasis cnt cb state part)] ;
            (recur state))
          (run-oasis state cb))))
    (doall (map #(put! c %) numbered))
    (close! c)))

(defn trace
  [src duration msg]
  (trace/trace src duration msg))

(defn start-render-runtime
  ""
  [load in out]
  (reset! rt (run/make-runtime renderer-symbols sched))
  (reset! tracer (trace/init-tracer @rt (:tracer config)))
  (println "renderer started runtime" (:id @rt) @rt)
  (oasis/store (:store @rt))
  (start-oasis load)
  (println "renderer started oasis")
  (pipes/link! (:broadcast @rt) (pipes/sink out))
  ;; (pipes/link! (pipes/source in) (:scheduler @rt))
  (handle-render in)
  )
