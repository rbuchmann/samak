(ns samak.scheduler
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [promesa.core :as prom]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.pipes :as pipes]
     [samak.conveyor :as conv]
     [samak.modules :as modules]
     [samak.runtime :as run])]
   :cljs
   [(:require
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
     [promesa.core :as prom]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.pipes :as pipes]
     [samak.conveyor :as conv]
     [samak.modules :as modules]
     [samak.runtime :as run])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(defn make-pipe-id
  ""
  [name]
  {:target :pipe :named name})

(defn module-id
  ""
  [sym]
  (keyword (str "dynamic-" (name sym))))

(defn run-module
  ""
  [rt id sym ctx]
  (let [_ (println "run-module" sym id)
        exp [(assoc (api/fn-call {:db/id id} [(api/string "test")]) :db/id sym)]]
    (println "run-module2" exp)
    (reset! rt (update @rt :server run/eval-all exp ctx))))

(defn setup-out
  ""
  [rt [key pipe]]
  (println (:id @rt) "### setup out" key)
  (let [wrap (pipes/transduction-pipe (map (run/wrap-out {:named key} :setup)) (str "scheduler-out-" key))]
    (conv/link! pipe wrap)
    (conv/link! wrap (:broadcast @rt))))


(defn setup-outs
  ""
  [rt mod]
  (doall (map (partial setup-out rt) (:sinks mod))))


(defn eval-run-module
  ""
  [rt conf net id]
  (prom/let [mod-name (module-id id)
             mod-id (helpers/uuid)
             mod-sym (str mod-id "/" mod-name)]
    (modules/eval-module rt conf net (:id net) mod-id)
    (println (:id @rt) "### module" id (:id net) "done \\o/")
    (run-module rt (:id net) mod-name mod-id)
    (let [mod (run/resolve-fn @rt mod-sym)]
      (println (:id @rt) "mod" mod-sym mod)
      (setup-outs rt mod))
    mod-sym))


(defn start-module
  [rt conf sym id]
  (prom/let [net (modules/load-bundle @rt sym)]
    (eval-run-module rt conf net id)))
