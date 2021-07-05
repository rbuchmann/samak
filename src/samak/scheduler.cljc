(ns samak.scheduler
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [promesa.core :as p]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.pipes :as pipes]
     [samak.runtime :as run])]
   :cljs
   [(:require
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
     [promesa.core :as p]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.pipes :as pipes]
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

(defn load-module
  ""
  [rt mod]
  (p/let [sources (p/all (map #(run/load-network rt %) (:roots mod)))
          net (reduce (fn [a, v]
                        (let [val (vals v)]
                          {:nodes (into (:nodes a) (flatten [(map :xf val) (map :ends val)]))
                           :pipes (into (:pipes a) (map :db/id (flatten (map :pipes val))))
                           }))
                      {:nodes (into [] (:roots mod))
                       :pipes []}
                      sources)]
    {:nodes (distinct (:nodes net))
     :pipes (distinct (:pipes net))}))

(defn load-deps
  ""
  [rt [id mod]]
  (println "### load-deps" id mod)
  (p/let [deps (p/all (mapv (fn [m] (load-deps rt (first m))) (:dependencies mod)))
          roots (load-module rt mod)]
    {:id id
     :deps deps
     :sinks (:sinks mod)
     :sources (:roots mod)
     :roots roots}))


(defn load-bundle-by-id
  ""
  [rt bundle-id]
  (p/let [_ (println "  V" "Bundle id:" bundle-id)
          ast (run/load-bundle rt bundle-id)
          bundle (get ast bundle-id)
          _ (println "### bundle: " bundle)
          deps (load-deps rt [bundle-id bundle])]
    deps))

(defn load-bundle
  ""
  [rt sym]
  (p/let [_ (println "  V" "Fetching bundle from DB:" sym)
          bundle-id (run/resolve-name rt sym)
          deps (load-bundle-by-id rt bundle-id)]
    deps))

(defn eval-module
  ""
  [rt conf module root ctx]
  (if (contains? conf (:id module))
    (println "### skipping" (:id module))
    (p/do!
     (println "eval-module" (:id module) "->" module)
     (p/all (map #(eval-module rt conf % (:id %) ctx) (:deps module)))
     (println "### loading" (:id module))
     ;; (p/let [ast (run/load-ast @rt root)]
     ;;   (reset! rt (update @rt :server run/eval-all [ast]))
     ;;   (println "### done" (:id module)))
     (p/let [roots (:roots module)
             base (if root [root] [])
             _nodes (into base (:nodes roots))
             root-ids (into base (:pipes roots))
             _ (println "[" (:id module) "] roots" root-ids)
             asts (p/all (map #(run/load-ast @rt %) root-ids))]
       (println "### evaling" (:id module))
       (reset! rt (update @rt :server run/eval-all asts ctx)) ;;; FIXME
       (println "### done" (:id module)))
     )))

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
    (pipes/link! pipe wrap)
    (pipes/link! wrap (:broadcast @rt))))


(defn setup-outs
  ""
  [rt mod]
  (doall (map (partial setup-out rt) (:sinks mod))))


(defn eval-run-module
  ""
  [rt conf net id]
  (p/let [mod-name (module-id id)
          mod-id (helpers/uuid)
          mod-sym (str mod-id "/" mod-name)]
    (eval-module rt conf net (:id net) mod-id)
    (println (:id @rt) "### module" id (:id net) "done \\o/")
    (run-module rt (:id net) mod-name mod-id)
    (let [mod (run/resolve-fn @rt mod-sym)]
      (println (:id @rt) "mod" mod-sym mod)
      (setup-outs rt mod))
    mod-sym))


(defn start-module
  [rt conf sym id]
  (p/let [net (load-bundle @rt sym)]
    (eval-run-module rt conf net id)))
