(ns samak.scheduler
  #?@
  (:clj
   [(:require
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [samak.api :as api]
     [samak.helpers :as helpers]
     [samak.lisparser :as p]
     [samak.builtins :as builtins]
     [samak.stdlib :as std]
     [samak.pipes :as pipes]
     [samak.runtime :as run])]
   :cljs
   [(:require
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
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
  (let [sources (map #(run/load-network rt %) (:roots mod))
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
  (println "load-deps" id mod)
  (let [deps (:dependencies mod)]
    {:id id
     :deps (mapv (fn [m] (load-deps rt (first m))) deps)
     :sinks (:sinks mod)
     :sources (:roots mod)
     :roots (load-module rt mod)})
  )


(defn load-bundle
  ""
  [rt sym]
  (let [_ (print "  V" "Fetching bundle from DB:" sym)
        bundle-id (run/resolve-name rt sym)
        _ (print "  V" "Bundle id:" bundle-id)
        bundle (get (run/load-bundle rt bundle-id) bundle-id)
        _ (println "bundle: " bundle)
        deps (load-deps rt [bundle-id bundle])]
    deps
))

(defn eval-module
  ""
  [rt conf module root]
  (if (contains? conf (:id module))
    (println "skipping" (:id module))
    (do
      ;; (println "eval" (:id module) "->" module)
      (doall (map #(eval-module rt conf % (:id %)) (:deps module)))
      (println "loading" (:id module))
      (let [roots (:roots module)
            base (if root [root] [])
            root-ids (into (into base (:nodes roots)) (:pipes roots))
            ;; _ (println "[" (:id module) "] roots" root-ids)
            asts (doall (map #(run/load-ast @rt %) root-ids))]
        ;; (println "evaling" (:id module))
        (reset! rt (update @rt :server run/eval-all asts))
        (println "done" (:id module))))))

(defn run-module
  ""
  [rt id sym]
  (let [mod (run/get-definition-by-id @rt id)
        exp [(assoc (api/fn-call {:db/id id} []) :db/id sym)]]
    (reset! rt (update @rt :server run/eval-all exp))))

(defn setup-out
  ""
  [rt [key pipe]]
  (println (:id @rt) "setup out" key)
  (let [wrap (pipes/transduction-pipe (map (run/wrap-out {:named key} :setup)))]
    (pipes/link! pipe wrap)
    (pipes/link! wrap (:broadcast @rt))))


(defn setup-outs
  ""
  [rt mod]
  (doall (map (partial setup-out rt) (:sinks mod))))


(defn start-module
  [rt conf sym]
  (let [net (load-bundle @rt sym)
        mod-name (module-id 'lone)]
    (eval-module rt conf net (:id net))
    (println "module" sym "done \\o/" rt)
    (run-module rt (:id net) mod-name)
    (let [mod (run/resolve-fn @rt mod-name)]
      (println "mod" mod)
      (setup-outs rt mod))))
