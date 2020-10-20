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
  (println "load-deps" id mod)
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
  (p/let [_ (print "  V" "Bundle id:" bundle-id)
          ast (run/load-bundle rt bundle-id)
          bundle (get ast bundle-id)
          _ (println "bundle: " bundle)
          deps (load-deps rt [bundle-id bundle])]
    deps))

(defn load-bundle
  ""
  [rt sym]
  (p/let [_ (print "  V" "Fetching bundle from DB:" sym)
          bundle-id (run/resolve-name rt sym)
          deps (load-bundle-by-id rt bundle-id)]
    deps))

(defn eval-module
  ""
  [rt conf module root]
  (if (contains? conf (:id module))
    (println "skipping" (:id module))
    (p/do!
     ;; (println "eval" (:id module) "->" module)
     (p/all (map #(eval-module rt conf % (:id %)) (:deps module)))
     (println "loading" (:id module))
     (p/let [roots (:roots module)
             base (if root [root] [])
             root-ids (into (into base (:nodes roots)) (:pipes roots))
             ;; _ (println "[" (:id module) "] roots" root-ids)
             asts (p/all (map #(run/load-ast @rt %) root-ids))]
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


(defn eval-run-module
  ""
  [rt conf net sym]
  (p/let [mod-name (module-id 'lone)]
    (eval-module rt conf net (:id net))
    (println (:id @rt) "module" sym "done \\o/")
    (run-module rt (:id net) mod-name)
    (let [mod (run/resolve-fn @rt mod-name)]
      (println (:id @rt) "mod" mod-name mod)
      (setup-outs rt mod))))


(defn start-module
  [rt conf sym]
  (p/let [net (load-bundle @rt sym)]
    (eval-run-module rt conf net sym)))
