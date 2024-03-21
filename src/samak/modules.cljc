(ns samak.modules
  (:require [samak.api :as api]
            [promesa.core :as prom]
            [samak.runtime :as run]
            ))

(defn make-entry
  [[pipe-name pipe]]
  [pipe-name pipe])

(defn defmodule
  [sources sinks]
  {:sources (into {} (map make-entry sources))
   :sinks (into {} (map make-entry sinks))})


(defn load-module
  ""
  [rt mod]
  (prom/let [sources (prom/all (map #(run/load-network rt %) (:roots mod)))
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
  (prom/let [deps (prom/all (map (fn [m] (load-deps rt (first m))) (:dependencies mod)))
          roots (load-module rt mod)]
    {:id id
     :deps deps
     :sinks (:sinks mod)
     :sources (:roots mod)
     :roots roots}))


(defn load-bundle-by-id
  ""
  [rt bundle-id]
  (prom/let [_ (println "  V" "Bundle id:" bundle-id)
             ast (run/load-bundle rt bundle-id)
             _ (println "AST " ast)
             bundle (get ast bundle-id)
             _ (println "### bundle: " bundle)
             deps (load-deps rt [bundle-id bundle])]
    deps))

(defn load-bundle
  ""
  [rt sym]
  (prom/let [_ (println "  V" "Fetching bundle from DB:" sym)
          bundle-id (run/resolve-name rt sym)
          deps (load-bundle-by-id rt bundle-id)]
    deps))

(defn eval-module
  ""
  [rt conf module root ctx]
  (if (contains? conf (:id module))
    (println "### skipping" (:id module))
    (prom/do!
     (println "eval-module" (:id module) "->" module)
     (prom/all (map #(eval-module rt conf % (:id %) ctx) (:deps module)))
     (println "### loading" (:id module))
     (prom/let [roots (:roots module)
             base (if root [root] [])
             _nodes (into base (:nodes roots))
             root-ids (into base (:pipes roots))
             _ (println "[" (:id module) "] roots" root-ids)
             asts (prom/all (map #(run/load-ast @rt %) root-ids))]
       (println "### evaling" (:id module))
       (reset! rt (update @rt :server run/eval-all asts ctx)) ;;; FIXME
       (println "### done" (:id module)))
     )))
