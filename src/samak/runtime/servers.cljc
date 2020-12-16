(ns samak.runtime.servers
  #?@
  (:clj
   [(:require [clojure.core.async :as a]
              [clojure.edn :as edn]
              [samak.pipes :as pipes]
              [samak.nodes :as n])]
   :cljs
   [(:require [clojure.core.async :as a]
              [cljs.reader :as edn]
              [samak.pipes :as pipes]
              [samak.nodes :as n])]))

(defprotocol SamakServer
  (eval-ast [this ast ctx])
  (get-defined [this])
  (load-builtins [this builtins])
  (unload [this ids]))


(defrecord LocalSamakServer [defined-ids builtins manager]
  SamakServer
  (eval-ast [this {:keys [db/id] :as ast} ctx]
    (println "eval <-" (str ctx "/" id) ast)
    (let [defs (atom (get-defined this))
          man (merge (get this :manager)
                     {:resolve (fn [x] (println "res <-" (str ctx "/" x) (get @defs (str ctx "/" x))) (or (get @defs (str ctx "/" x)) (get @defs (str "/" x))))
                      :register (fn [did def] (println "reg ->" (str ctx "/" did) def) (swap! defs assoc (str ctx "/" did) def))})
          def (n/eval-env man builtins ast {:db-id id :ctx ctx})]
      (swap! defs assoc (str ctx "/" id) def)
      (println "eval ->" (str ctx "/" id) def)
      (assoc this :defined-ids @defs)))
  (get-defined [this]
    (get this :defined-ids))
  (load-builtins [this builtins]
    (update this :builtins merge builtins))
  (unload [this ids]
    (update this :defined-ids #(apply dissoc % ids))))

;; FIXME: think a little about the protocol
;; (defrecord PipedSamakServer [pipe]
;;   SamakServer
;;   (eval-ast [_ ast]
;;     (a/put! pipe (pr-str {:cmd :eval-ast :args {:ast ast}})))
;;   (get-defined [_]
;;     (a/put! pipe (pr-str {:cmd :get-defined :args {}})))
;;   (load-builtins [_ builtins]
;;     (a/put! pipe (pr-str {:cmd :load-builtins :args {:builtins builtins}})))
;;   (unload [_ ids]
;;     (a/put! pipe (pr-str {:cmd :unload :args {:ids ids}}))))

(defn load-builtins!
  ""
  [server builtins]
  (load-builtins server builtins))

(defn make-local-server [manager]
  (LocalSamakServer. {} {} manager))

;; (defn make-piped-server [pipe]
;;   (PipedSamakServer. pipe))
