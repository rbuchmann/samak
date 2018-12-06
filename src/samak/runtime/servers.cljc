(ns samak.runtime.servers
  (:require [samak.nodes :as n]))

(defprotocol SamakServer
  (eval-ast [this ast])
  (get-defined [this])
  (unload [this ids]))

(defrecord LocalSamakServer [defined-ids]
  SamakServer
  (eval-ast [this {:keys [db/id] :as ast}]
    (binding [n/*environment* defined-ids]
      (update this :defined-ids assoc id (n/eval-node ast))))
  (get-defined [_]
    defined-ids)
  (unload [this ids]
    (update this :defined-ids #(apply dissoc % ids))))

(defn make-local-server []
  (LocalSamakServer. {}))
