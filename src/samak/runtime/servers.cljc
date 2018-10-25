(ns samak.runtime.servers
  (:require [samak.nodes :as n]))

(defprotocol SamakServer
  (eval-ast! [this ast])
  (get-defined [this])
  (unload! [this ids]))

(defrecord LocalSamakServer [defined-ids]
  SamakServer
  (eval-ast! [this {:keys [db/id] :as ast}]
    (binding [n/*environment* defined-ids]
      (let [evaluated (n/eval-node ast)]
        (update this :defined-ids assoc id evaluated))))
  (get-defined [_]
    (keys defined-ids))
  (unload! [this ids]
    (update this :defined-ids #(apply dissoc % ids))))


(defn make-local-server []
  (LocalSamakServer. {}))
