(ns samak.runtime.servers
  (:require [samak.nodes :as n]))

(defprotocol SamakServer
  (eval-ast [this ast])
  (get-defined [this])
  (load-builtins [this builtins])
  (unload [this ids]))

(defrecord LocalSamakServer [defined-ids builtins]
  SamakServer
  (eval-ast [this {:keys [db/id] :as ast}]
    (update this :defined-ids assoc id (n/eval-env defined-ids builtins ast)))
  (get-defined [_]
    defined-ids)
  (load-builtins [this builtins]
    (update this :builtins merge builtins))
  (unload [this ids]
    (update this :defined-ids #(apply dissoc % ids))))

(defn load-builtins!
  ""
  [server builtins]
  (load-builtins server builtins))

(defn make-local-server []
  (LocalSamakServer. {} {}))
