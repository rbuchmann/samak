(ns ui.transitions
  (:require [ui.modes :refer [modes]]))

(defn dispatch-event [state event-key]
  (println "In dispatch event")
  (let [{:keys [mode path]} @state
        [k action] (some-> modes (get mode) (get-in path) vec (nth event-key))]
    (when action
      (cond
        (keyword? action) (swap! state assoc :mode action :path [])
        (map? action) (swap! state update :path conj k)
        (fn? action) (swap! state action)
        :default (js/console.log "Invalid entry in modes:" (str action))))))
