(ns ui.state
  (:require [reagent.core :as r]
            [keybind.core :as k]))

(defonce app-state (r/atom {}))

(defn conj-keybinding [m k]
  (update m :keybindings conj k))

(defn reset-keybindings! [m]
  (doseq [[key-string binding-key] (:keybindings @app-state)]
    (k/unbind! key-string binding-key))
  (doall
   (map-indexed (fn [i [key-string handler]]
                  (let [binding-key (keyword "state" (str "k" i))]
                    (k/bind! key-string binding-key handler)
                    (swap! app-state conj-keybinding [key-string binding-key])))
                m)))

(defn reset-app-state! []
  (reset! app-state {}))
