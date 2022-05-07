(ns ui.state
  (:require [reagent.core   :as r]
            [keybind.core   :as k]
            [samak.code-db  :as db]
            [samak.core     :as core]
            [ui.modes       :as modes]
            [ui.transitions :as transitions]))

(defn make-initial-state []
  {:mode            modes/starting-mode
   :path            []
   :repl-lines      []
   :defined-symbols core/samak-symbols})

(defonce app-state (r/atom (make-initial-state)))
(defonce db (db/create-empty-db))
(defonce keybinds-needed? (atom true))

(defn reset-state! [state]
  (reset! state (make-initial-state)))

(defn register-keybindings [state]
  (k/bind! "escape" :state/quit #(reset-state! state))
  (doseq [i (range 10)]
    (k/bind! (str "ctrl-" i)
             (keyword "state" (str "menu" i))
             #(transitions/dispatch-event state (dec i)))))
