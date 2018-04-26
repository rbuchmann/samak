(ns ui.state
  (:require [reagent.core   :as r]
            [keybind.core   :as k]
            [samak.code-db  :as db]
            [samak.stdlib   :as std]
            [samak.core     :as core]
            [ui.modes       :as modes]
            [ui.transitions :as transitions]))

(defn make-initial-state []
  {:mode            modes/starting-mode
   :path            []
   :repl-lines      []
   :defined-symbols (merge core/samak-symbols std/pipe-symbols)})

(defonce app-state (r/atom (make-initial-state)))
(defonce db (db/create-ratom-db db/schema))
(defonce keybinds-needed? (atom true))

(defn reset-state! [state]
  (reset! state (make-initial-state)))

(defn register-keybindings [state]
  (k/bind! "escape" :state/quit #(reset-state! state))
  (doseq [i (range 10)]
    (k/bind! (str "ctrl-" i)
             (keyword "state" (str "menu" i))
             #(transitions/dispatch-event state (dec i)))))
