(ns ui.core
  (:require [reagent.core       :as r]
            [ui.state           :as s]
            [ui.components.repl :as repl]))

(set! *warn-on-infer* true)

(enable-console-print!)

;; (when @s/keybinds-needed?
;;   (s/register-keybindings s/app-state)
;;   (reset! s/keybinds-needed? false))

(defn setup []
  (r/render
   [repl/make s/app-state s/db]
   (js/document.getElementById "app-container")))

(setup)
