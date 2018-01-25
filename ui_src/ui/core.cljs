(ns ui.core
  (:require [reagent.core       :as r]
            [reagent.ratom      :refer [reaction]]
            [samak.combiparser  :as parser]
            [samak.emit         :as emit]
            [clojure.string     :as string :refer [split-lines]]
            [ui.components      :as ui]
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
