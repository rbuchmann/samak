(ns ui.core
  (:require [reagent.core   :as r]
            [reagent.ratom  :refer [reaction]]
            [samak.parser   :as parser]
            [samak.emit     :as emit]
            [samak.app]
            [clojure.string :as string :refer [split-lines]]
            [ui.components  :as ui :refer [grid row col editor
                                            button panel listing
                                            button-group]]
            [ui.editor      :as editor]
            [ui.state       :as s]))

(set! *warn-on-infer* true)

(enable-console-print!)

(defn setup []
  (when @s/keybinds-needed?
    (s/register-keybindings s/app-state)
    (reset! s/keybinds-needed? false))
  (r/render
   [editor/root s/app-state s/db]
   (js/document.getElementById "app-container")))

(setup)
