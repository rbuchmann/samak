(ns ui.editor
  (:require [ui.components            :as ui]
            [reagent.core             :as r]
            [ui.components.graph-view :as graph]))

(defn make-state []
  (r/atom {}))

(defn editor-root [state]
  [ui/grid {:fluid true}
   [ui/menu-bar "사막"]
   [ui/row
    [ui/col {:md 12}
     [graph/layout (r/atom graph/example-graph)]]]])
