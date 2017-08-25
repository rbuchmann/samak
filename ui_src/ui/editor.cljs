(ns ui.editor
  (:require [ui.components            :as ui]
            [reagent.core             :as r]
            [ui.components.graph-view :as graph]))

(defn make-state []
  (r/atom {:editing? false}))

(defn root [state]
  [ui/grid {:fluid true}
   [ui/row
    [ui/col {:md 2}
     [:h1 "사막"]
     [ui/stacked-nav 0 (fn [_] (swap! state update :editing? not)) ["Foo" "bar"]]
     (when (:editing? @state)
       [ui/form "Blarb"
        [ui/autofocused-input]])]
    [ui/col {:md 10}
     [graph/layout (r/atom graph/example-graph)]]]])
