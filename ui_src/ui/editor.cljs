(ns ui.editor
  (:require [ui.components            :as ui]
            [ui.transitions           :as t]
            [ui.modes                 :as m]
            [reagent.core             :as r]
            [ui.components.graph-view :as graph]
            [ui.transitions           :as transitions]))

(defn root [state]
  (let [{:keys [mode path]} @state
        menu-items (get-in m/modes (list* mode path))]
    [ui/grid {:fluid true}
     [ui/row
      [ui/col {:md 2}
       [:h1 "사막"]
       [:h2 mode]
       [ui/stacked-nav 0 (fn [event-key]
                           (transitions/dispatch-event state event-key))
        (mapv name (keys menu-items))]
       #_(when (:editing? @state)
           [ui/form "Blarb"
            [ui/autofocused-input]])]
      #_[ui/col {:md 10}
         [graph/layout (r/atom graph/example-graph)]]]]))
