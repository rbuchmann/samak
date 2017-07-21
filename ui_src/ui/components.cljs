(ns ui.components
  (:require [reagent.core :as reagent]))

(defonce react-bootstrap (js/require "react-bootstrap"))

(defn to-reagent [s]
  (->> s
       (aget react-bootstrap)
       reagent/adapt-react-class))

(defonce button (to-reagent "Button"))
(defonce grid (to-reagent "Grid"))
(defonce row (to-reagent "Row"))
(defonce col (to-reagent "Col"))
(defonce panel (to-reagent "Panel"))
(defonce button-group (to-reagent "ButtonGroup"))
(defonce jumbotron (to-reagent "Jumbotron"))

(def codemirror-base-component (.-default (js/require "react-codemirror2")))

#_(println code-mirror-component)
(def clojure-mode (js/require "codemirror/mode/clojure/clojure"))

(def codemirror-base (reagent/adapt-react-class codemirror-base-component))

(defn editor [state]
  [codemirror-base {:value @state
                    :on-value-change (fn [_ _ value] (reset! state value))
                    :options {:mode "clojure"
                              :lineNumbers true}}])

(defn listing [state]
  [codemirror-base {:value @state
                    :options {:mode "clojure"
                              :lineNumbers true
                              :readOnly "nocursor"}}])
