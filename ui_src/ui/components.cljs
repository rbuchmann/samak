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

(def listing-base-component (.-default (js/require "react-codemirror2")))
(def codemirror-component (js/require "react-codemirror"))

#_(println code-mirror-component)
(defonce haskell-mode (js/require "codemirror/mode/haskell/haskell"))
(defonce clojure-mode (js/require "codemirror/mode/clojure/clojure"))

(def listing-base (reagent/adapt-react-class listing-base-component))
(def codemirror (reagent/adapt-react-class codemirror-component))

(defn editor [state]
  [codemirror {:value @state
                :on-change #(reset! state %)
                :options {:mode "clojure"
                          :lineNumbers true}}])

(defn listing [state]
  [listing-base {:value @state
                 :options {:mode "clojure"
                           :lineNumbers true
                           :readOnly "nocursor"}}])
