(ns ui.components
  (:require [reagent.core   :as reagent]
            [goog.functions :as f]))

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
(defonce form-group (to-reagent "FormGroup"))
(defonce form-control (to-reagent "FormControl"))

(def codemirror-base-component (.-default (js/require "react-codemirror2")))

#_(println code-mirror-component)
(def clojure-mode (js/require "codemirror/mode/clojure/clojure"))

(def codemirror-base (reagent/adapt-react-class codemirror-base-component))

(defn editor [state]
  [codemirror-base {:value @state
                    :on-value-change (f/debounce (fn [_ _ value] (reset! state value)) 500 nil)
                    :options {:mode "clojure"
                              :lineNumbers true}}])

(defn listing [state]
  [codemirror-base {:value @state
                    :options {:mode "clojure"
                              :lineNumbers true
                              :readOnly "nocursor"}}])


(defn form [id & args]
  [:form [form-group args]])

(defn form-input [id placeholder]
  (let [val (reagent/atom "")]
    (fn []
      [form-control {:key (str "form-input-" id)
                     :type :text
                     :id id
                     :value @val
                     :placeholder placeholder
                     :on-change #(reset! val (-> % .-target .-value))}])))


(defn unordered-list [children]
  [:ul (map-indexed
        (fn [i child]
          [:li {:key (str i)} child])
        children)])
