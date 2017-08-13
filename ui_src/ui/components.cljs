(ns ui.components
  (:require [reagent.core   :as reagent]
            [goog.functions :as f])
  (:require-macros [ui.components :refer [adapt-components]]))

(defonce react-bootstrap (js/require "react-bootstrap"))

(adapt-components
 react-bootstrap
 "Button"
 "Grid"
 "Row"
 "Col"
 "Panel"
 "ButtonGroup"
 "Jumbotron"
 "FormGroup"
 "FormControl"
 "Navbar"
 "Nav"
 "NavItem")

(def nav-bar navbar) ; Consistent naming in react bootstrap ftw.

(def nav-header (-> "react-bootstrap/lib/NavbarHeader"
                    js/require
                    reagent/adapt-react-class))
(def nav-brand (-> "react-bootstrap/lib/NavbarBrand"
                   js/require
                   reagent/adapt-react-class))

(defn menu-bar [brand & items]
  [nav-bar {:inverse true}
   [nav-header [nav-brand brand]]
   [nav
    items]])

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
