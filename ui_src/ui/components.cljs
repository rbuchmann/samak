(ns ui.components
  (:require [reagent.core   :as r]
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
 "NavItem"
 "Badge"
 "HelpBlock")

(def nav-bar navbar) ; Consistent naming in react bootstrap ftw.

(def nav-header (-> "react-bootstrap/lib/NavbarHeader"
                    js/require
                    r/adapt-react-class))
(def nav-brand (-> "react-bootstrap/lib/NavbarBrand"
                   js/require
                   r/adapt-react-class))

(defn menu-bar [brand & items]
  [nav-bar {:inverse true}
   [nav-header [nav-brand brand]]
   [nav
    items]])

(defn stacked-nav [active-key handler items]
  [nav {:bs-style :pills
        :stacked true
        :active-key active-key
        :on-select handler}
   (map-indexed (fn [i item]
                  [nav-item {:event-key i :key i} item [badge {:pull-right true} (inc i)]])
                items)])

(defn autofocused-input [state]
  (let [focus-ref (r/atom nil)]
    (r/create-class
     {:display-name "focus-input"
      :component-did-mount (fn [] (some-> @focus-ref .focus))
      :reagent-render (fn []
                        [form-control
                         {:type :text
                          :input-ref #(reset! focus-ref %)
                          :value @state
                          :on-change #(reset! state (-> % .-target .-value))}])})))

(def codemirror-base-component (.-default (js/require "react-codemirror2")))

(def clojure-mode (js/require "codemirror/mode/clojure/clojure"))

(def codemirror-base (r/adapt-react-class codemirror-base-component))

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
  [:form
   (into [form-group] args)])

(defn form-input [id placeholder]
  (let [val (r/atom "")]
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
