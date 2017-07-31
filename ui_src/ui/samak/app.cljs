(ns ui.samak.app
  (:require [ui.samak.stdlib :as std]
            [ui.samak.pipes :as pipes]
            [ui.components :as ui]
            [cljs.core.async :as a :refer
                             [put! chan <! >! timeout close!]]))

(do (def add-todo (std/pipe (chan))) (def todos (std/pipe (chan))))

(def app (pipes/ui))

(def initial-todos (pipes/from-seq ["1" "2"]))

(def http (pipes/http))

(def add-todo-form
 (fn [] [ui/form
         :todo-form
         [ui/form-input :todo-text "Enter Number!"]
         [ui/button
          {:key "submit-button",
           :on-click
           (cljs.core/fn
             [& args__29819__auto__]
             (std/fire!
               add-todo
               (cljs.core/aget
                 (js/document.getElementById "todo-text")
                 "value")))} 
          "Fetch joke!"]]))

(std/link initial-todos [] add-todo)

(std/link
  add-todo
  [(map (fn [joke] {:url (str "http://api.icndb.com/jokes/" joke)}))]
  http)

(std/link http [(map (fn [r] (get-in r [:value :joke] "error")))] todos)

(std/link
  todos
  [(std/reductions-tx conj [])
   (map (fn [list] [:div [add-todo-form] [ui/unordered-list list]]))]
  app)

(std/start)
