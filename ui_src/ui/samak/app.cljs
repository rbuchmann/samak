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
         [ui/form-input :todo-text "Enter Todo!"]
         [ui/button
          {:key "submit-button",
           :on-click
           (cljs.core/fn
             [& args__37142__auto__]
             (std/fire!
               add-todo
               (cljs.core/aget
                 (js/document.getElementById "todo-text")
                 "value")))} 
          "Add todo!"]]))

(std/link initial-todos [] add-todo)

(std/link add-todo [(map (fn [joke] {:a 1}))] http)

(std/link http [] todos)

(std/link
  todos
  [(std/reductions-tx conj [])
   (map (fn [list] [:div [add-todo-form] [ui/unordered-list list]]))]
  app)

(std/start)
