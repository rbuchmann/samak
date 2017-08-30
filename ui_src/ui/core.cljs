(ns ui.core
  (:require [reagent.core    :as r]
            [reagent.ratom   :refer [reaction]]
            [ui.samak.parser :as parser]
            [ui.samak.emit   :as emit]
            [ui.samak.app]
            [clojure.string  :as string :refer [split-lines]]
            [ui.components   :as ui :refer [grid row col editor
                                            button panel listing
                                            button-group]]
            [ui.editor       :as editor]
            [ui.state        :as s]))

(set! *warn-on-infer* true)

(enable-console-print!)


(def demo "
chans(add-todo)
app = pipes/ui()
initial-todos = pipes/from-seq([\"1\" \"2\"])

http = pipes/http()

; ! + literal = (constantly literal)
add-todo-form = ![ui/form :todo-form
                        [ui/form-input :todo-text \"Enter Number!\"]
                        [ui/button {:on-click add-todo <- #todo-text.value
                                    :key \"submit-button\"} \"Fetch joke!\"]]

initial-todos | add-todo

; #{...} is a function of one argument that will populate every key
; in the result map with the application of the value to the
; argument, provided the value isn't a literal

add-todo
  | map*(joke -> {:url str(\"http://api.icndb.com/jokes/\" joke)})
  | http
  | map*(r -> get-in(r [:value :joke] \"error\"))
  | reductions*(conj [])
  | map*(#[:div [add-todo-form] ui/unordered-list])
  | app

")

(defonce program-src (r/atom demo))

(def parse-tree (reaction (parser/safe-parse @program-src)))

(def parsed-str (reaction (with-out-str
                            (cljs.pprint/pprint @parse-tree))))

(def emitted (reaction (emit/emit-clj @parse-tree)))

(defonce my-state (r/atom ""))

(defonce fs (js/require "fs"))

(def read-file (aget fs "readFileSync"))

(def write-file (aget fs "writeFile"))

(def app-path "ui_src/ui/samak/app.cljs")

(defn emit-to-file []
  (.log js/console "Emitting...")
  (write-file app-path @emitted #(println %)))

(defn reset-app []
  (.log js/console "Resetting...")
  (write-file app-path "(ns ui.samak.app)"))

(defn root-component []
  [:div
   [grid {:fluid true}
    [row
     [col {:md 1}
      [:h1 "사막"]]
     [col {:md 3}
      [:h2 "Code"]]
     [col {:md 4}
      [:h2 "Parse tree"]]
     [col {:md 4}
      [:h2 "Emitted code"]]]
    [row
     [col {:md 1}
      [button-group {:vertical true}
       [button {:on-click emit-to-file
                :bs-style :primary} "Emit"]
       [button {:on-click reset-app
                :bs-style :primary} "Reset"]]]
     [col {:md 3}
      [editor program-src]]
     [col {:md 4}
      [:pre @parsed-str]]
     [col {:md 4}
      [listing emitted]]]]])

(defn setup []
  (when @s/keybinds-needed?
    (s/register-keybindings s/app-state)
    (reset! s/keybinds-needed? false))
  (r/render
   [editor/root s/app-state]
   (js/document.getElementById "app-container")))

(setup)
