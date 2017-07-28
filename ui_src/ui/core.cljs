(ns ui.core
  (:require [reagent.core    :as r]
            [reagent.ratom   :refer [reaction]]
            [ui.samak.parser :as parser]
            [ui.samak.emit   :as emit]
            [ui.samak.app]
            [clojure.string  :as string :refer [split-lines]]
            [ui.components   :as ui :refer [grid row col editor
                                            button panel listing
                                            button-group]]))

(set! *warn-on-infer* true)

(enable-console-print!)

(def demo "
chans add-todo;
chan app = ui()
chan initial-todos = from-seq([\"Do foo\" \"Do bar\"])

add-todo-form = () -> [ui/form :todo-form
                        [ui/form-input :todo-text \"Enter Todo!\"]
                        [ui/button {:on-click add-todo ! #todo-text.value
                                    :key \"submit-button\"} \"Add todo!\"]]

initial-todos | add-todo

add-todo
  | std/reductions-tx(conj [])
  | map(list -> [:div [add-todo-form] [ui/unordered-list list]])
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

(r/render
  [root-component]
  (js/document.getElementById "app-container"))
