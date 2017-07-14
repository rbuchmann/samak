(ns ui.core
  (:require [reagent.core    :as r]
            [reagent.ratom   :refer [reaction]]
            [ui.samak.parser :as parser]
            [ui.samak.emit   :as emit]
            [ui.samak.app]
            [clojure.string  :as string :refer [split-lines]]
            [ui.components   :as ui :refer [grid row col editor button panel listing]]))

(set! *warn-on-infer* true)

(enable-console-print!)

(defonce program-src (r/atom "foo = 1"))

(def parse-tree (reaction (parser/safe-parse @program-src)))

(def parsed-str (reaction (with-out-str
                            (cljs.pprint/pprint @parse-tree))))

(def emitted (reaction (emit/emit-clj @parse-tree)))

(defonce my-state (r/atom ""))

(defonce fs (js/require "fs"))

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
      #_[:h2 "Cmds"]]
     [col {:md 3}
      [:h1 "Code"]]
     [col {:md 4}
      [:h1 "Parse tree"]]
     [col {:md 4}
      [:h1 "Emitted code"]]]
    [row
     [col {:md 1}
      [button {:on-click emit-to-file
               :bs-style "primary"} "Emit"]
      [button {:on-click reset-app
               :bs-style "primary"} "Reset"]]
     [col {:md 3}
      [editor program-src]]
     [col {:md 4}
      [:pre @parsed-str]]
     [col {:md 4}
        [listing emitted]]]]])

(r/render
  [root-component]
  (js/document.getElementById "app-container"))
