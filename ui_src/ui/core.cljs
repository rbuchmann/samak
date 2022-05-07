(ns ui.core
  (:require [reagent.core         :as r]
            [ui.state             :as s]
            #_[ui.components.repl :as repl]))

(set! *warn-on-infer* true)

(enable-console-print!)

(defonce file-loaded? (atom false))

(defn setup []
  (r/render
   [:h1 "Hi!"]
   #_[repl/make s/app-state s/db]
   (js/document.getElementById "app-container")))

#_(setup)
