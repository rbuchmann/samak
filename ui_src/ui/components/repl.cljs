(ns ui.components.repl
  (:require [ui.components  :as ui]
            [reagent.ratom  :refer [reaction]]
            [clojure.string :as str]))

(def vconj (fnil conj []))

(defn make [state db]
  (let [lines (reaction (str/join "\n" (:repl-lines @state)))]
    (fn [state db]
      (println "WAT" lines)
      [ui/grid {:fluid true}
       [ui/row
        [ui/col {:md 2}
         [:h1 "사막"]
         [:h2 "REPL"]
         [ui/listing lines]
         [ui/form :repl
          [ui/submitting-input :repl-txt "Repl here"
           #(swap! state update :repl-lines vconj %)]]]]])))
