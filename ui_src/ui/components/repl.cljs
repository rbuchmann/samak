(ns ui.components.repl
  (:require [ui.components  :as ui]
            [reagent.ratom  :refer [reaction]]
            [clojure.string :as str]
            [samak.repl     :as repl]))

(def vconj (fnil conj []))

(defn eval-all-lines [state lines]
  (repl/eval-lines lines)
  (reduce state #(update %1 :repl-lines vconj %2) lines))

(defn eval-line-with-state [state line]
  (repl/eval-line line)
  (update state :repl-lines vconj line))

(defn make [state db]
  (let [lines (reaction (str/join "\n" (:repl-lines @state)))]
    (fn [state db]
      [ui/grid {:fluid true}
       [ui/row
        [ui/col {:md 4}
         [:h1 "사막"]
         [:h2 "REPL"]
         [ui/listing lines]
         [ui/form :repl
          [ui/submitting-input :repl-txt "Repl here"
           #(swap! state eval-line-with-state %)]]]]])))
