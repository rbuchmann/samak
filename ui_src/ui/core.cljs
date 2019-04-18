(ns ui.core
  (:require [reagent.core       :as r]
            [ui.state           :as s]
            [ui.components.repl :as repl]
            [clojure.string     :as str]))

(set! *warn-on-infer* true)

(enable-console-print!)

(def fs ^js/fs (js/require "fs"))

;; (when @s/keybinds-needed?
;;   (s/register-keybindings s/app-state)
;;   (reset! s/keybinds-needed? false))
(def args (->  ^js/electron (js/require "electron") .-remote .-process .-argv))

(defn load-samak-file [state filename]
  (->> (.readFileSync fs filename)
       str/split-lines
       (repl/eval-all-lines state)))

(defonce file-loaded? (atom false))

(defn setup []
  (if-let [filename (->> args (drop 2) first)]
    (when-not @file-loaded?
      (reset! file-loaded? true)
      (swap! s/app-state load-samak-file filename))
    (r/render
     [repl/make s/app-state s/db]
     (js/document.getElementById "app-container"))))

(setup)
