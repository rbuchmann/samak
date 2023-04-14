(ns samak.terminal
  (:import com.googlecode.lanterna.screen.Screen
           com.googlecode.lanterna.input.Key
           com.googlecode.lanterna.terminal.Terminal)
  (:use [lanterna.common :only [parse-key]])
  (:require [clojure.string :as str]
            [clojure.core.async :as a :refer [chan put! <! go-loop go mult]]
            [lanterna.constants :as c]
            [lanterna.screen :as s]
            [promesa.core   :as prom]
            [samak.runtime  :as rt]
            [samak.helpers  :as helpers]
            [samak.stdlib   :as std]
            [samak.trace    :as trace]
            [samak.metrics  :as metrics]
            [samak.builtins :as builtins]
            [samak.caravan  :as caravan]
            [samak.halef    :as halef]
            [samak.repl     :as repl]))

(def screens (atom {}))
(def current (atom :none))

(def dirty (atom true))
(def run (atom false))

(defn format-msg [{:keys [:msg :source] :as raw}]
  (if source
    (str source ": " (:samak.pipes/content msg) " <- " (:samak.pipes/meta msg))
    raw))

(defn merge-context [context addition]
  (-> context
      (update-in [:origin :x] + (or (:x addition) 0))
      (update-in [:origin :y] + (or (:y addition) 0))))

(declare draw-component)

(defn draw-box [scr box context]
  (let [children (last box)
        box-context (merge-context context (if (map? (second box)) (second box) {}))]
    (if (not (vector? children)) (draw-component scr [:line (str "no child vector" children)] context)
        (doall (map-indexed (fn [i c] (draw-component scr c (merge-context box-context {:y i})))
                            children)))))

(defn draw-screen [scr screen context]
  (when-let [attrs (second screen)]
    (when (map? attrs)
      (when-let [cursor (:cursor attrs)]
        (s/move-cursor scr (:x cursor) (:y cursor)))))
  (draw-box scr screen context))

(defn draw-component [scr com {:keys [:origin] :as context}]
  (condp = (first com)
    :screen (draw-screen scr com context)
    :box (draw-box scr com context)
    :line (s/put-string scr (:x origin) (:y origin) (str (last com)))
    (s/put-string scr (:x origin) (:y origin) (str "unknown component: " (first com)))))

(defn draw-content [scr in]
  (when in
    (draw-component scr in {:origin {:x 0 :y 0}})))

(defn draw [screen msg]
  (prom/do!
   (when @dirty
     (s/clear screen)
     (draw-content screen msg)
     (s/redraw screen)
     (reset! dirty false))
   (when-let [key (.readInput screen)]
     {:key (parse-key key)
      :alt (.isAltPressed key)
      :ctrl (.isCtrlPressed key)})))

(defn start-loop []
  (when (compare-and-set! run false true)
    (reset! run true)
    (go-loop []
      (let [p-chan (a/promise-chan)
            screen (get @screens @current)]
        (println "scr" @screens)
        (prom/let [raw-key (draw (:screen screen) (:msg screen))
                   res (if raw-key
                         (prom/resolved (put! (:key-chan screen) raw-key))
                         (prom/delay 50 [raw-key nil]))]
          (println "loop" raw-key "")
          (put! p-chan true))
        (and (<! p-chan) (when @run (recur)))))))

(defn start-screen [uuid scr draw-chan exit-chan]
  (go
    (let [exit (<! exit-chan)]
      (s/stop scr)
      (swap! screens dissoc uuid)
      (reset! current (first (first @screens)))))
  (go-loop []
    (let [in (<! draw-chan)]
      (swap! screens assoc-in [uuid :msg] in)
      (reset! dirty true))
    (recur))
  (s/start scr))

(defn add-screen [draw-chan key-chan exit-chan]
  (let [scr (s/get-screen)
        uuid (helpers/uuid)]
    (swap! screens assoc uuid {:screen scr :key-chan key-chan :msg nil})
    (println "screens" @screens)
    (start-screen uuid scr draw-chan exit-chan)
    (reset! current uuid)
    (start-loop)))
