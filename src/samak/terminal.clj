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
            [samak.helpers  :as helpers]
            [samak.tools    :as tools]
            [samak.pipes    :as pipes]))

(def screens (atom {}))
(def terminal (atom (s/get-screen)))
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

(defn draw-screen [^Screen scr screen context]
  (when-let [attrs (second screen)]
    (when (map? attrs)
      (if-let [cursor (:cursor attrs)]
        (s/move-cursor scr (:x cursor) (:y cursor))
        (.setCursorPosition scr nil))))
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

(defn find-next [ids cur]
  (:res (reduce (fn [acc id]
                  (if (:found acc)
                    {:res id :found false}
                    {:res (:res acc) :found (= cur id)}))
                {:res (first ids) :found false}
                ids)))

(defn handle-key [key chan]
  (if (= (:key key) :end)
    (prom/resolved (do
                     (let [ids (keys @screens)
                           cur @current
                           next (find-next ids cur)]
                       (reset! current next)
                       (reset! dirty true))))
    (prom/resolved (when chan (put! chan key)))))

(defn start-loop []
  (when (compare-and-set! run false true)
    (reset! run true)
    (go-loop []
      (let [p-chan (a/promise-chan)
            scr (get @screens @current)]
        (prom/let [raw-key (draw @terminal (:msg scr))
                   res (if raw-key
                         (handle-key raw-key (:key-chan scr))
                         (prom/delay 50 [raw-key nil]))]
          (put! p-chan true))
        (and (<! p-chan) (when @run (recur)))))
    (s/start @terminal)))

(defn start-screen [uuid draw-chan exit-chan]
  (go
    (let [exit (<! exit-chan)
          scr (swap! screens dissoc uuid)]
      (if (= 0 (count scr))
        (s/stop @terminal)
        (reset! current (first (first @screens))))))
  (go-loop []
    (let [in (<! draw-chan)]
      (swap! screens assoc-in [uuid :msg] in)
      (reset! dirty true))
    (recur)))

(defn add-screen [draw-chan key-chan exit-chan]
  (let [uuid (helpers/uuid)]
    (swap! screens assoc uuid {:key-chan key-chan :msg nil})
    (start-screen uuid draw-chan exit-chan)
    (reset! current uuid)
    (start-loop)))

(defn terminal-module
  ""
  [prefix]
  (println "def term" prefix)
  (fn []
    (let [inst (str prefix "-" (helpers/uuid))
          draw-in (pipes/pipe-chan ::in 100 (map :samak.pipes/content))
          draw-pipe (pipes/sink draw-in)
          key-out (pipes/pipe-chan ::key 100 (map #(helpers/make-paket % ::exit)))
          key-pipe (pipes/pipe key-out)
          exit-in (pipes/pipe-chan ::exit 1 (map :samak.pipes/content))
          exit-pipe (pipes/pipe exit-in)]
      (add-screen draw-in key-out exit-in)
      (println "init terminal" inst)
      ;; (go-loop []
      ;;   (when-let [x (<! draw-in)]
      ;;     (tools/log "terminal-in: " inst " " x)
      ;;     (recur)))
      (let [foo {:sources {:key key-pipe}
                 :sinks {:draw draw-pipe
                         :exit exit-pipe}}]
        (println "terminal is" inst "->" foo)
        foo))))

(def samak-symbols {'modules/terminal terminal-module})
