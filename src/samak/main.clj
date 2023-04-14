(ns samak.main
  (:gen-class)
  (:import com.googlecode.lanterna.screen.Screen
           com.googlecode.lanterna.input.Key
           com.googlecode.lanterna.terminal.Terminal)
  (:use [lanterna.common :only [parse-key]])
  (:require [clojure.string :as str]
            [clojure.core.async :as a :refer [chan put! <! go-loop mult]]
            [lanterna.constants :as c]
            [promesa.core   :as prom]
            [samak.runtime  :as rt]
            [samak.stdlib   :as std]
            [samak.terminal :as term]
            [samak.trace    :as trace]
            [samak.metrics  :as metrics]
            [samak.builtins :as builtins]
            [samak.caravan  :as caravan]
            [samak.halef    :as halef]
            [samak.repl     :as repl]))

(def run (atom true))

(def ui-mock-symbols
  {'modules/ui :blank
   'pipes/ui :blank
   'pipes/events :blank
   'pipes/mouse :blank
   'pipes/keyboard :blank})

(def cli-symbols
  (merge builtins/samak-symbols
         std/pipe-symbols
         halef/samak-symbols))

(defn take-lastv [v n]
  (let [c (count v)]
    (reverse (take n (concat
                       (reverse (subvec v (max 0 (- c n)) c))
                       (repeat ""))))))

(defn draw-repl [input history log prompt]
  (println "log: " log)
  [:screen {:cursor {:x (+ 4 (count input)) :y 22}}
   [[:box {:x 1 :y 1} (mapv #(vector :line (str %)) (take-lastv log 10))]
    [:box {:x 2 :y 10} (mapv #(vector :line (str %)) (take-lastv history 10))]
    [:box {:x 2 :y 20} [[:line (str "> " input)]]]
    [:box {:x 2 :y 21} [[:line (str "# " prompt)]]]]])

(defn log-loop [log-chan log]
  (go-loop []
    (let [line (<! log-chan)]
      (swap! log conj (str "log: " line))
      (println "log: " line))
    (recur)))

(defn main-loop [rt
                 draw-repl-chan key-chan draw-plain-chan
                 history log prompt]
  (go-loop [res rt
            input ""]
    (let [p-chan (a/promise-chan)
          raw-key (<! key-chan)]
      (reset! prompt "")
      (prom/let [k (:key raw-key)
                 [input line] (if k
                                (condp = k
                                  :backspace [(subs input 0 (max 0 (dec (count input)))) nil]
                                  :enter ["" input]
                                  [(str input k) nil]))
                 res (if line
                       (do
                         (swap! history conj (str "history: " line))
                         (-> (repl/eval-line line rt)
                             (prom/catch #(do
                                            (println "error: " %)
                                            (swap! history conj (str "error: " (ex-message %)))
                                            rt))))
                       rt)]
        (do
          (when-let [modi (or (:alt raw-key) (:ctrl raw-key))]
            (reset! prompt (str "mod: " raw-key)))
          (when (or (= line "!q")
                    (and (:ctrl raw-key)
                         (or (= \c (:key raw-key))
                             (= \d (:key raw-key)))))
            (reset! run false))
          (put! draw-repl-chan (draw-repl input @history @log @prompt))
          (put! p-chan [res input])))
      (let [[res input] (<! p-chan)]
        (if @run (recur res input))))))

(defn -main [filename & args]
  (try
    (let [exit-repl-chan (a/promise-chan)
          exit-plain-chan (a/promise-chan)]
      (-> (prom/let [rt (rt/make-runtime cli-symbols)
                     ;; _ (trace/init-tracer rt {:backend :logging})
                     _ (caravan/init rt)
                     int-ch (chan 100)
                     trac (mult int-ch)
                     _ (trace/init-tracer rt {:backend :samak :chan int-ch})
                     log-chan (chan 100)
                     draw-repl-chan (chan (a/sliding-buffer 1))
                     draw-plain-chan (chan 100)
                     _ (std/init log-chan)
                     met (metrics/init-metrics)
                     _ (halef/init trac met)
                     key-chan (chan 100)
                     history (atom [])
                     log (atom [])
                     prompt (atom "Have a cookie")
                     lines (some-> filename
                                   slurp
                                   str/split-lines)
                     res (when lines
                           (let [res (repl/eval-lines lines rt)]
                             (run! #(swap! history conj (str "loaded: " %)) lines)
                             res))
                     _ (repl/init res (if args (str args) "[]"))]
            (log-loop log-chan log)
            (main-loop res draw-repl-chan key-chan draw-plain-chan history log prompt)
            (put! draw-repl-chan (draw-repl "" @history @log @prompt))
            ;; (put! draw-plain-chan (draw-log-only @log))
            (term/add-screen draw-repl-chan key-chan exit-repl-chan)
            ;; (term/start-loop draw-plain-chan key-chan exit-plain-chan)
            )
          (prom/catch #(println "ERROR" %)))
      (while @run
        (Thread/sleep 100))
      (put! exit-repl-chan true)
      (put! exit-plain-chan true))
    (catch RuntimeException ex (println ex))))
