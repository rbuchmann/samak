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
            [samak.conveyor :as conv]
            [samak.stdlib   :as std]
            [samak.helpers  :as helpers]
            [samak.terminal :as term]
            [samak.trace    :as trace]
            [samak.metrics  :as metrics]
            [samak.builtins :as builtins]
            [samak.pipes    :as pipes]
            [samak.caravan  :as caravan]
            [samak.halef    :as halef]
            [samak.emit     :as emit]
            [samak.repl     :as repl]))

(def run (atom true))
(def dirty-chan (chan))

(def rt (atom nil))

(def ui-mock-symbols
  {'modules/ui :blank
   'pipes/ui :blank
   'pipes/events :blank
   'pipes/mouse :blank
   'pipes/keyboard :blank})

(def cli-symbols
  (merge builtins/samak-symbols
         std/pipe-symbols
         halef/samak-symbols
         term/samak-symbols))

(defn take-lastv [n v]
  (let [c (count v)]
    (reverse (take n (concat
                       (reverse (subvec v (max 0 (- c n)) c))
                       (repeat ""))))))

(defn draw-repl [input history log prompt]
  [:screen {:cursor {:x (+ 4 (count input)) :y 22}}
   [[:box {:x 1 :y 1} (mapv #(vector :line (str %)) (take-last 10 log))]
    [:box {:x 2 :y 10} (mapv #(vector :line (str %)) (take-lastv 10 history))]
    [:box {:x 2 :y 20} [[:line (str "> " input)]]]
    [:box {:x 2 :y 21} [[:line (str "# " prompt)]]]]])

(defn draw-log-only [log]
  (let [logs (take-last 25 log)]
    [:screen {}
     [[:box {:x 0 :y 0} (mapv #(vector :line (str %)) logs)]]]))

(defn draw-code [code]  )

(defn draw-named [node]
  (emit/emit node))

(defn draw-pipe [p w]
  (let [m (meta p)
        id (:samak.nodes/id m)]
    (helpers/fixstring (str
                        (if id
                          (if-let [code @(rt/load-by-id @rt id)]
                            (draw-named code)
                            (str "unknown:" id))
                          m)
                        " - "
                        (if (satisfies? pipes/Identified p) (.uuid p) p))
                       w)))

(defn draw-break [break]
  (str
   (helpers/fixstring (str (:samak.pipes/content (::msg break))) 60)
   " : "
   (draw-pipe (::from break) 60)
   " xf "
   (draw-pipe (if (satisfies? conv/Station (::to break)) (.xf (::to break))) 60)
   " -> "
   (draw-pipe (::to break) 60)))

(defn draw-debug [breaks k]
  [:screen {}
   [[:box {:x 0 :y 0} [[:line (str "Debug " (count breaks))]]]
    [:box {:x 0 :y 1}  (mapv #(vector :line (draw-break %)) (take-last 10 breaks))]
   [:box {:x 0 :y 12} [[:line (str "Key:" k)]]]]])

(defn log-loop [log-chan log dirty-chan]
  (go-loop []
    (let [line (<! log-chan)
          content (or (:samak.pipes/content (:msg line)) (:msg line) line)
          formatted (str (:time line) " " (or (:source line) "log") ": " content)]
      (swap! log conj formatted))
      (put! dirty-chan true)
    (recur)))

(defn make-debugger [debug-chan]
  (fn [{:keys [:samak.conveyor/from :samak.conveyor/to :samak.conveyor/msg] :as call}]
    (let [d (prom/deferred)]
      (put! debug-chan {::prom d ::from from ::to to ::msg msg})
      d)))

(defn debug-loop [rt
                  debug-chan key-chan
                  draw-debug-chan]
  (go-loop [breaks (atom [])
            k (atom nil)
            halt (atom false)]
    (let [[v port] (a/alts! [key-chan debug-chan])]
      (when (= port debug-chan)
        (if @halt
          (swap! breaks conj v)
          (prom/resolve! (::prom v))))
      (when (= port key-chan)
        (reset! k v)
        (condp = (:key v)
          \space (do
                   (reset! conv/debug (if @halt nil (make-debugger debug-chan)))
                   (when @halt
                     (dorun (map #(prom/resolve! (::prom %)) @breaks))
                     (reset! breaks []))
                   (swap! halt not))
          :enter (when-let [d (first @breaks)]
                   (prom/resolve! (::prom d))
                   (swap! breaks #(vec (rest %))))
          true))
      (put! draw-debug-chan (draw-debug @breaks @k))
      (recur breaks k halt))))

(defn main-loop [runtime
                 draw-repl-chan key-chan draw-plain-chan dirty-chan
                 history log prompt]
  (go-loop [res runtime
            input ""]
    (let [p-chan (a/promise-chan)
          [raw-key port] (a/alts! [key-chan dirty-chan])]
      (if (= port key-chan)
        (prom/let [k (:key raw-key)
                   [input line] (if k
                                  (condp = k
                                    :backspace [(subs input 0 (max 0 (dec (count input)))) nil]
                                    :enter ["" input]
                                    [(str input k) nil]))
                   res (if line
                         (do
                           (swap! history conj (str "history: " line))
                           (-> (repl/eval-line line @rt)
                               (prom/catch #(do
                                              (println "error: " %)
                                              (swap! history conj (str "error: " (ex-message %)))
                                              res))))
                         res)]
          (reset! rt res)
          (reset! prompt "")
          (when-let [modi (or (:alt raw-key) (:ctrl raw-key))]
            (reset! prompt (str "mod: " raw-key)))
          (when (or (= line "!q")
                    (and (:ctrl raw-key)
                         (or (= \c (:key raw-key))
                             (= \d (:key raw-key)))))
            (reset! run false))
          (put! p-chan [res input]))
        (put! p-chan [res input]))
      (let [[res input] (<! p-chan)]
        (put! draw-repl-chan (draw-repl input @history @log @prompt))
        (put! draw-plain-chan (draw-log-only @log))
        (if @run (recur res input))))))

(defn -main [filename & args]
  (do (do ;;(println (with-out-str
  (try
    (let [exit-repl-chan (a/promise-chan)
          exit-debug-chan (a/promise-chan)
          exit-plain-chan (a/promise-chan)
          log-chan (chan 100)
          draw-repl-chan (chan (a/sliding-buffer 1))
          draw-plain-chan (chan (a/sliding-buffer 1))
          draw-debug-chan (chan (a/sliding-buffer 1))
          key-repl-chan (chan 100)
          key-debug-chan (chan 100)
          dirty-chan (chan (a/sliding-buffer 1))
          debug-chan (chan)
          int-ch (chan 100)
          trac (mult int-ch)
          history (atom [])
          log (atom [])
          prompt (atom "Have a cookie")
          ]
      (-> (prom/let [runtime (rt/make-runtime cli-symbols)
                     debugger (make-debugger debug-chan)
                     ;; _ (reset! conv/debug debugger)
                     _ (caravan/init rt)
                     _ (trace/init-tracer rt {:backend :samak :chan int-ch})
                     _ (std/init log-chan)
                     met (metrics/init-metrics)
                     _ (halef/init trac met)
                     lines (some-> filename
                                   slurp
                                   str/split-lines)
                     res (when lines
                           (let [res (repl/eval-lines lines runtime)]
                             (run! #(swap! history conj (str "loaded: " %)) lines)
                             res))
                     _ (repl/init res (if args (str args) "[]"))]
            (reset! rt res)
            (println (:server @rt))
            (log-loop log-chan log dirty-chan)
            (main-loop res draw-repl-chan key-repl-chan draw-plain-chan dirty-chan history log prompt)
            (debug-loop res debug-chan key-debug-chan draw-debug-chan)
            (put! draw-plain-chan (draw-log-only @log))
            (put! draw-repl-chan (draw-repl "" @history @log @prompt))
            (put! draw-debug-chan (draw-debug ["none"] "init"))
            (term/add-screen draw-debug-chan key-debug-chan exit-debug-chan)
            (term/add-screen draw-plain-chan key-repl-chan exit-plain-chan)
            (term/add-screen draw-repl-chan key-repl-chan exit-repl-chan)
            )
          (prom/catch #(println "ERROR" %)))
      (while @run
        (Thread/sleep 100))
      (put! exit-repl-chan true)
      (put! exit-debug-chan true)
      (put! exit-plain-chan true))
    (catch RuntimeException ex (println ex))))))
