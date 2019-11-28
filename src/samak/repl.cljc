(ns samak.repl
  #?@
  (:clj
   [(:require
     [clojure.edn :as edn]
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put!]]
     [samak.lisparser :as p]
     [samak.oasis :as oasis]
     [samak.pipes :as pipes]
     [samak.runtime :as run]
     [samak.stdlib :as std]
     [samak.caravan :as caravan]
     [samak.api :as api]
     [samak.tools :as t]
     [samak.trace :as trace]
     [samak.core :as core]
     [samak.test-programs :as test-programs]
     [samak.runtime.servers :as servers]
     [samak.runtime.stores :as stores])]
   :cljs
   [(:require
     [cljs.reader :as edn]
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan close! put!]]
     [samak.lisparser :as p]
     [samak.oasis :as oasis]
     [samak.pipes :as pipes]
     [samak.runtime :as run]
     [samak.stdlib :as std]
     [samak.caravan :as caravan]
     [samak.api :as api]
     [samak.tools :as t]
     [samak.trace :as trace]
     [samak.core :as core]
     [samak.test-programs :as test-programs]
     [samak.runtime.servers :as servers]
     [samak.runtime.stores :as stores])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(def config {:tracer {:backend :none}})

(def rt (atom (run/make-runtime core/samak-symbols nil)))
(def trace (atom (trace/init-tracer rt (:tracer config))))
(caravan/init @rt)

(defn catch-errors [ast]
  (if-let [error (:error ast)]
    (println "There was a parse error: " error)
    (:value ast)))

(defn parse-samak-string [s]
  (some-> s
          p/parse-all
          catch-errors))

(defn eval-exp
  [runtime expression]
  (let [new-symbols (:defined-ids (run/eval-expression! runtime expression))]
    (when-let [latest (:latest new-symbols)]
      (print "EVALED:" latest))
    new-symbols))

(defn fire-event-into-named-pipe
  [pipe-name event]
  (let [pipe (run/get-definition-by-name @rt (symbol pipe-name))]
    (if (pipes/pipe? pipe)
      (do (let [arg (edn/read-string event)]
            (pipes/fire! pipe arg pipe-name))
          {})
      (println (str "could not find pipe " pipe-name)))))

(defn eval-oasis
  ""
  [length cb state [nr exp]]
  (let [progress (int (* (/ nr length) 100))]
    (when (= 0 (mod progress 10))
      (go (>! cb progress))
      (println (str progress "%")))
    (run/eval-expression! state exp)))

(defn run-oasis
  ""
  [state]
  (let [parsed [(api/defexp 'start (api/fn-call (api/symbol 'pipes/debug) []))]]
    ;; (println "oasis loaded: " (str net))
    (reset! rt state)
    (caravan/init @rt)
    (fire-event-into-named-pipe "init" "1")
    (println "oasis started")
    (doseq [expression parsed]
      (caravan/repl-eval expression))
    (servers/get-defined (:server @rt))))


(defn start-oasis
  [cb]
  (let [c (chan)
        exps (oasis/start)
        numbered (map-indexed vector exps)
        cnt (count numbered)]
    (go-loop [state @rt]
      (let [part (<! c)]
        (if part
          (let [state (eval-oasis cnt cb state part)]
            (recur state))
          (run-oasis state))))
    (doall (map #(put! c %) numbered))
    (close! c)))

(def repl-prefixes
  {\f (fn [in] (let [[pipe-name event] (str/split in #" " 2)]
                        (fire-event-into-named-pipe pipe-name event)))
   \o (fn [_] (start-oasis #()))
   \e (fn [_] (println "Defined symbols:\n" (->> @rt
                                                :server
                                                servers/get-defined
                                                t/pretty)))
   \p (fn [in] (println (parse-samak-string in)))})

(defn run-repl-cmd [s]
  (let [[_ dispatch & rst] s]
    (when-let [repl-cmd (repl-prefixes dispatch)]
      (repl-cmd (->> rst (apply str) str/trim)))))

(defn eval-line
  "Evals some input line in the context of the defined symbols,
  and returns a new map of symbols"
  [input]
  (if (str/starts-with? input "!")
    (run-repl-cmd input)
    (when-let [parsed (parse-samak-string input)]
      #_(println parsed)
      #_(doseq [expression parsed]
          (caravan/repl-eval expression))
      (swap! rt #(reduce run/eval-expression! % parsed)))))

(defn group-repl-cmds [lines]
  (->> lines
       (partition-by #(str/starts-with? % "!"))
       (mapcat (fn [lines] (if (-> lines first (str/starts-with? "!"))
                            lines
                            [(str/join " " lines)])))))

(defn eval-lines [lines]
  (doseq [line (group-repl-cmds lines)]
    (eval-line line)))
