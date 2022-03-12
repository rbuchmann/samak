(ns samak.repl
  #?@
  (:clj
   [(:require
     [clojure.edn :as edn]
     [clojure.string :as str]
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put!]]
     [promesa.core :as prom]
     [samak.lisparser :as p]
     [samak.oasis :as oasis]
     [samak.pipes :as pipes]
     [samak.runtime :as run]
     [samak.builtins :as builtins]
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
     [promesa.core :as prom]
     [samak.lisparser :as p]
     [samak.oasis :as oasis]
     [samak.pipes :as pipes]
     [samak.runtime :as run]
     [samak.stdlib :as std]
     [samak.builtins :as builtins]
     [samak.caravan :as caravan]
     [samak.api :as api]
     [samak.tools :as t]
     [samak.trace :as trace]
     [samak.core :as core]
     [samak.test-programs :as test-programs]
     [samak.runtime.servers :as servers]
     [samak.runtime.stores :as stores])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(def ^:dynamic *default-timeout* 0)
(def config {:tracer {:backend :none}})

(def rt (atom nil))
(def trace (atom (trace/init-tracer rt (:tracer config))))

(def cli-symbols
  (merge builtins/samak-symbols
         std/pipe-symbols
         caravan/symbols
         ;; #?(:cljs layout/layout-symbols)
         ;; #?(:cljs uistd/ui-symbols)
         ))

(def scheduler
  (let [broadcast (pipes/pipe (chan) ::main-broadcast)
        to-rt (pipes/pipe (chan) ::main-scheduler)]
    (println "sched")
    ;; (handle-update "out" broadcast)
    ;; (handle-update "in" to-rt)
    (fn [] [to-rt broadcast])))

(def cli-conf {:id "rt-cli"
                :modules {}})

(defn init []
  (prom/let [rt-inst (run/make-runtime cli-symbols scheduler cli-conf)]
    (reset! rt rt-inst)
    (caravan/init @rt)))

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
  (let [new-symbols (:defined-ids (run/eval-expression! runtime expression :broken))]
    (when-let [latest (:latest new-symbols)]
      (print "EVALED:" latest))
    new-symbols))

(defn fire-event-into-named-pipe
  [runtime pipe-name event]
  (prom/let [arg (edn/read-string event)
             rt (prom/resolved runtime)
             res (run/fire-into-named-pipe rt :repl (symbol pipe-name) arg *default-timeout*)]
    ;; (println "###fire" pipe-name event)
    (if (:error res)
      (println (:error res)))))

(def repl-prefixes
  {\f (fn [in rt] (let [[pipe-name event] (str/split in #" " 2)]
                        (fire-event-into-named-pipe rt pipe-name event)))
   \e (fn [_ _] (prom/resolved (println "Defined symbols:\n" (->> @rt
                                                                :server
                                                                servers/get-defined
                                                                t/pretty))))
   \p (fn [in _] (prom/resolved (println (parse-samak-string in))))})

(defn run-repl-cmd [s rt]
  (let [[_ dispatch & rst] s]
    (when-let [repl-cmd (repl-prefixes dispatch)]
      (println "###cmd" s)
      (repl-cmd (->> rst (apply str) str/trim) rt))))

(def foo (atom 1))

(defn eval-line
  "Evals some input line in the context of the defined symbols,
  and returns a new map of symbols"
  [input runtime]
  (if (str/starts-with? input "!")
    (do
      (run-repl-cmd input runtime)
      runtime)
    (prom/let [parsed (parse-samak-string input)
               prt (prom/resolved runtime)
               new (reduce (fn [rt exp] (prom/handle rt (fn [res _] (println "eval" exp) (run/eval-expression! res exp :repl)))) prt parsed)]
      (reset! rt new)
      new)))

(defn group-repl-cmds [lines]
  (->> lines
       (partition-by #(str/starts-with? % "!"))
       (mapcat (fn [lines] (if (-> lines first (str/starts-with? "!"))
                            lines
                            [(str/join " " lines)])))))

(defn eval-lines [lines]
  (prom/let [evals (reduce (fn [rt exp] (prom/handle rt (fn [res _] (eval-line exp res)))) (prom/resolved @rt) (group-repl-cmds lines))]
    evals))
