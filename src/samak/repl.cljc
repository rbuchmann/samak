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

(def rt (atom (run/make-runtime core/samak-symbols)))
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
  (let [new-symbols (:defined-ids (run/eval-expression! runtime expression :broken))]
    (when-let [latest (:latest new-symbols)]
      (print "EVALED:" latest))
    new-symbols))

(defn fire-event-into-named-pipe
  [pipe-name event]
  (prom/let [arg (edn/read-string event)
             res (run/fire-into-named-pipe @rt "" (symbol pipe-name) arg *default-timeout*)]
    (if (:error res)
      (println (:error res)))))

(def repl-prefixes
  {\f (fn [in] (let [[pipe-name event] (str/split in #" " 2)]
                        (fire-event-into-named-pipe pipe-name event)))
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
    (prom/resolved (run-repl-cmd input))
    (prom/let [parsed (parse-samak-string input)
               prt (prom/resolved @rt)
               new (reduce (fn [rt exp] (prom/handle rt (fn [res _] (run/eval-expression! res exp :repl)))) prt parsed)]
      (reset! rt new))))

(defn group-repl-cmds [lines]
  (->> lines
       (partition-by #(str/starts-with? % "!"))
       (mapcat (fn [lines] (if (-> lines first (str/starts-with? "!"))
                            lines
                            [(str/join " " lines)])))))

(defn eval-lines [lines]
  (prom/all (map eval-line (group-repl-cmds lines))))
