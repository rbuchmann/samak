(ns samak.repl
  #?@
   (:clj
    [(:require
      [clojure.edn :as edn]
      [clojure.string :as str]
      [samak.lisparser :as p]
      [samak.oasis :as oasis]
      [samak.pipes :as pipes]
      [samak.runtime :as run]
      [samak.stdlib :as std]
      [samak.tools :as t]
      [samak.core :as core])]
    :cljs
    [(:require
      [cljs.reader :as edn]
      [clojure.string :as str]
      [samak.lisparser :as p]
      [samak.oasis :as oasis]
      [samak.pipes :as pipes]
      [samak.runtime :as run]
      [samak.stdlib :as std]
      [samak.tools :as t]
      [samak.core :as core])]))

(def rt (run/make-runtime (keys core/samak-symbols)))

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
  (let [pipe (run/get-definition-by-name rt (symbol pipe-name))]
    (if (pipes/pipe? pipe)
      (do (let [arg (edn/read-string event)]
            (pipes/fire! pipe arg))
          {})
      (println (str "could not find pipe " pipe-name)))))

(defn eval-oasis
  ""
  [length]
  (fn [state [nr exp]]
    (println (str (int (* (/ nr length) 100)) "%"))
    (run/eval-expression! state exp)))


(defn start-oasis
  []
  (let [exps (oasis/start)
        numbered (map-indexed vector exps)
        state (reduce (eval-oasis (count numbered)) rt numbered)]
    (println "oasis loaded")
    (fire-event-into-named-pipe "oasis" "1")
    (println "oasis started")
    (run/get-defined-ids state)))

(def repl-prefixes
  {\f (fn [in] (let [[pipe-name event] (str/split in #" " 2)]
                        (fire-event-into-named-pipe pipe-name event)))
   \o (fn [_] (start-oasis))
   \e (fn [_] (println "Defined symbols:\n" (->> rt
                                                run/get-defined-ids
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
      (println parsed)
      (doseq [expression parsed]
        (std/notify-source expression))
      (reduce run/eval-expression! rt parsed))))

(defn group-repl-cmds [lines]
  (->> lines
       (partition-by #(str/starts-with? % "!"))
       (mapcat (fn [lines] (if (-> lines first (str/starts-with? "!"))
                            lines
                            [(str/join " " lines)])))))

(defn eval-lines [lines]
  (doseq [line (group-repl-cmds lines)]
    (eval-line line)))

(def t
  (str/split-lines
   "(def in (|> inc inc))
(def out (pipes/log))
(| in out)
!f in 5"))

(def tm
  (str/split-lines
   "(def in (|> {:foo inc}))
(def out (pipes/log))
(| in out)
!f in 5"))

(def tl
  (str/split-lines
"(def in (pipes/debug))
(def out (pipes/log))
(| in (|> inc inc) out)
!f in 5"))


(def tl2
  (str/split-lines
"(def in (pipes/debug))
(def out (pipes/log))
(| in (|> [:div id]) out)
!f in 42"))

(def tl3
  (str/split-lines
"(def in (pipes/debug))
(def out (pipes/log))
(| in (|> (if even? id ignore)) out)
!f in 5
!f in 6"))

(def tl3b
  (str/split-lines
   "(def in (pipes/debug))
(def out (pipes/log))
(| in (|> (only even?)) out)
!f in 5
!f in 6"))

(def tl4
  (str/split-lines
   "(def in (pipes/debug))
(def out (pipes/log))
(| in (pipes/reductions (|> [:-next :-state] sum) 0) out)
!f in 5
!f in 6"))

(def tl5
  (str/split-lines
   "(def in (pipes/debug))
(def out (pipes/log))
(| in (|> (mapcat (repeat 3))) out)
!f in [5 6]"))
