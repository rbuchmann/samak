(ns samak.repl
  #?@
   (:clj
    [(:require
      [clojure.edn :as edn]
      [clojure.string :as str]
      [samak.code-db :as db]
      [samak.core :as core]
      [samak.lisparser :as p]
      [samak.oasis :as oasis]
      [samak.pipes :as pipes]
      [samak.stdlib :as std]
      [samak.tools :as t]
      [samak.runtime :as run])]
    :cljs
    [(:require
      [cljs.reader :as edn]
      [clojure.string :as str]
      [samak.code-db :as db]
      [samak.core :as core]
      [samak.lisparser :as p]
      [samak.oasis :as oasis]
      [samak.pipes :as pipes]
      [samak.stdlib :as std]
      [samak.tools :as t]
      [samak.runtime :as run])]))

(def db (db/create-empty-db))

(def rt (run/make-runtime))

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

#_(defn load-expression
  [db input symbols]
  (let [sym (symbol input)
        _ (println (str "loading " sym))
        ast (db/load-ast db sym)]
    (println "from db: " ast)
    (let [e (eval-exp symbols ast)]
      (println (str "evaled " e))
      e)))

#_(defn fire-event-into-named-pipe
  [symbols pipe-name event]
  (let [pipe (get symbols (symbol pipe-name))]
    (if (pipes/pipe? pipe)
      (do (let [arg (or (get symbols (symbol event))
                        (edn/read-string event))]
            (pipes/fire! pipe arg))
          {})
      (println (str "could not find pipe " pipe-name)))))

#_(defn start-oasis
  [symbols]
  (let [code (reduce eval-exp symbols (oasis/start))]
    (fire-event-into-named-pipe code "oasis" "1")
    code))

(def repl-prefixes

  {\f (fn [in symbols] (let [[pipe-name event] (str/split in #" " 2)]
                     #_(fire-event-into-named-pipe symbols pipe-name event)))
   \s (fn [in _] (db/parse-tree->db! db in) {})
   ;; \q (fn [_ symbols] (oasis/store db) symbols)
   ;; \o (fn [_ symbols] (start-oasis symbols))
   ;; \l (fn [in symbols] (load-expression in symbols))
   ;; \e (fn [_ symbols] (println "Defined symbols:\n" (t/pretty (sort-by first symbols))))
   \p (fn [in _] (println (parse-samak-string in)))})

(defn run-repl-cmd [s defined-symbols]
  (let [[_ dispatch & rst] s]
    (when-let [repl-cmd (repl-prefixes dispatch)]
      (let [new-symbols (repl-cmd (->> rst (apply str) str/trim) defined-symbols)]
        (merge defined-symbols new-symbols)))))

(defn eval-line
  "Evals some input line in the context of the defined symbols,
  and returns a new map of symbols"
  [defined-symbols input]
  (if (str/starts-with? input "!")
    (run-repl-cmd input defined-symbols)
    (when-let [parsed (parse-samak-string input)]
      (println parsed)
      (doseq [expression parsed]
        (std/notify-source expression))
      (reduce eval-exp defined-symbols parsed))))

(defn group-repl-cmds [lines]
  (->> lines
       (partition-by #(str/starts-with? % "!"))
       (mapcat (fn [lines] (if (-> lines first (str/starts-with? "!"))
                            lines
                            [(str/join " " lines)])))))

(defn eval-lines [lines]
  (reduce eval-line (merge core/samak-symbols std/pipe-symbols) (group-repl-cmds lines)))

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
(| in (> [:div id]) out)
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
