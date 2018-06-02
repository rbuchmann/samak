(ns samak.repl
  (:require [clojure.string       :as str]
            #?(:clj [clojure.edn  :as edn]
               :cljs [cljs.reader :as edn])
            [samak.lisparser      :as p]
            [samak.core           :as core]
            [samak.api            :as api]
            [samak.nodes          :as n]
            [samak.pipes          :as pipes]
            [samak.tools          :as t]
            [samak.code-db        :as db]
            [samak.oasis          :as oasis]
            [samak.stdlib         :as std]))

(def db (db/create-empty-db))

(defn maybe-wrap-instrumentation [evaluated]
  (if (pipes/pipe? evaluated)
    evaluated
    (pipes/instrument evaluated)))

(defn eval-pipe-op [{:keys [samak.nodes/arguments]}]
  (->> (n/eval-reordered arguments)
       (map maybe-wrap-instrumentation)
       (partition 2 1)))

(defn eval-toplevel-ast [defined-symbols ast]
  (binding [n/*symbol-map* defined-symbols]
    (condp (fn [k form] (= (::n/type form) k)) ast
      ::n/def (let [{:keys [::n/name ::n/rhs]} ast
                    node (n/eval-node rhs)
                    new-symbols (assoc defined-symbols name node)]
                [node new-symbols])
      ::n/pipe (let [pipe-pairs (eval-pipe-op ast)]
                 (pipes/link-all! pipe-pairs)
                 [pipe-pairs defined-symbols])
      [(n/eval-node ast) defined-symbols])))

(defn catch-errors [ast]
  (if-let [error (:error ast)]
    (println "There was a parse error: " error)
    (:value ast)))

(defn parse-samak-string [s]
  (some-> s
          p/parse
          catch-errors))

(defn eval-exp
  [defined-symbols expression]
  (let [[result new-symbols] (eval-toplevel-ast defined-symbols expression)]
    (print result)
    (or new-symbols defined-symbols)))

(defn fire-event-into-named-pipe
  [symbols pipe-name event]
  (let [pipe (get symbols (symbol pipe-name))]
    (if (pipes/pipe? pipe)
      (do (let [arg (or (get symbols (symbol event))
                        (edn/read-string event))]
            (pipes/fire! pipe arg))
          {})
      (println (str "could not find pipe " pipe-name)))))

(defn persist-expression
  [input]
  (let [exp (parse-samak-string input)]
    (println (str "persisting expression: " exp))
    (db/parse-tree->db db [exp])))

(defn load-expression
  [input symbols]
  (let [sym (symbol input)
        _ (println (str "loading " sym))
        ast (db/load-ast db sym)]
    (println "from db: " ast)
    (let [e (eval-exp symbols ast)]
      (println (str "evaled " e))
      e)))

(defn start-oasis
  [symbols]
  (let [code (reduce (fn [a e] (merge a (eval-exp a [e]))) symbols (oasis/start))]
    (fire-event-into-named-pipe code "oasis" "1")
    code))


(def repl-prefixes
  {\f (fn [in symbols] (let [[pipe-name event] (str/split in #" " 2)]
                         (fire-event-into-named-pipe symbols pipe-name event)))
   \s (fn [in _] (persist-expression in) {})
   \o (fn [_ symbols] (start-oasis symbols))
   \l (fn [in symbols] (load-expression in symbols))
   \e (fn [_ symbols] (println "Defined symbols:\n" (t/pretty (sort-by first symbols))))
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
      (std/notify-source parsed)
      (eval-exp defined-symbols parsed))))

(defn eval-lines [lines]
  (reduce eval-line (merge core/samak-symbols std/pipe-symbols) lines))


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
(| in [:div id] out)
!f in 42"))

(def tl3
  (str/split-lines
"(def in (pipes/debug))
(def out (pipes/log))
(| in (filter even?) out)
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
(| in (mapcat (repeat 3)) out)
!f in 5
!f in 6"))
