(ns samak.repl
  (:require [clojure.string       :as str]
            #?(:clj [clojure.edn  :as edn]
               :cljs [cljs.reader :as edn])
            [samak.lisparser      :as p]
            [samak.core           :as core]
            [samak.api            :as api]
            [samak.nodes          :as n]
            [samak.pipes          :as pipes]
            [samak.stdlib         :as std]))

(defn eval-toplevel-defs [defined-symbols ast]
  (binding [n/*symbol-map* defined-symbols]
    (let [defs (filter (fn [t] (= (::n/type t) ::n/def)) ast)
          evaled (map (fn [{:keys [::n/name ::n/rhs]}]
                        [name (n/eval-node rhs)])
                      defs)]
      (into {} evaled))))

(defn maybe-wrap-instrumentation [evaluated]
  (if (pipes/pipe? evaluated)
    evaluated
    (pipes/instrument evaluated)))

(defn eval-pipe-op [{:keys [samak.nodes/arguments]}]
  (->> (n/eval-reordered arguments)
       (map maybe-wrap-instrumentation)
       (partition 2 1)))

(defn eval-pipes [defined-symbols ast]
  (binding [n/*symbol-map* defined-symbols]
    (->> ast
         (filter #(and (= (::n/type %) ::n/binop)
                       (= (::n/op   %) ::n/pipe)))
         (mapcat eval-pipe-op))))

(defn catch-errors [ast]
  (if-let [error (:error ast)]
    (println "There was a parse error: " error)
    (:value ast)))

(defn parse-samak-string [s]
  (some-> s
          p/parse
          catch-errors))

(def repl-prefixes
  {\f (fn [in symbols] (let [[pipe-name event] (str/split in #" " 2)
                             pipe (get symbols (api/symbol (symbol pipe-name)))]
                         (if (pipes/pipe? pipe)
                           (do (let [arg (or (get symbols (api/symbol (symbol event)))
                                             (edn/read-string event))]
                                 (pipes/fire! pipe arg)))
                           (println (str "could not find pipe " pipe-name)))))
   \e (fn [_ symbols] (println "Defined symbols: " (pr-str symbols)))})

(defn run-repl-cmd [s defined-symbols]
  (let [[_ dispatch & rst] s]
    (when-let [repl-cmd (repl-prefixes dispatch)]
      (repl-cmd (->> rst (apply str) str/trim) defined-symbols))))

(defn eval-exp
  [defined-symbols expression]
  (let [new-symbols (some->> expression
                               (eval-toplevel-defs defined-symbols)
                               (merge defined-symbols))
        pipe-pairs (eval-pipes new-symbols expression)]
    (pipes/link-all! pipe-pairs)
    (or new-symbols defined-symbols)))

(defn eval-line
  "Evals some input line in the context of the defined symbols,
  and returns a new map of symbols"
  [defined-symbols input]
  (if (str/starts-with? input "!")
    (do
      (run-repl-cmd input defined-symbols)
      defined-symbols)
    (let [parsed (parse-samak-string input)]
      (eval-exp defined-symbols parsed))))

(defn eval-lines [lines]
  (reduce eval-line (merge core/samak-symbols std/pipe-symbols) lines))
