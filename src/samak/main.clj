(ns samak.main
  (:gen-class)
  (:require [clojure.string :as str]
            [samak.repl     :as repl]))

(defn prompt []
  (print "> ")
  (flush)
  (read-line))

(defn input-lines []
  (->> (lazy-seq (cons (prompt) (input-lines)))
       (take-while (fn [line] (not= "!q" (str/trim line))))))

(defn -main [filename & args]
  (-> filename
      slurp
      str/split-lines
      repl/eval-lines))
