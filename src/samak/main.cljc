(ns samak.main
  (:gen-class)
  (:require [clojure.string :as str]
            [samak.repl :as repl]))

(def tp
  (str/split-lines
"in = (pipes/debug {})
out = (pipes/log !)
in | out
!e"))

(def tp2
  (str/split-lines
"x = inc
y = (x (1 !))
!e"))

(def tp3
  "x = (pipes/from-seq ([1 2 3] !))")

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
