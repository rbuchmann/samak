(ns samak.main
  (:gen-class)
  (:require [clojure.string :as str]
            [promesa.core   :as prom]
            [samak.runtime  :as rt]
            [samak.stdlib   :as std]
            [samak.builtins :as builtins]
            [samak.caravan  :as caravan]
            [samak.repl     :as repl]))

(def ui-mock-symbols
  {'modules/ui :blank
   'pipes/ui :blank
   'pipes/events :blank
   'pipes/mouse :blank
   'pipes/keyboard :blank})

(def cli-symbols
  (merge builtins/samak-symbols
         std/pipe-symbols))

(defn prompt []
  (print "> ")
  (flush)
  (read-line))

(defn input-lines []
  (->> (lazy-seq (cons (prompt) (input-lines)))
       (take-while (fn [line] (not= "!q" (str/trim line))))))

(defn -main [filename & args]
  (prom/let [rt (rt/make-runtime cli-symbols)
             lines (-> filename
                       slurp
                       str/split-lines)
             res (repl/eval-lines lines rt)]
    (println "program running...")
    ;; (println "res" res)
    (Thread/sleep Long/MAX_VALUE)))
