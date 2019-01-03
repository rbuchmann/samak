(ns cli.node-core
  (:require [cljs.nodejs    :as nodejs]
            [clojure.string :as str]
            [samak.repl     :as repl]))

(nodejs/enable-util-print!)

(def fs ^js/fs (nodejs/require "fs"))

(defn load-samak-file [filename]
  (->> (.readFileSync fs filename)
       str/split-lines
       repl/eval-lines))

(defn -main [& [filename & args]]
  (if (not-empty filename)
    (load-samak-file filename)
    (println "Usage: node samak-cli.js <filename>")))

(set! *main-cli-fn* -main)
