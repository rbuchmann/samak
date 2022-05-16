(ns cli.node-core
  (:require [cljs.nodejs    :as nodejs]
            [clojure.string :as str]
            [promesa.core   :as prom]
            [samak.repl     :as repl]))

(nodejs/enable-util-print!)

(def fs ^js/fs (nodejs/require "fs"))

(defn load-samak-file [filename rt]
  (->> (.readFileSync fs filename)
       str/split-lines
       (#(repl/eval-lines %1 rt))))

(defn -main [& [filename & args]]
  (if (not-empty filename)
    (prom/let [rt (repl/init)
               res (load-samak-file filename rt)]
      res)

    (println "Usage: node samak-cli.js <filename>")))

(set! *main-cli-fn* -main)
