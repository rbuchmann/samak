(ns cli.node-core
  (:require [cljs.nodejs    :as nodejs]
            [clojure.string :as str]
            [cli.runtime    :as rt]
            [promesa.core   :as prom]
            [samak.repl     :as repl]))

(nodejs/enable-util-print!)

(def fs ^js/fs (nodejs/require "fs"))

(defn load-samak-file [filename rt]
  (-> (.readFileSync fs filename)
       str/split-lines
       (rt/eval-lines rt)))

(defn -main [& [filename & args]]
  (if (not-empty filename)
    (prom/let [w (rt/make-runtime-remote {:store :remote :id "cli-w1"})
               rt (rt/make-runtime {:id "cli-main" :env {:connects [w]}})]
               res (load-samak-file filename rt)
      res)

    (println "Usage: node samak-cli.js <filename>")))

(set! *main-cli-fn* -main)
