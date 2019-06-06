(ns dev.core
  (:require [cljsjs.react]
            [samak.repl :as repl]))

(defn init
  ""
  []
  (repl/start-oasis))

(init)
