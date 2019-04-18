(ns dev.core
  (:require [samak.repl :as repl]))

(defn init
  ""
  []
  (repl/start-oasis))

(init)
