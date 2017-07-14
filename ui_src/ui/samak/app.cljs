(ns ui.samak.app
  (:require [ui.samak.stdlib :as std] [ui.samak.pipes :as pipes]))

(def in (pipes/from-seq [1 2 3 4]))

(def out (pipes/log))

(std/link in [(map inc)] out)

(std/start)
