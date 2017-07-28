(ns ui.samak.app
  (:require [ui.samak.stdlib :as std]
            [ui.samak.pipes :as pipes]
            [ui.components :as ui]
            [cljs.core.async :as a :refer
                             [put! chan <! >! timeout close!]]))

(def a (pipes/from-seq [1 2 3]))

(do (def b (std/pipe (chan))))

(def c (pipes/log))

(std/link a [(map inc)] b)

(std/link b [] c)

(std/start)
