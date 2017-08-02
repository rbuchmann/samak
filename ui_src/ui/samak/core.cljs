(ns ui.samak.core
  (:require [ui.samak.stdlib   :as std]
            [net.cgrand.xforms :as x]))

(defn or* [a b]
  (fn [x]
    (if-some [result (a x)]
      result
      (b x))))

(defn map* [f]
  (std/transduction-pipe (map f)))

(defn filter* [f]
  (std/transduction-pipe (filter f)))

(defn reductions* [f init]
  (std/transduction-pipe (x/reductions f init)))
