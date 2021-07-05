(ns samak.modules
  (:require [samak.api :as api]))

(defn make-entry
  [[pipe-name pipe]]
  [pipe-name pipe])

(defn defmodule
  [sources sinks]
  {:sources (into {} (map make-entry sources))
   :sinks (into {} (map make-entry sinks))})
