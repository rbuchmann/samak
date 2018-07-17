(ns samak.core
  (:require [samak.builtins :as builtins]
            [samak.caravan  :as caravan]
            [samak.stdlib   :as std]))

(def samak-symbols
  (merge builtins/samak-symbols
         caravan/symbols
         std/pipe-symbols))
