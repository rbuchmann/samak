(ns samak.core
  (:require [samak.builtins :as builtins]
            [samak.caravan  :as c]
            [samak.stdlib   :as std]))

(def samak-symbols
  (merge builtins/samak-symbols
         c/symbols
         std/pipe-symbols))
