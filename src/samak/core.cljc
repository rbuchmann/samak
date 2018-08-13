(ns samak.core
  (:require [samak.builtins :as builtins]
            [samak.stdlib   :as std]))

(def samak-symbols
  (merge builtins/samak-symbols
         std/pipe-symbols))
