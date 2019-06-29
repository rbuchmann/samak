(ns samak.core
  (:require [samak.builtins           :as builtins]
            [samak.caravan            :as c]
            [samak.stdlib             :as std]
            #?(:cljs [samak.ui_stdlib :as uistd])))

(def ui-mock-symbols
  {'pipes/ui       :noop
   'pipes/mouse    :noop
   'pipes/keyboard :noop
   'pipes/layout   :noop})

(def samak-symbols
  (merge builtins/samak-symbols
         c/symbols
         #?(:cljs uistd/ui-symbols)
         std/pipe-symbols))
