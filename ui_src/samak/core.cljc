(ns samak.core
  (:require [samak.builtins           :as builtins]
            [samak.stdlib             :as std]
            ;#?(:cljs [samak.ui_stdlib :as uistd])
            ;#?(:cljs [samak.layout    :as layout])
            ))

(def ui-mock-symbols
  {'pipes/ui       :noop
   'pipes/mouse    :noop
   'pipes/keyboard :noop
   'pipes/layout   :noop})

(def samak-symbols
  (merge builtins/samak-symbols
         ; #?(:cljs uistd/ui-symbols)
         ; #?(:cljs layout/layout-symbols)
         std/pipe-symbols))
