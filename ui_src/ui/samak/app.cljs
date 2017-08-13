(ns ui.samak.app
  (:require [ui.samak.stdlib :as std]
            [ui.samak.pipes  :as pipes]
            [ui.samak.core   :refer [map* reductions*]]
            [ui.components   :as ui]
            [cljs.core.async :as a :refer
                             [put! chan <! >! timeout close!]]))
