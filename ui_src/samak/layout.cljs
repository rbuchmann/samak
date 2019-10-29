(ns samak.layout
 (:require [cljsjs.elkjs]))

(def elk (js/ELK.))

(defn compute-layout [graph options success error]
   (.log js/console (str "Computing layout..." graph))
   (->  (.layout (js/ELK.) (clj->js graph) (clj->js (or options {})))
        (.then #(success (js->clj % :keywordize-keys true)))
        (.catch #(error (js->clj % :keywordize-keys true)))))
