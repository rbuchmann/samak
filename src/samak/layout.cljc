(ns samak.layout
  #?(:cljs (:require [cljsjs.elkjs]))
  ;; (:import [org.eclipse.elk.core.data LayoutMetaDataService])
  )

#?(:cljs (def elk (js/ELK.)))


#?(:cljs
   (defn compute-layout [graph options success error]
     (.log js/console (str "Computing layout..."))
     (->  (.layout (js/ELK.) (clj->js graph) (clj->js (or options {})))
          (.then #(success (js->clj % :keywordize-keys true)))
          (.catch #(error (js->clj % :keywordize-keys true))))))

#?(:clj (defn compute-layout [graph options success error]))
