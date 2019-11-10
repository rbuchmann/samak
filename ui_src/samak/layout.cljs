(ns samak.layout
 (:require [cljsjs.elkjs]))

(def elk (js/ELK.))

(defn keywordize-type
  ""
  [c]
  (update c :type #(keyword "caravan" %)))

(defn update-child
  ""
  [c]
  (update c :value (fn [v] (mapv keywordize-type v))))


(defn compute-layout [graph options success error]
   (.log js/console (str "Computing layout..." graph))
   (->  (.layout (js/ELK.) (clj->js graph) (clj->js (or options {})))
        (.then (fn [ret] (success (let [res (js->clj ret :keywordize-keys true)]
                                    (update res :children #(map update-child (map keywordize-type %)))))))
        (.catch #(error (js->clj % :keywordize-keys true)))))
