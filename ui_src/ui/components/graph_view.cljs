(ns ui.components.graph-view
  (:require [cljsjs.klayjs]
            [reagent.core :as r]))

(def example-graph (clj->js {:id         "root"
                             :properties {:direction "RIGHT" :spacing 40}
                             :children   [{:id "n1" :width 40 :height 40}
                                          {:id "n2" :width 40 :height 40}]
                             :edges      [{:id "e1" :source "n1" :target "n2"}]}))

(defn log-convert [x]
  (let [res (js->clj x :keywordize-keys true)]
    (.log js/console (cljs.pprint/pprint res))
    res))

(defn compute-layout [graph options success error]
  (.layout
   js/$klay
   (clj->js {:graph   graph
             :options options
             :success #(success (log-convert %))
             :error   error})))

(defn draw-nodes [nodes]
  [:g
   (map-indexed (fn [i {:keys [x y width height]}]
                  [:rect {:key i
                          :style {:fill :blue}
                          :x x
                          :y y
                          :width width
                          :height height}])
                nodes)])

(defn draw-edges [edges]
  [:g
   (map-indexed (fn [i {{x1 :x y1 :y} :sourcePoint
                        {x2 :x y2 :y} :targetPoint} ]
                  [:line {:key i
                          :style {:stroke :red}
                          :x1 x1
                          :y1 y1
                          :x2 x2
                          :y2 y2
                          }])
                edges)])

(defn layout [graph]
  (let [layout (r/atom nil)]
    (fn []
      (compute-layout @graph {:spacing 50}
                      #(reset! layout %)
                      #(js/console.log "Layout error: " %))
      (when-let [{:keys [children edges]} @layout]
        [:svg {:width 200
               :height 200}
         [draw-nodes children]
         [draw-edges edges]]))))
