(ns ui.styles
  (:require [garden.def :refer [defstyles]]))

(defstyles style
  [:body {:background "#ddd"}]
  [:h1 {:color "#f00"}]
  [:p {:font "18px \"Century Gothic\", Futura, sans-serif"}]
  [:.my-class {:font-size "20px" :background "#ddf"}])
