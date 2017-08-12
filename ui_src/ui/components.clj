(ns ui.components
  (:require [camel-snake-kebab.core :refer [->kebab-case-symbol]]))

(defmacro adapt-components [react & component-names]
  `(do
     ~@(for [cname component-names]
         `(defonce ~(->kebab-case-symbol cname)
            (reagent.core/adapt-react-class (aget ~react ~cname))))))
