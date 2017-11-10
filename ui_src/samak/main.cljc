(ns samak.main
  (:require [samak.combiparser :as p]
            [samak.nodes       :as n]))

(defn eval-toplevel-defs [ast]
  (->> ast)
  (filter #(= (::n/type %) ::n/def))
  (map (fn [{:keys [::n/name ::n/rhs]}]
         [name (n/eval-node rhs)]))
  (into {}))

(defn eval-all [ast]
  (let [symbols ]))

(defn catch-errors [ast]
  (if-let [error (:error ast)]
    (println "There was a parse error: " error)
    ast))

(defn eval-samak-string [s]
  (some-> s
          p/parse
          catch-errors
          eval-all
    ))
