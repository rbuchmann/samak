(ns samak.main
  (:require [samak.combiparser :as p]
            [samak.nodes       :as n]
            [samak.core        :as core]))

(defn eval-toplevel-defs [ast]
  (->> ast
       (filter #(= (::n/type %) ::n/def))
       (map (fn [{:keys [::n/name ::n/rhs]}] ; [:-n/name (n/eval-node . :-n/rhs)]
              [name (n/eval-node rhs)]))
       (into {})))

(defn eval-pipes [ast]
  ast)

(defn eval-all [ast]
  (let [defined-symbols (eval-toplevel-defs ast)]
    (binding [n/*symbol-map* (merge core/samak-symbols defined-symbols)]
      (->> ast
           (remove #(= (::n/type %) ::n/def))
           eval-pipes))))

(defn catch-errors [ast]
  (if-let [error (:error ast)]
    (println "There was a parse error: " error)
    (:value ast)))

(defn eval-samak-string [s]
  (some-> s
          p/parse
          catch-errors
          eval-all
          ))
