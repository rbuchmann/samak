(ns samak.main
  (:require [samak.combiparser :as p]
            [samak.nodes       :as n]
            [samak.pipes       :as pipes]
            [samak.core        :as core]))

(defn eval-toplevel-defs [ast]
  (->> ast
       (filter #(= (::n/type %) ::n/def))
       (map (fn [{:keys [::n/name ::n/rhs]}]
              [name (n/eval-node rhs)]))
       (into {})))

(defn maybe-wrap-instrumentation [evaluated]
  (if (pipes/pipe? evaluated)
    evaluated
    (pipes/instrument evaluated)))

(defn eval-pipe-op [{:keys [arguments]}]
  (->> (n/eval-reordered arguments)
       (map maybe-wrap-instrumentation)
       (partition 2 1)
       (map (partial apply pipes/link))))

(defn eval-pipes [ast]
  (->> ast
       (filter #(and (= (::n/type %) ::n/binop)
                     (= (::n/op   %) ::n/pipe)))
       (mapcat eval-pipe-op)))

(defn eval-all [ast]
  (let [defined-symbols (eval-toplevel-defs ast)]
    (binding [n/*symbol-map* (merge core/samak-symbols defined-symbols)]
      (eval-pipes ast))))

(defn catch-errors [ast]
  (if-let [error (:error ast)]
    (println "There was a parse error: " error)
    (:value ast)))

(defn eval-samak-string [s]
  (some-> s
          p/parse
          catch-errors
          eval-all))
