(ns samak.main
  (:require [samak.combiparser :as p]
            [samak.nodes       :as n]
            [samak.pipes       :as pipes]
            [samak.core        :as core]
            [clojure.string    :as str])
  #?(:clj (:gen-class)))

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
       (partition 2 1)))

(defn eval-pipes [ast defined-symbols]
  (binding [n/*symbol-map* (merge core/samak-symbols defined-symbols)]
    (->> ast
         (filter #(and (= (::n/type %) ::n/binop)
                       (= (::n/op   %) ::n/pipe)))
         (mapcat eval-pipe-op))))

(defn link-all! [pipe-pairs]
  (map (partial apply pipes/link) pipe-pairs))

(defn catch-errors [ast]
  (if-let [error (:error ast)]
    (println "There was a parse error: " error)
    (:value ast)))

(defn parse-samak-string [s]
  (some->> s
           p/parse
           catch-errors))

#?(:clj
   (do
     (defn prompt []
       (print "> ")
       (flush))

     (defn -main [& args]
       (loop [defined-symbols {}]
         (prompt)
         (let [input (read-line)]
           (when (not= (str/trim input) ":quit")
             (let [parsed (parse-samak-string input)
                   new-symbols (some->> parsed
                                        eval-toplevel-defs
                                        (merge defined-symbols))
                   pipe-pairs (eval-pipes parsed new-symbols)]
               (link-all! pipe-pairs)
               (recur new-symbols))))))))
