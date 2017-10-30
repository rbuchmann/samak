(ns samak.spec-parse
  (:require [clojure.spec.alpha :as s]))

(defmacro postprocess [spec f]
  `(s/conformer (fn [val#]
                  (let [conformed (s/conform ~spec val#)]
                    (if (s/valid? conformed)
                      (f conformed)
                      conformed)))))

(s/def ::map (s/map-of :keyword? ::simple-expression))

(def literal-spec (s/or :int            integer?
                        :float-literal  number?
                        :bool-literal   boolean?
                        :symbol-literal symbol?
                        :map-literal    ::map))

(s/def ::literal (s/conformer
                  (fn [val]
                    (cond->> (s/conform literal-spec val)
                        #(not= :clojure.spec.alpha/invalid %) (zipmap [:type :value])))))


(defn bin-op [sym]
  (s/cat :fst ::simple-expression :rst (s/* (s/cat :op #{sym} :rst ::simple-expression))))

(s/def ::compose (bin-op '.))

(s/def ::grouped (s/cat ))

(s/def ::simple-expression (s/or ::literal ::literal
                                 ::grouped ::grouped))


(s/def ::def (s/cat :name symbol? :op #{'=} :rhs ::simple-expression))
