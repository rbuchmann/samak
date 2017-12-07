(ns samak.spec-parse
  (:require [clojure.spec.alpha :as s]))

(defmacro postprocess [spec f]
  `(s/conformer (fn [val#]
                  (let [conformed (s/conform ~spec val#)]
                    (if (s/valid? conformed)
                      (f conformed)
                      conformed)))))

(defn qualify-kw
  ([kw] (qualify-kw *ns* kw))
  ([ns kw] (keyword (str ns) (name kw))))

(defn to-spec-params [bindings]
  (mapcat (fn [[k v]] [(keyword k) v]) (partition 2 bindings)))

(defmacro defnodespec [name bindings body]
  `(s/def ~(qualify-kw name)
     (let [spec# (s/cat ~@(to-spec-params bindings))]
       (s/conformer
        (fn [val#]
          (let [conformed# (s/conform spec# val#)
                {:keys ~(mapv first (partition 2 bindings))} conformed#]
            (if (= :clojure.spec.alpha/invalid conformed#)
              :clojure.spec.alpha/invalid
              (assoc ~body
                     :samak.nodes/type ~(qualify-kw "samak.nodes" name)))))))))

(defnodespec define
  [lhs symbol?
   _ #{'=}
   rhs ::literal]
  {:lhs lhs
   :rhs rhs})



(s/def ::map (s/map-of :keyword? ::simple-expression))

(s/def literal-spec (s/or :int            integer?
                          :float-literal  number?
                          :bool-literal   boolean?
                          :symbol-literal symbol?
                          :map-literal    ::map))

(defnodespec literal
  [lit literal-spec]
  (zipmap [:samak.nodes/type :samak.nodes/value] lit))


(defn bin-op [sym]
  (s/cat :fst ::simple-expression :rst (s/* (s/cat :op #{sym} :rst ::simple-expression))))

(s/def ::compose (bin-op '.))

(s/def ::grouped (s/cat ))

(s/def ::simple-expression (s/or ::literal ::literal
                                 ::grouped ::grouped))


(s/def ::def (s/cat :name symbol? :op #{'=} :rhs ::simple-expression))
