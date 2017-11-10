(ns samak.parse-tools
  (:require [blancas.kern.core :as p]
            [samak.tools       :as tools])
  #?(:cljs (:require-macros [samak.parse-tools :refer [defparser defliteral defbinop]])))

#?(:clj
   (defmacro defparser [name bindings body]
     `(def ~name (p/bind ~bindings
                         (p/return ~body)))))

(defn parser-name [s]
  (symbol (str "p-" s)))

#?(:clj
   (defmacro defliteral [literal-name bindings body]
     `(defparser ~(parser-name literal-name) ~bindings
        #:samak.nodes{:type  ~(tools/qualify-kw "samak.nodes" literal-name)
                      :value ~body})))

(defn with-ws [p]
  (p/between (p/many p/white-space) p))

#?(:clj
   (defmacro defbinop [op-name op-sym p-operands]
     `(defparser ~(parser-name op-name) [fst# ~p-operands
                                         rst# (p/<*> (p/>> (with-ws (p/sym* ~op-sym)) ~p-operands))]
        #:samak.nodes {:type      :samak.nodes/binop
                       :op        ~(tools/qualify-kw "samak.nodes" op-name)
                       :arguments (tools/ordered  (list* fst# rst#))})))
