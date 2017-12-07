(ns samak.combiparser
  (:require [blancas.kern.core                :as p]
            [blancas.kern.lexer.haskell-style :as hs]
            [samak.parse-tools                :refer [defbinop defliteral with-ws defparser]]
            [samak.tools                      :as tools]))

(declare p-simple-expression)

;; literals

(def p-identifier (p/<+> (p/many1 (p/<|> p/alpha-num (p/one-of* "-!?")) )))

(def p-ns-identifier (p/<+> p/letter (p/many (p/<|> (p/sym* \.) p-identifier))))

(defparser p-qualified-identifier [ns (p/optional (p/<:> (p/<+> p-ns-identifier (p/skip (p/sym* \/)))))
                                   v p-identifier]
  (symbol ns v))

(defliteral symbol [s p-qualified-identifier]
  s)

(defliteral accessor [_ (p/<:> (p/<*> (p/sym* \:) (p/sym* \-)))
                      v p-qualified-identifier] (keyword v))

(defliteral keyword [_ (p/<?> (p/sym* \:) "keyword literal")
                     v p-qualified-identifier] (keyword v))

(defliteral integer [v hs/dec-lit] v)

(defliteral float [v hs/float-lit] v)

(defliteral string [v hs/string-lit] v)

(def p-map (hs/braces (p/bind [kvs (p/many (p/<*> p-keyword (p/fwd p-simple-expression)))]
                              (p/return
                               #:samak.nodes {:type :samak.nodes/map
                                              :kv-pairs (vec kvs)}))))

(def p-vector (hs/brackets (p/bind [items (p/many (p/fwd p-simple-expression))]
                                   (p/return #:samak.nodes {:type     :samak.nodes/vector
                                                            :children (tools/ordered items)}))))

(def p-literal (p/<|> p-integer p-float p-accessor p-keyword p-string p-symbol p-vector p-map))

;; operators

(defbinop compose \. (p/fwd p-simple-expression))
(defbinop pipe \| (p/fwd p-simple-expression))

;; program expressions

(declare p-fn-call)

(def p-grouped (hs/parens (p/<|> (p/<:> p-compose) (p/fwd p-fn-call))))

(def p-simple-expression (p/<|> (p/<:> p-literal) p-grouped))

(defparser p-fn-call [fn-expression p-simple-expression
                      _ (p/many p/white-space)
                      expression p-simple-expression]
  #:samak.nodes {:type     :samak.nodes/fn-call
                 :fn       fn-expression
                 :argument expression})

(defparser p-def [expression-name p-symbol
                  _ (with-ws (p/sym* \=))
                  rhs p-simple-expression]
  #:samak.nodes {:type :samak.nodes/def
                 :name (:samak.nodes/value expression-name)
                 :rhs  rhs})

(def p-toplevel (p/<|> (p/<:> p-def) p-pipe))

(defn parse [s]
  (let [result (p/parse (p/many (with-ws p-toplevel)) s)]
    (if (:ok result)
      (select-keys result [:value])
      {:error (with-out-str (p/print-error result))})))
