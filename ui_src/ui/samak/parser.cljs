(ns ui.samak.parser
  (:require [instaparse.core :as insta]
            [clojure.string  :as str]))

(defn to-xforms [m]
  (into {}
        (for [[k f] m]
          [k (fn [& args]
               {:kind k
                :value (apply f args)})])))

(defn parse-int [s]
  (js/parseInt s))

(defn parse-float [s]
  (js/parseDouble s))

(defn parse-bool [s]
  (= "true" s))

(defn ordered [items]
  (into []
        (map-indexed
         (fn [i item]
           (assoc item :order i))
         items)))

(def transforms
  (merge
   (to-xforms
    {:expression-root identity
     :integer         parse-int
     :boolean         parse-bool
     :float           parse-float
     :string          str
     :keyword         :value
     :var             :value
     :fn-literal      identity
     :const-literal   identity
     :map             (fn [& args] (apply hash-map args))
     :identifier      str
     :bin-operator    identity})
   {:tagged-literal (fn [tag literal]
                      {:kind    :tagged-literal
                       :tag     (:value tag)
                       :literal literal})
    :bin-op         (fn [lhs op rhs]
                      {:kind     :bin-op
                       :operator (:value op)
                       :lhs      lhs
                       :rhs      rhs})
    :program        (fn [& args]
                      {:kind        :program
                       :definitions (vec args)})
    :vector         (fn [& args]
                      {:kind     :vector
                       :children (ordered args)})
    :def          (fn [name rhs]
                    {:kind :def
                     :name (:value name)
                     :rhs  rhs})
    :fn-call      (fn [name & args]
                    {:kind      :fn-call
                     :name      (:value name)
                     :arguments (ordered args)})
    :handler      (fn [ch field-id]
                    {:kind     :handler
                     :channel  (:value ch)
                     :field-id field-id})
    :field-id     (fn [id field]
                    {:kind  :field-id
                     :id    (:value id)
                     :field (:value field)})
    :chan-declare (fn [& chans]
                    {:kind  :chan-declare
                     :chans (mapv :value chans)})}))

(def whitespace
  (insta/parser
   "whitespace = #'(\\s|,|;[^\\n]*\\n)+'"))

(def expression-grammar
  "
  expression-root = expression?
  <expression> = fn-call / literal / handler / fn-literal / const-literal / bin-op
  <literal> = string / integer / float / map / vector / keyword / boolean / var
  fn-call = (var <'('> expression* <')'>) | (var <'$'> fn-call)
  fn-literal = <'#'> (map | vector)
  string = ( <'\"'>#'[^\"]*'<'\"'> ) | ( <'\\''>#'[^\\']*'<'\\''> )
  boolean = 'true' | 'false'
  integer = #'[0-9]+'
  float = #'[0-9]+(.[0-9]+)?'
  identifier = #'[a-z][a-z0-9-#/*]*'
  var = identifier
  keyword = <':'> identifier
  map = <'{'> (expression  expression)* <'}'>
  vector = <'['> expression* <']'>
  handler = var <'<-'> field-id
  field-id = <'#'> var <'.'> var
  bin-op = expression bin-operator expression
  bin-operator = '|'
  const-literal = <'!'> literal
  ")

(def program-grammar
  "
  program = statement*
  <statement> = def / chan-declare / bin-op
  chan-declare = <'chans'> <'('> var+ <')'>
  def = var <'='> expression
  ")

(def expression-parser (insta/parser expression-grammar
                                     :auto-whitespace whitespace))

(def parser
  (insta/parser
   (str/join "\n" [program-grammar expression-grammar])
   :auto-whitespace whitespace))

(def parse (comp (partial insta/transform transforms) parser))
(def parse-expression (comp (partial insta/transform transforms) expression-parser))

(defn safe-parse [s]
  (try
    (parse s)
    (catch :default e
      (str "Parse error" e))))

(def debug (comp #(with-out-str (cljs.pprint/pprint %)) parse))


(def tp (insta/parser "
s = expression
<expression> = binop / integer
integer = #'[0-9]+'
<binop> = times / plus
times = expression <'*'> expression
plus = expression <'+'> expression"
                      :auto-whitespace whitespace))
