(ns ui.samak.parser
  (:require [instaparse.core :as insta]))

(defn log [& x]
  (println x)
  x)

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
    {:integer    parse-int
     :boolean    parse-bool
     :float      parse-float
     :string     str
     :keyword    :value
     :var        :value
     :map        (fn [& args] (apply hash-map args))
     :identifier str
     :chan-ref   :value
     :chan-src   :value})
   {:tagged-literal (fn [tag literal]
                      {:kind    :tagged-literal
                       :tag     (:value tag)
                       :literal literal})
    :program        (fn [& args]
                      {:kind        :program
                       :definitions (vec args)})
    :vector         (fn [& args]
                      {:kind     :vector
                       :children (ordered args)})
    :pipe-def (fn [from transducers to]
                {:kind :pipe-def
                 :transducers transducers
                 :from from
                 :to to})
    :transducers    (fn [& args]
                      {:kind     :transducers
                       :children (ordered args)})
    :def            (fn [name rhs]
                      {:kind :def
                       :name (:value name)
                       :rhs  rhs})
    :chan-def       (fn [name rhs]
                      {:kind :chan-def
                       :name (:value name)
                       :rhs  rhs})
    :fn-call        (fn [name & args]
                      {:kind      :fn-call
                       :name      (:value name)
                       :arguments (ordered args)})
    :lambda (fn [[_ & params] rhs]
              {:kind :lambda
               :params (if (map? params)
                         [(assoc params :order 0)]
                         (ordered params))
               :rhs rhs})
    :handler (fn [ch field-id]
               {:kind :handler
                :channel (:value ch)
                :field-id field-id})
    :field-id (fn [id field]
                {:kind :field-id
                 :id (:value id)
                 :field (:value field)})
    :chan-declare (fn [& chans]
                    {:kind :chan-declare
                     :chans (mapv :value chans)})}))

(def whitespace
  (insta/parser
   "whitespace = #'(\\s|,|;[^\\n]*\\n)+'"))

(def parser
  (insta/parser
   "
program = statement*
<statement> = def / chan-def / pipe-def / chan-declare
<expression> = fn-call / literal / chan-ref / tagged-literal / lambda / handler
handler = var <'<-'> field-id
chan-declare = <'chans'> <'('> var+ <')'>
field-id = <'#'> var <'.'> var
lambda = params <'->'> expression
params = ( var | <'('> var* <')'> )
chan-def = <'chan'> var <'='> fn-call
pipe-def = var <'|'> transducers var
transducers = (fn-call <'|'>)*
fn-call = (var <'('> expression* <')'>) | (var <'$'> fn-call)
def = var <'='> expression
<literal> = string / integer / float / map / vector / keyword / boolean / var
tagged-literal = <'#'> var literal
string = ( <'\"'>#'[^\"]*'<'\"'> ) | ( <'\\''>#'[^\\']*'<'\\''> )
boolean = 'true' | 'false'
integer = #'[0-9]+'
float = #'[0-9]+(.[0-9]+)?'
identifier = #'[a-z][a-z0-9-#/]*'
var = identifier
chan-ref = <'?'> identifier
keyword = <':'> identifier
map = <'{'> (expression  expression)* <'}'>
vector = <'['> expression* <']'>
"
   :auto-whitespace whitespace))

(def parse (comp (partial insta/transform transforms) parser))

(defn safe-parse [s]
  (try
    (parse s)
    (catch :default e
      "Parse error")))

(def debug (comp cljs.pprint/pprint parse))


(def tp (insta/parser "
s = expression
<expression> = binop / integer
integer = #'[0-9]+'
<binop> = times / plus
times = expression <'*'> expression
plus = expression <'+'> expression"
                      :auto-whitespace whitespace))
