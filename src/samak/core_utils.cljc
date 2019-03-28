(ns samak.core-utils
  #?@
  (:clj
   [(:require [samak.protocols :as p]
              [clojure.string  :as str])]
   :cljs
   [(:require [samak.protocols :as p]
              [clojure.string  :as str])
    (:require-macros [samak.core-utils :refer [samakify samakify-all]])]))


;; Args marked as fixed aren't applied to the input,
;; just passed to the underlying function unchanged.
;; This is needed mostly for higher order functions.
(defn fixed-sym? [s]
  (str/starts-with? (name s) "!"))

(defmacro samakify
  "Turns a function or special form into a samak higher order function.
  All parameters called f<number?> will be taken to be function
  arguments and not applied to the input in the returned closure."
  [f argvec]
  (let [[p-args [_ v-arg]] (split-with #(not= '& %) argvec) ; positional + variadic args
        sym-map            (into {} (for [arg (cons v-arg p-args)]
                                      [arg (gensym arg)]))
        input-sym          (gensym 'input)
        fn-name            (gensym 'fn-name)
        arg-list           (concat (for [arg p-args]
                                     (if (fixed-sym? arg)
                                       (sym-map arg)
                                       (list (sym-map arg) input-sym)))
                                   (when v-arg
                                     [`(map (fn [f#] (f# ~input-sym)) ~(sym-map v-arg))]))]
    `(fn ~fn-name
       (~argvec
        (let [~@(apply concat (for [arg p-args]
                                [(sym-map arg) `(p/eval-as-fn ~arg)]))
              ~@(when v-arg
                  [(sym-map v-arg) `(map p/eval-as-fn ~v-arg)])]
          (fn [~input-sym]
            ~(if v-arg
               `(apply ~f ~@arg-list)
               `(~f ~@arg-list))))))))

(defmacro samakify-all [& defns]
  (into {}
        (for [dfn defns]
          (if (vector? dfn)
            (let [[f-sym [f argvec]] dfn]
              `['~f-sym (samakify ~f ~argvec)])
            (let [[f argvec] dfn]
              `['~f (samakify ~f ~argvec)])))))

;; (samakify-all (if [pred then else]) [foo (inc [?])] (map [f col]) (concat [& args]))
