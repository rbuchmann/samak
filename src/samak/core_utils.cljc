(ns samak.core-utils
  (:require [samak.protocols :as p])
  #?(:cljs (:require-macros samak.core-utils :refer [samakify samakify-all])))

(defn fn-sym? [s]
  (re-matches #"f[0-9]*" (name s)))

(defmacro samakify
  "Turns a function or special form into a samak higher order
  function. Some parameters have a special meaning:
  - ? can be ommitted and will be replaced by identity
  - All parameters called f<number?> will be taken to be function
    arguments and not applied to the input in the returned closure."
  [f argvec]
  (let [[p-args [_ v-arg]] (split-with #(not= '& %) argvec) ; positional + variadic args
        sym-map            (into {} (for [arg (cons v-arg p-args)]
                                      [arg (gensym arg)]))
        input-sym          (gensym 'input)
        fn-name            (gensym 'fn-name)
        arg-list           (concat (for [arg p-args]
                                     (if (fn-sym? arg)
                                       (sym-map arg)
                                       (list (sym-map arg) input-sym)))
                                   (when v-arg
                                     [`(map (fn [f#] (f# ~input-sym)) ~v-arg)]))]
    `(fn ~fn-name
       ~@(when (some #{'?} argvec)
           [`(~(->> argvec (remove #{'?}) vec)
              (~fn-name ~@(replace {'? 'identity} argvec)))])
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


(samakify-all (if [pred then else]) [foo (inc [?])])
