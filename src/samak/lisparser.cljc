(ns samak.lisparser
  (:require #?(:clj  [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            [samak.api            :as api]
            [clojure.string       :as str]))

(declare form->ast)

(defn list->ast [l]
  (when-let [[f & args] (not-empty l)]
    (case f
      def       (let [[name-sym rhs] args] (api/defexp name-sym (form->ast rhs)))
      defmodule (let [[name-sym rhs] args] (api/defmodule name-sym (form->ast rhs)))
      |         (apply api/pipe (map form->ast args))
      (api/fn-call (form->ast f) (map form->ast args)))))

(defn key-fn-or-keyword [k]
  (if (str/starts-with? (name k) "-")
    (api/key-fn (-> k name (.substring 1) keyword))
    (api/keyword k)))

(defn form->ast [s]
  (condp (fn [pred item] (pred item)) s
    keyword? (key-fn-or-keyword s)
    string?  (api/string s)
    symbol?  (api/symbol s)
    integer? (api/integer s)
    float?   (api/float s)
    vector?  (api/vector (map form->ast s))
    map?     (api/map (map #(mapv form->ast %) s))
    list?    (list->ast s)
    (throw (str "unexpected form: " s))))

(defn parse-form [form]
  {:value (form->ast form)})

(defn parse-all [s]
  (try
    (let [sexps (edn/read-string (str "[" s "]"))]
      {:value (mapv form->ast sexps)})
    #?(:cljs (catch js/Error e
               {:error (str "Error parsing: " s ", error was:" (.-message e))})
       :clj  (catch Exception e
               {:error (str "Error parsing: " s ", error was:" (.-getMessage e))}))))
