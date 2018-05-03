(ns samak.lisparser
  (:require #?(:clj [clojure.edn  :as edn]
               :cljs [cljs.reader :as edn])
            [samak.api            :as api]))

(declare form->ast)

(defn list->ast [l]
  (if-let [[f & args] (not-empty l)]
    (case f
      def (apply api/defexp (map form->ast args))
      |>  (api/compose (map form->ast args))
      |   (api/pipe (map form->ast args))
      ;; TODO: Make this accept multiple args again
      (api/fn-call (form->ast f) (form->ast (first args))))
    nil))

(defn form->ast [s]
  (condp (fn [pred item] (pred item)) s
    keyword? (api/keyword s)
    string?  (api/string s)
    symbol?  (api/symbol s)
    integer? (api/integer s)
    float?   (api/float s)
    vector?  (api/vector (map form->ast s))
    map?     (api/map (map #(mapv form->ast %) s))
    list?    (list->ast s)))

(defn parse [s]
  (try
    (let [sexp (edn/read-string s)]
      {:value (form->ast sexp)})
    #?(:cljs (catch js/Error e
               {:error (.-message e)})
       :clj  (catch Exception e
               {:error (.getMessage e)}))))

(defn parse-all [s]
  (try
    (let [sexps (edn/read-string (str "[" s "]"))]
      {:value (mapv form->ast sexps)})
    #?(:cljs (catch js/Error e
               {:error (.-message e)})
       :clj  (catch Exception e
               {:error (.getMessage e)}))))
