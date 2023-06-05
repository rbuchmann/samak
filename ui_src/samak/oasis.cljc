(ns ^:figwheel-no-load samak.oasis
  #?@
  (:clj
   [(:require
     [clojure.spec.alpha :as s]
     [samak.api :as api]
     [samak.code-db :as db]
     [samak.nodes :as n]
     [samak.pipes :as pipes]
     [samak.runtime.stores :as stores]
     samak.spec
     [samak.stdlib :as std])]
   :cljs
   [(:require
     [cljs.spec.alpha :as s]
     [samak.api :as api]
     [samak.code-db :as db]
     [samak.nodes :as n]
     [samak.pipes :as pipes]
     [samak.runtime.stores :as stores]
     samak.spec
     [samak.stdlib :as std])]))

(defn defncall
  ([sym fn-name]
   (api/defexp sym (api/fn-call (api/symbol fn-name) [])))
  ([sym fn-name & args]
   (api/defexp sym (api/fn-call (api/symbol fn-name) args))))

;; (defn defncall
;;   ([sym fn-name]
;;    (api/defexp sym (api/fn-call (api/symbol fn-name) [])))
;;   ([sym fn-name & args]
;;    (api/defexp sym (api/fn-call (api/symbol fn-name) args))))

(defn defmap
  [sym m]
  (defncall sym '-> (api/map m))
)
(defn defpipe
  ""
  [sym call in-spec out-spec & args]
  (defncall sym call args))

(defn pipe
  ""
  ([in out]
   (api/pipe (api/symbol in) (api/symbol out)))
  ([in x out]
   (api/pipe (api/symbol in)
             (api/symbol x)
             (api/symbol out))))


(def network [])
(def oasis [])

(defn start []
  (into oasis (flatten network)))

(defn store [s]
  (println oasis)
  (stores/persist-tree! s oasis)
  (stores/persist-tree! s (flatten network))
  s)
