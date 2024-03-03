(ns samak.trace
  #?(:clj
     (:require
      [clojure.core.async :as a :refer [chan put! <! go-loop]]
      [clojure.spec.alpha   :as s]
      [clojure.walk         :as w]
      [promesa.core         :as prom]
      [samak.helpers        :as helper]
      [samak.tools          :as tools]
      [samak.api            :as api]
      ;; [samak.runtime.stores :as store]
      )
     :cljs
     (:require
      [cljs.core.async :as a :refer [chan put! <!]]
      [cljs.spec.alpha :as s]
      [clojure.walk :as w]
      [promesa.core :as prom]
      [samak.helpers :as helper]
      [samak.api :as api]
      [samak.tools :as tools])))

(def ^:dynamic *db-id* nil)
(def ^:dynamic *code* nil)

(def rt (atom {}))

(def tracer (atom nil))

(def meters (atom {}))


(defn init-tracer
  ""
  [rt-in config]
  (println "init tracer" config)
  (reset! rt rt-in)
  (when (= :logging (:backend config))
    (let [pre (or (:prefix config) "TRACE -")]
      (reset! tracer {:trace-fn (fn [t x] (println pre x) t)}))))

(defn trace
  ""
  [db-id duration event]
  (if-not (:samak.pipes/uuid (:samak.pipes/meta event))
    (when event
      (println "no traceable event:" event)))
  event)
