(ns samak.protocols
  (:require [samak.tools :as t]))

(defprotocol SamakCallable
  (to-samak-fn [this]))

(defn eval-as-fn [f]
  (cond
    (satisfies? SamakCallable f) (to-samak-fn f)
    (ifn? f) f
    :default (constantly f)))

(defn to-map-fn [m]
  (let [fn-m (t/map-vals eval-as-fn m)]
    (fn [x]
      (->> fn-m
           (map (fn [[k f]] [k (f x)]))
           (into {})))))

(defn to-vector-fn [v]
  (apply juxt (map eval-as-fn v)))

(extend-protocol SamakCallable
  #?(:clj  clojure.lang.PersistentArrayMap
     :cljs cljs.core/PersistentArrayMap)
  (to-samak-fn [this] (to-map-fn this))
  #?(:clj  clojure.lang.PersistentVector
     :cljs cljs.core/PersistentVector)
  (to-samak-fn [this] (to-vector-fn this)))
