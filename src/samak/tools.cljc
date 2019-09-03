(ns samak.tools
  #?(:clj
     (:require [clojure.spec.alpha :as s]
               [clojure.pprint     :refer [pprint]]
               [samak.helpers      :as helper])
     :cljs
     (:require [cljs.spec.alpha :as s]
               [cljs.pprint :refer [pprint]]
               [samak.helpers :as helper])))

(defn log [& args]
  (let [msg (apply str args)]
    #?(:cljs (.log js/console msg)
       :clj (println msg))))

(defn trace
  ""
  [db-id event]
  (log (merge {:samak.trace/id db-id
               :samak.trace/level :trace
               :samak.trace/timestamp (helper/now)}
              event))
  event)


(defn fail [& args]
  (let [msg (->> args (interpose " ") (apply str))]
    (throw
     #?(:clj  (Exception. msg)
        :cljs (js/Error.  msg)))))

(defn pretty [x]
  (with-out-str
    (pprint x)))

(defn xform-spec [spec f]
  (s/conformer
   (fn [val]
     (let [conformed (s/conform spec val)]
       (if (= :clojure.spec.alpha/invalid conformed)
         :clojure.spec.alpha/invalid
         (f conformed))))))

(defn assoc-order [items]
  (into []
        (map-indexed
         (fn [i item]
           (assoc item :order i))
         items)))

(defn qualify-kw
  ([kw]
   (qualify-kw *ns* kw))
  ([ns kw]
   (keyword (str ns) (name kw))))

(defn ordered [items]
  (into []
        (map-indexed
         (fn [i item]
           {:order i
            :samak.nodes/node item})
         items)))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))
