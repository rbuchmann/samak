(ns samak.conveyor
  (:refer-clojure :exclude [uuid])
  #?@
  (:clj
   [(:require [clojure.core.async :as a :refer [<! >! chan go go-loop close! put!]]
              [clojure.core.async.impl.protocols :as ap]
              [promesa.core :as prom]
              [promesa.protocols :as pt]
              [promesa.exec :as px]
              [samak.metrics :as metrics]
              [samak.trace :as t]
              [samak.helpers :as helpers]
              [samak.transduction-tools :as tt]
              [samak.tools :refer [fail log]]
              [samak.pipes :as pipes])
    (:import java.util.concurrent.ForkJoinPool)]
   :cljs
   [(:require [clojure.core.async :as a :refer [<! >! chan close! put!]]
              [clojure.core.async.impl.protocols :as ap]
              [promesa.core :as prom]
              [promesa.protocols :as pt]
              [promesa.exec :as px]
              [samak.metrics :as metrics]
              [samak.trace :as t]
              [samak.transduction-tools :as tt]
              [samak.helpers :as helpers]
              [samak.tools :refer [fail log]]
              [samak.pipes :as pipes])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(def queue (a/buffer 102400))
(def links (atom {}))

(defn meter [n t]
  (metrics/get-meter "conveyor" n t))

(def meters (atom {}))

(defn get-meter [k t]
  (or (get meters k)
      (let [m (swap! meters #(if (contains? % k) % (assoc % k (meter (name k) t))))]
        (get m k))))

(def MAX_RUNS 10)

(defprotocol Station
  (xf [this])
  (cancel? [this]))

(defrecord Passthrough [uuid xf cancel?]
  pipes/Identified
  (uuid [_] uuid)
  Station
  (xf [_] xf)
  (cancel? [_] cancel?))

(defrecord Sink [uuid xf]
  pipes/Identified
  (uuid [_] uuid)
  Station
  (xf [_] xf)
  (cancel? [_] false))

(defn passthrough [uuid xf cancel? spec]
  (Passthrough. uuid xf cancel?))

(defn station? [s]
  (satisfies? Station s))

(defn sink? [s]
  (instance? Sink s))

(defn getNext [queue]
  (try (ap/remove! queue)
       (catch #?(:clj java.lang.RuntimeException :cljs js/Error) _)))

(declare schedule-next)

(defn wrap [{:keys [::from ::to ::msg] :as call}]
  (fn []
    ;; (println "starting" call)
    (if (and (fn? (cancel? to)) ((cancel? to) msg))
      (do
        (.add (get-meter (str "node-" (pipes/uuid to) "-cancel") :counter) 1)
        (t/trace ::cancel 0 call))
      (try
        (let [target (xf to)
              passthrough (not (sink? to))
              cont (if passthrough (:samak.pipes/content msg) msg)
              id (pipes/uuid to)
              meta-info (:samak.pipes/meta msg)
              before (helpers/now)
              res (target cont)
              duration (helpers/duration before (helpers/now))]
          (when passthrough
            (when (nil? res)
              (throw (ex-info (str "received nil on " call ", with meta: " meta-info) {})))
            (if (satisfies? tt/Streamable res)
              (->> res
                   tt/get-items
                   (map (tt/re-wrap meta-info))
                   (map #(t/trace id duration %))
                   (run! (partial schedule-next to)))
              (->> res
                   (tt/re-wrap meta-info)
                   (t/trace id duration)
                   (schedule-next to))))
          )
        (catch #?(:clj java.lang.RuntimeException :cljs js/Error) ex
          (println "exception running" ex))))))

(defn trigger [c]
  ;; FIXME: LOCK
  (when-let [next (getNext queue)]
    (prom/then (px/run! (wrap next))
               #(when (pos? (dec c))) (trigger (dec c)))))

(defn schedule [from to msg]
  ;; (println "----------")
  ;; (println "count" (count queue))
  ;; (println "----------")
  (let [call {::msg msg ::from from ::to to}]
    (ap/add!* queue call))
  (trigger MAX_RUNS))

(defn schedule-next [station res]
  (when-not (sink? station)
    (let [next (get @links station)]
      (if (empty? next)
        (log "dangling node for " station)
        (run! (fn [target] (if (pipes/pipe? target)
                             (pipes/fire-raw! target res)
                             (schedule station target res)))
              next)))))

(defn link!
  [from to]
  (when (or (nil? from) (nil? to)) (fail (str "cant link nil " from " - " to)))
  (let [t [(pipes/pipe? from) (pipes/pipe? to)]
        res (condp = t
              ;; plain link
              [true true] (pipes/link! from to)
              ;; to is conveyor
              [true false] (do
                             (let [c (chan)]
                               (a/tap (pipes/out-port from) c)
                               (go-loop []
                                 ()
                                 (let [msg (<! c)]
                                   (schedule from to msg)
                                   (recur))))
                             to)
              ;; from is conveyor
              [false true] to
              ;; both conveyor
              [false false] to)]
    (swap! links update from conj to)
    (.add (get-meter :link_counter :counter) 1)
    (println "----------")
    (run! #(println %) @links)
    (println "----------")
    res))

(defn fire!
  [pipe event db-id]
  (let [paket (pipes/make-paket event ::fire)]
    (if (pipes/pipe? pipe)
      (pipes/fire-raw! pipe paket)
      (schedule ::fire pipe paket))
    paket))

(defn fire? [t]
  (or (station? t) (pipes/pipe? t)))

(defn transduction-pipe
  [xf uuid cancel?]
  (passthrough uuid xf cancel? nil))

(defn sink
  [f uuid]
  (Sink. uuid f))
