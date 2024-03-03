(ns samak.conveyor
  (:refer-clojure :exclude [uuid])
  #?@
  (:clj
   [(:require [clojure.core.async :as a :refer [<! >! chan go go-loop close! put!]]
              [promesa.core :as prom]
              [promesa.protocols :as pt]
              [promesa.exec :as px]
              [promesa.exec.csp.mutable-list :as mlist]
              [samak.trace :as t]
              [samak.helpers :as helpers]
              [samak.transduction-tools :as tt]
              [samak.tools :refer [fail log]]
              [samak.pipes :as pipes])
    (:import java.util.concurrent.ForkJoinPool
             java.util.concurrent.locks.ReentrantLock)]
   :cljs
   [(:require [clojure.core.async :as a :refer [<! >! chan close! put!]]
              [promesa.core :as prom]
              [promesa.protocols :as pt]
              [promesa.exec :as px]
              [promesa.exec.csp.mutable-list :as mlist]
              [samak.trace :as t]
              [samak.transduction-tools :as tt]
              [samak.helpers :as helpers]
              [samak.tools :refer [fail log]]
              [samak.pipes :as pipes])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(defprotocol Buffer
  (size [this])
  (add! [this value])
  (remove! [this])
  (get-buffer [this]))

(defn buffer []
  (let [buf (mlist/create)]
    (reify
      Buffer
      (get-buffer [_] buf)
      (add! [this o] (locking this (mlist/add-first! buf o)) true)
      (remove! [this] (locking this (mlist/remove-last! buf)))
      (size [_] (mlist/size buf)))))

(def queue (buffer))
(def links (atom {}))
(def debug (atom nil))
(def parked (atom {}))
(def lock #?(:clj (Object.) :cljs nil))
(def id (atom "unnamed"))

(def MAX_RUNS 10)

(defn getNext [queue]
    (locking lock
      (try
        (remove! queue)
        ;; will throw on empty list
        (catch #?(:clj java.lang.RuntimeException :cljs js/Error) _))))

(defprotocol Station
  (call-fn [this arg])
  (cancel? [this]))

(defrecord Passthrough [uuid xf cancel?]
  pipes/Identified
  (uuid [_] uuid)
  Station
  (call-fn [_ arg] (log "pass " arg) (apply xf [arg]))
  (cancel? [_] cancel?))

(defrecord Sink [uuid xf]
  pipes/Identified
  (uuid [_] uuid)
  Station
  (call-fn [_ arg] (log "sink " arg) (apply xf [arg]))
  (cancel? [_] false))

(defprotocol Store
  (get-state [this] )
  (set-state [this value]))

(defrecord AtomStore [store]
  Store
  (get-state [_] @store)
  (set-state [_ value] (reset! store value)))

(defprotocol AssignedQueue
  (nextOrUnlock [this] "Return next value or unlock")
  (enqueueLocking [this elem] "Add elem to queue or lock if first element"))

(defrecord Splitter [uuid xf store queue lock init-fn]
  pipes/Identified
  (uuid [_] uuid)
  Station
  (call-fn [_ arg]
    (let [cur (or (get-state store)
                  (apply init-fn [arg]))
          _ (log "split " cur arg)
          res (apply xf [{:state cur :next arg}])]
      (set-state store res)
      res))
  (cancel? [_] false)
  AssignedQueue
  (nextOrUnlock [_]
    (if (pos? (size queue))
      (remove! queue)
      (do (reset! lock false) nil)))
  (enqueueLocking [_ call]
    (if @lock
      (do
        (add! queue call)
        (let [size (size queue)]
          ;; (println uuid " " size)
          false))
      (reset! lock true))))

(defn passthrough [uuid xf cancel? spec]
  (Passthrough. uuid xf cancel?))

(defn station? [s]
  (satisfies? Station s))

(defn sink? [s]
  (instance? Sink s))

(defn splitter? [s]
  (instance? Splitter s))

(declare schedule-next)

(defn call-node [{:keys [::from ::to ::msg] :as call}]
  (if (and (fn? (cancel? to)) ((cancel? to) msg))
    (println "canceled" call)
    (try
      (println "[" @id "]" "run" call)
      (let [passthrough (not (sink? to))
            cont (if passthrough (:samak.pipes/content msg) msg)
            id (pipes/uuid to)
            meta-info (:samak.pipes/meta msg)
            before (helpers/now)
            res (call-fn to cont)
            duration (helpers/duration before (helpers/now))]
        (when passthrough
          (when (nil? res)
            (throw (ex-info (str "received nil on " from to msg ", with meta: " meta-info) {})))
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
        (println "exception running" ex)))))

(defn wrap [{:keys [::from ::to ::msg] :as call}]
  (fn []
    (if-let [d @debug]
      (prom/then (d call)
                 (fn [_]
                   (call-node call)))
      (call-node call))))

(defn trigger [c]
  (when-let [next (getNext queue)]
    (when (< 10 c) (println c "trigger"))
    (px/run! (wrap next))
    (do (when (< 10 c ) (println "c" c)) (when (pos? (dec c)) (trigger (dec c))))))

(defn process-splitter [to call]
  ((wrap call))
  (loop [next (nextOrUnlock to)
         c 1]
    (when next
      ;; (println c "next" (-> next ::msg :samak.pipes/meta :samak.pipes/uuid))
      ((wrap next))
      (recur (nextOrUnlock to) (inc c)))))

(defn schedule [from to msg]
  (println "----------")
  (let [size (size queue)]
    (println "count" size)
    (when (pos? size)
      (println "first" (peek (get-buffer queue))))
    )
  (println "----------")
  (let [call {::msg msg ::from from ::to to}]
    (if (splitter? to)
      (do
        (when (enqueueLocking to call)
          (px/run! #(process-splitter to call))))
      (do
        (add! queue call)
        (trigger MAX_RUNS)))))

(defn schedule-next [station res]
  (when-not (sink? station)
    (let [next (get @links station)]
      (if (empty? next)
        (log "[" @id "] " "dangling node for " station)
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
                                 (let [msg (<! c)]
                                   (schedule from to msg)
                                   (recur))))
                             to)
              ;; from is conveyor
              [false true] to
              ;; both conveyor
              [false false] to)]
    (swap! links update from conj to)
    ;; (println "----------")
    ;; (run! #(println %) @links)
    ;; (println "----------")
    res))

(defn fire!
  [pipe event db-id]
  (let [paket (helpers/make-paket event ::fire)]
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

(defn splitter
  [xf init-fn uuid]
  (Splitter. uuid xf (AtomStore. (atom nil)) (buffer) (atom false) init-fn))
