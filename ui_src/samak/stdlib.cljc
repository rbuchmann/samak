(ns samak.stdlib
  #?@(:clj
      [(:require [clojure.core.async :as a :refer [<! chan go put!]]
                 [samak.tools :refer [log]])]
      :cljs
      [(:require [cljs.core.async :as a :refer [<! chan put!]])
       (:require-macros
        [cljs.core.async.macros :refer [go]]
        [samak.tools :refer [log]])]))

;; Helpers

(defn apply-fn [f x]
  (if (or (map?     f)
          (vector?  f)
          (string?  f)
          (number?  f)
          (boolean? f)
          (keyword? f))
    f
    (f x)))

(defn map->fn [m]
  (fn [x]
    (into {}
          (for [[k f] m]
            [k (apply-fn f x)]))))

(defn vec->fn [v]
  (fn [x]
    (mapv (fn [f] (apply-fn f x)) v)))

;; Pipes and flow control

(def start-chan (a/promise-chan))

(defn new-tap [mult-ch]
  (let [ch (chan)]
    (a/tap mult-ch ch)
    ch))

(defprotocol Pipe
  (in-port [this])
  (out-port [this]))

(defrecord Sink [ch]
  Pipe
  (in-port [_] ch)
  (out-port [_] nil))

(defn sink [ch]
  (Sink. ch))

(defrecord Source [ch]
  Pipe
  (in-port [_] nil)
  (out-port [_] (new-tap ch)))

(defn source [ch]
  (Source. (a/mult ch)))

(defrecord Pipethrough [in out]
  Pipe
  (in-port [_] in)
  (out-port [_] (new-tap out)))

(defn pipe [ch]
  (Pipethrough. ch (a/mult ch)))

(defn transduction-pipe [xf]
  (pipe (chan 1 xf)))

(defn async-pipe [xf]
  (let [in-chan (chan)
        out-chan (chan)]
    (a/pipeline-async 1 out-chan xf in-chan)
    (Pipethrough. in-chan (a/mult out-chan))))

(def ports (juxt in-port out-port))

(defn fire! [pipe event]
  (log (str "Fired event: " event))
  (let [intake (in-port pipe)]
    (put! intake event)))

(defrecord CompositePipe [a b]
  Pipe
  (in-port [_] (in-port a))
  (out-port [_] (out-port b)))

(defn composite-pipe [a b]
  (CompositePipe. a b))

(defn link [from to]
  (let [source (out-port from)
        sink (in-port to)]
    (go
      (<! start-chan)
      (a/pipe source sink false))
    (composite-pipe from to)))

(defn start []
  (put! start-chan :go))
