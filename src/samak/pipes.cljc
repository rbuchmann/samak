(ns samak.pipes
  #?@
   (:clj
    [(:require
      [clojure.core.async :as a :refer [chan put!]]
      [samak.tools :refer [log]])]
    :cljs
    [(:require [cljs.core.async :as a :refer [chan put!]])
     (:require-macros [samak.tools :refer [log]])]))

;; Pipes and flow control

(def start-chan (a/promise-chan))

(defprotocol Pipe
  (in-port [this])
  (out-port [this]))

(def pipe? (partial satisfies? Pipe))

(defprotocol Disconnectable
  (disconnect [this]))

(defrecord Sink [ch]
  Pipe
  (in-port [_] ch)
  (out-port [_] nil))

(defn sink [ch]
  (Sink. ch))

(defrecord Source [ch]
  Pipe
  (in-port [_] nil)
  (out-port [_] ch))

(defn source [ch]
  (Source. (a/mult ch)))

(defrecord Pipethrough [in out]
  Pipe
  (in-port [_] in)
  (out-port [_] out))

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
  (out-port [_] (out-port b))
  Disconnectable
  (disconnect [_] (a/untap (out-port a) (in-port b))))

(defn composite-pipe [a b]
  (CompositePipe. a b))

(defn link [from to]
  (let [source (out-port from)
        sink (in-port to)]
    (a/tap source sink)
    (composite-pipe from to)))

(defn instrument [f]
  (transduction-pipe (map f)))

(defn start []
  (put! start-chan :go))
