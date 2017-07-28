(ns ui.samak.stdlib
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async   :as a :refer [put! chan <! >! timeout close!]]
            [net.cgrand.xforms :as x]))

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

(def ports (juxt in-port out-port))

(defn fire! [pipe event]
  (.log js/console (str "Fired event: " event))
  (let [intake (in-port pipe)]
    (put! intake event)))

(defn link [from transducers to]
  (let [source (out-port from)
        sink (in-port to)
        xf-pipe (ports
                 (if (empty? transducers)
                   (pipe (chan))
                   (transduction-pipe (apply comp transducers))))
        [pipe-in pipe-out] xf-pipe]
    (go
      (<! start-chan)
      (a/pipe source pipe-in false))
    (a/pipe pipe-out sink false)))

(defn start []
  (put! start-chan :go))

(def reductions-tx x/reductions)
