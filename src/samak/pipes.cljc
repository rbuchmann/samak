(ns samak.pipes
  (:refer-clojure :exclude [uuid])
  #?@
  (:clj
   [(:require
    [clojure.core.async :as a :refer [chan put! <! go-loop]]
    [clojure.spec.alpha :as s]
    [com.stuartsierra.dependency :as dep]
    [samak.tools :as t]
    [samak.helpers :as help]
    [samak.protocols :as p]
    [samak.transduction-tools :as tt])]
   :cljs
   [(:require
    [cljs.core.async :as a :refer [chan put! <!]]
    [cljs.spec.alpha :as s]
    [com.stuartsierra.dependency :as dep]
    [samak.protocols :as p]
    [samak.tools :as t]
    [samak.helpers :as help]
    [samak.transduction-tools :as tt])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(def buffs (atom {}))
(def pipes (atom {}))

(defn pipe-buf
  ""
  [n size]
  (let [buf (a/buffer size)]
    (swap! buffs assoc n buf)
    buf))

(defn exh
  ""
  [n]
  (fn [e]
    (let [msg (str "exception in pipe" n e)]
      (t/log msg)
    (throw (ex-info msg {} e)))))

(defn pipe-chan
  ""
  [id size]
  (chan (when size (pipe-buf id size)) nil (exh id)))

;; Pipes and flow control

(defprotocol Pipe
  (uuid [this])
  (in-port [this])
  (out-port [this])
  (in-spec [this])
  (out-spec [this]))

(defn pipe? [p]
  (satisfies? Pipe p))

(defprotocol CleanupRequired
  (clean-up [this]))

(defrecord Sink [ch in-spec uuid]
  Pipe
  (uuid [_] uuid)
  (in-port [_] ch)
  (in-spec [_] in-spec)
  (out-port [_] nil)
  (out-spec [_] nil))

(defn sink
  ([ch] (sink ch nil (help/uuid)))
  ([ch spec]
   (Sink. ch in-spec (help/uuid)))
  ([ch spec uuid]
   (Sink. ch in-spec uuid)))

(defrecord Source [ch out-spec uuid]
  Pipe
  (uuid [_] uuid)
  (in-port [_] nil)
  (in-spec [_] nil)
  (out-port [_] ch)
  (out-spec [_] out-spec))

(defn source
  ([ch] (source ch nil))
  ([ch out-spec]
   (Source. (a/mult ch) out-spec (help/uuid)))
  ([ch out-spec uuid]
  (Source. (a/mult ch) out-spec uuid)))

(defrecord Pipethrough [in out in-spec out-spec uuid]
  Pipe
  (uuid [_] uuid)
  (in-port [_] in)
  (in-spec [_] in-spec)
  (out-port [_] out)
  (out-spec [_] out-spec))

(defn pipe
  ([ch] (pipe ch nil nil (help/uuid)))
  ([ch uuid] (pipe ch nil nil uuid))
  ([ch in-spec out-spec uuid] (pipe ch ch in-spec out-spec uuid))
  ([in out uuid] (pipe in out nil nil uuid))
  ([in out in-spec out-spec uuid]
   (let [[old _] (swap-vals! pipes assoc uuid uuid)]
     (if (get old uuid)
       (println "!!!1 mult" uuid)
       (println "!!!2 reg" uuid)))
   (let [m (a/mult out)
         c (pipe-chan ::drop nil)]
     (go-loop []
         (let [msg (<! c)]
           (when msg (t/log "%%% drip pipe [" (or uuid "-") "] " msg))
           (recur)))
     (a/tap m c)
     (Pipethrough. in m in-spec out-spec uuid))))


(defn transduction-pipe
  ([xf] (transduction-pipe xf ::xf))
  ([xf uuid] (pipe (chan 1 xf) uuid);; (pipe (chan (pipe-buf uuid 1) xf (exh uuid)) uuid)
   ))

(defn async-pipe [xf in-spec out-spec]
  (let [in-chan  (chan)
        out-chan (chan)]
    (a/pipeline-async 1 out-chan xf in-chan)
    (Pipethrough. in-chan (a/mult out-chan) in-spec out-spec (help/uuid))))

(def ports (juxt in-port out-port))

(defn make-meta
  ""
  [specific]
  (merge {::created (help/now)
          ::span (help/make-span)
          ::parent (help/make-span)
          ::cancel (help/uuid)
          ::uuid (help/uuid)} specific))

(defn make-paket
  ""
  ([event source]
   {::meta (make-meta {::source source})
    ::content event})
  ([event source uuid]
   {::meta (make-meta {::uuid uuid ::source source})
    ::content event}))


(defn fire-raw!
  "put a raw event into the given pipe. should be used for testing only."
  [pipe event]
  ;; (println "pipe fire" pipe)
  (put! (in-port pipe) event))


(defn fire! [pipe event db-id]
  (let [paket (make-paket event ::fire)]
    (fire-raw! pipe paket)))

(defrecord CompositePipe [a b]
  Pipe
  (in-port [_] (in-port a))
  (out-port [_] (out-port b))
  (in-spec [_] (in-spec a))
  (out-spec [_] (out-spec b))
  CleanupRequired
  (clean-up [_] (a/untap (out-port a) (in-port b))))

(defn composite-pipe [a b]
  (CompositePipe. a b))

(defn link! [from to]
  (let [source (out-port from)
        sink   (in-port to)]
    (a/tap source sink)
    (composite-pipe from to)))

(defn instrument [db-id cancel? f]
  (tt/instrumentation-xf (p/eval-as-fn f) db-id cancel?))

(defn cancel [cancel? f]
  (tt/cancel-xf (p/eval-as-fn f) cancel?))

(defn to-depgraph [edges]
  ; If there is an edge from a->b, then b depends on a
  (reduce (fn [graph [a b]] (dep/depend graph b a)) (dep/graph) edges))

(defn order-links [pipe-pairs]
  (let [g (to-depgraph pipe-pairs)]
    (for [node       (-> g dep/topo-sort reverse)
          dependency (dep/immediate-dependencies g node)]
      [dependency node])))

(defn link-all! [pipe-pairs]
  (doseq [[a b] (order-links pipe-pairs)]
    (link! a b)))

(defn check-values
  ""
  [state spec paket]
  (let [x (::content paket)]
    (if x
      (when (not (s/valid? spec x))
        (let [reason (s/explain spec x)]
          (println "spec error in state" state)
          (println "reason for" x ":" reason)
          reason))
      (println "no content: " paket)))
  paket)

(defn checked-pipe
  ""
  [pipe in-spec out-spec uuid]
  (let [in-checked (transduction-pipe (map #(check-values (str uuid "-in") in-spec %)) (str uuid "-in"))
        out-checked (transduction-pipe (map #(check-values (str uuid "-out") out-spec %)) (str uuid "-out"))]
    (link! in-checked pipe)
    (link! pipe out-checked)
    (Pipethrough. (in-port in-checked) (out-port out-checked) in-spec out-spec uuid)))
