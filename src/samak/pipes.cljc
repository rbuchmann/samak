(ns samak.pipes
  #?
  (:clj
   (:require
    [clojure.core.async :as a :refer [chan put!]]
    [clojure.spec.alpha :as s]
    [com.stuartsierra.dependency :as dep]
    [samak.tools :as t]
    [samak.trace :as trace]
    [samak.helpers :as help]
    [samak.protocols :as p]
    [samak.transduction-tools :as tt])
   :cljs
   (:require
    [cljs.core.async :as a :refer [chan put!]]
    [cljs.spec.alpha :as s]
    [com.stuartsierra.dependency :as dep]
    [samak.protocols :as p]
    [samak.tools :as t]
    [samak.trace :as trace]
    [samak.helpers :as help]
    [samak.transduction-tools :as tt])))

;; Pipes and flow control

(defprotocol Pipe
  (db-id [this])
  (in-port [this])
  (out-port [this])
  (in-spec [this])
  (out-spec [this]))

(defn pipe? [p]
  (satisfies? Pipe p))

(defprotocol CleanupRequired
  (clean-up [this]))

(defrecord Sink [ch in-spec db-id]
  Pipe
  (db-id [_] db-id)
  (in-port [_] ch)
  (in-spec [_] in-spec)
  (out-port [_] nil)
  (out-spec [_] nil))

(defn sink
  ([ch db-id] (sink ch nil db-id))
  ([ch spec db-id]
   (Sink. ch in-spec db-id)))

(defrecord Source [ch out-spec db-id]
  Pipe
  (db-id [_] db-id)
  (in-port [_] nil)
  (in-spec [_] nil)
  (out-port [_] ch)
  (out-spec [_] out-spec))

(defn source
  ([ch db-id] (source ch nil db-id))
  ([ch out-spec db-id]
  (Source. (a/mult ch) out-spec db-id)))

(defrecord Pipethrough [in out in-spec out-spec db-id]
  Pipe
  (db-id [_] db-id)
  (in-port [_] in)
  (in-spec [_] in-spec)
  (out-port [_] out)
  (out-spec [_] out-spec))

(defn pipe
  ([ch db-id] (pipe ch nil nil db-id))
  ([ch in-spec out-spec db-id]
   (Pipethrough. ch (a/mult ch) in-spec out-spec db-id))
  ([in out db-id] (pipe in out nil nil db-id))
  ([in out in-spec out-spec db-id]
   (Pipethrough. in (a/mult out) in-spec out-spec db-id)))


(defn transduction-pipe [xf db-id]
  (pipe (chan 1 xf) db-id))

(defn async-pipe [xf in-spec out-spec db-id]
  (let [in-chan  (chan)
        out-chan (chan)]
    (a/pipeline-async 1 out-chan xf in-chan)
    (Pipethrough. in-chan (a/mult out-chan) in-spec out-spec db-id)))

(def ports (juxt in-port out-port))

(defn make-meta
  ""
  [specific]
  (merge {::created (help/now)
          ::span (help/make-span)
          ::uuid (help/uuid)} specific))

(defn make-paket
  ""
  [event source]
  {::meta (make-meta {::source source})
   ::content event})


(defn fire-raw!
  "put a raw event into the given pipe. should be used for testing only."
  [pipe event]
  (put! (in-port pipe) event))


(defn fire! [pipe event]
  (let [paket (make-paket event ::fire)]
    (trace/trace (db-id pipe) 0 paket)
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

(defn link! [from to db-id]
  (let [source (out-port from)
        sink   (in-port to)]
    (a/tap source sink)
    (composite-pipe from to)))

(defn instrument [f]
  (tt/instrumentation-xf (p/eval-as-fn f) trace/*db-id*))

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
    (link! a b ::all)))

(defn check-values
  ""
  [state spec paket]
  (let [x (::content paket)]
  (when (not (s/valid? spec x))
    (println "spec error in state " state)
    (let [reason (s/explain spec x)]
      (println reason)
      reason)))
  paket)

(defn checked-pipe
  ""
  [pipe in-spec out-spec db-id]
  (let [in-checked (transduction-pipe (map #(check-values "in" in-spec %)) db-id)
        out-checked (transduction-pipe (map #(check-values "out" out-spec %)) db-id)]
    (link! in-checked pipe db-id)
    (link! pipe out-checked db-id)
    (Pipethrough. (in-port in-checked) (out-port out-checked) in-spec out-spec db-id)))
