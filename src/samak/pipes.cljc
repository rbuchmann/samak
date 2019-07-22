(ns samak.pipes
  #?
  (:clj
   (:require
    [clojure.core.async :as a :refer [chan put!]]
    [clojure.spec.alpha :as s]
    [com.stuartsierra.dependency :as dep]
    [samak.tools :as t]
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
    [samak.helpers :as help]
    [samak.transduction-tools :as tt])))

;; Pipes and flow control

(defprotocol Pipe
  (in-port [this])
  (out-port [this])
  (in-spec [this])
  (out-spec [this]))

(defn pipe? [p]
  (satisfies? Pipe p))

(defprotocol CleanupRequired
  (clean-up [this]))

(defrecord Sink [ch in-spec]
  Pipe
  (in-port [_] ch)
  (in-spec [_] in-spec)
  (out-port [_] nil)
  (out-spec [_] nil))

(defn sink
  ([ch] (sink ch nil))
  ([ch spec]
   (Sink. ch in-spec)))

(defrecord Source [ch out-spec]
  Pipe
  (in-port [_] nil)
  (in-spec [_] nil)
  (out-port [_] ch)
  (out-spec [_] out-spec))

(defn source
  ([ch] (source ch nil))
  ([ch out-spec]
  (Source. (a/mult ch) out-spec)))

(defrecord Pipethrough [in out in-spec out-spec]
  Pipe
  (in-port [_] in)
  (in-spec [_] in-spec)
  (out-port [_] out)
  (out-spec [_] out-spec))

(defn pipe
  ([ch] (pipe ch nil nil))
  ([ch in-spec out-spec]
   (Pipethrough. ch (a/mult ch) in-spec out-spec))
  ([in out] (pipe in out nil nil))
  ([in out in-spec out-spec]
   (Pipethrough. in (a/mult out) in-spec out-spec)))

;; (defn wrap-xf
;;   ""
;;   [fun]
;;   (fn [xf]
;;     (fn
;;       ([] (xf))
;;       ([result] (xf result))
;;       ([result input]
;;        (println "res: " result)
;;        (println "inp: " input)
;;        (let [inner (map ::content input)
;;              step (map fun)
;;              wrap (map #(into {::baz %}))
;;              appl (xf result (comp inner))]
;;          (println "step: "  res)
;;          res)))))


(defn transduction-pipe [xf]
  (pipe (chan 1 xf)))

(defn map-pipe
  ([f] (map-pipe f "fn"))
  ([f dbg]
   (let [wrap-f (fn [x]
                  (println dbg " x: " x " cont: " (::content x))
                  (let [res (assoc x ::content (f (::content x)))]
                    (println dbg " res: " res " cont: " (::content res))
                    res))]
     (pipe (chan 1 (map wrap-f))))))

(defn async-pipe [xf in-spec out-spec]
  (let [in-chan  (chan)
        out-chan (chan)]
    (a/pipeline-async 1 out-chan xf in-chan)
    (Pipethrough. in-chan (a/mult out-chan) in-spec out-spec)))

(def ports (juxt in-port out-port))

(defn make-meta
  ""
  [specific]
  (merge {::created (help/now)
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
    (t/log (str "Fired event: " event))
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

(defn instrument [f]
  (-> f p/eval-as-fn tt/instrumentation-xf))

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
  [state spec x]
  (when (not (s/valid? spec x))
    (println "spec error in state " state)
    (let [reason (s/explain spec x)]
      (println reason)
      reason))
  x)


(defn checked-pipe
  ""
  [pipe in-spec out-spec]
  (let [in-checked (map-pipe #(check-values "in" in-spec %) "in")
        out-checked (map-pipe #(check-values "out" out-spec %) "out")]
    (link! in-checked pipe)
    (link! pipe out-checked)
    (Pipethrough. (in-port in-checked) (out-port out-checked) in-spec out-spec)))
