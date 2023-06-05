(ns samak.stdlib
  #?@
  (:clj
   [(:require
     [clojure.spec.alpha :as s]
     [clj-http.client :as http]
     [clojure.core.async :as a :refer [<! put! chan go go-loop close!]]
     [samak.pipes :as pipes]
     [samak.conveyor :as conv]
     [samak.code-db :as db]
     [samak.tools :as tools]
     [samak.transduction-tools :as tt]
     [samak.helpers :as helpers]
     [samak.trace :as trace]
     [clojure.string :as str]
     [net.cgrand.xforms :as x]
     [samak.protocols :as p])]
   :cljs
   [(:require
     [clojure.spec.alpha :as s]
     [cljs-http.client :as http]
     [cljs.core.async :as a :refer [<! put! chan close!]]
     [clojure.string :as str]
     [samak.pipes :as pipes]
     [samak.conveyor :as conv]
     [samak.code-db :as db]
     [samak.tools :as tools]
     [samak.transduction-tools :as tt]
     [samak.helpers :as helpers]
     [samak.trace :as trace]
     [samak.protocols :as p]
     [net.cgrand.xforms :as x])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

;; Utility helper

(defn debug
  ([] (debug (helpers/uuid)))
  ([id] (debug id nil))
  ([id spec]
   (if spec
     (pipes/checked-pipe (debug) spec spec id)
     (pipes/pipe (pipes/pipe-chan id nil) id))))

(defn conveyor
  ([] (conveyor (helpers/uuid)))
  ([id] (conveyor id nil))
  ([id spec]
   (conv/passthrough id identity spec nil)))

(defn log-through
  ([]
   (log-through nil))
  ([prefix]
   (conv/transduction-pipe
    (if prefix
      (fn [x] (tools/log prefix x) x)
      (fn [x] (tools/log x) x))
    (str "logthrough-" prefix)
    nil)))

(def lock 1)
(def log-chan (atom nil))

(defn init [log-c]
  (reset! log-chan log-c))

(defn log
  ([] (log (rand-int 100000)))
  ([prefix]
   (conv/sink
    (fn [l]
      (if @log-chan
        (put! @log-chan {:msg l :source prefix :time (helpers/now)})
        (locking lock (tools/log (str "log " prefix " " l)))))
    (str "log-" prefix))))

;; Networking

(defn http-call [request res]
  (go
    (println (str "http: " request))
    (let [meta (:samak.pipes/meta request)
          req (http/get (:url request))]
      (a/pipeline 1 res  (map #(tt/re-wrap meta (:body %))) req))))

(defn http []
  (pipes/async-pipe http-call nil nil))


;; General purpose

(defn wrap-samak-reducer [f]
  (let [db-id trace/*db-id*]
    (fn [state nxt]
      (let [meta-info-nxt (when (map? nxt)
                            (:samak.pipes/meta nxt))
            content-nxt   (cond-> nxt
                            (some? meta-info-nxt) :samak.pipes/content)
            meta-info-state (when (map? state)
                              (:samak.pipes/meta state))
            content-state   (cond-> state
                              (some? meta-info-state) :samak.pipes/content)
            before    (helpers/now)
            res (f {:next  content-nxt
                    :state content-state})
            duration  (helpers/duration before (helpers/now))
            end (tt/re-wrap meta-info-nxt res)]
        (when-not meta-info-nxt (println "wrapper" res))
        (trace/trace db-id duration end)
        end))))

(defn reductions* [f init]
  (pipes/transduction-pipe
   (x/reductions (-> f p/eval-as-fn wrap-samak-reducer)
                 (tt/re-wrap (helpers/make-meta {:samak.pipes/source ::reductions})
                             init))
   (str "reductions-" (helpers/uuid))))


(def pipe-symbols
  {'pipes/log         log
   'pipes/log-through log-through
   'pipes/debug       debug
   'pipes/station     conveyor
   'pipes/http        http
   'pipes/reductions  reductions*})
