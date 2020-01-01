(ns samak.zipkin
  #?@
  (:clj
   [(:require
     [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! pipe]]
     [clojure.string :as str]
     [clj-http.client :as http]
     [samak.helpers :as helpers])]
   :cljs
   [(:require
     [clojure.core.async :as a :refer [<! >! chan close! put! pipe]]
     [clojure.string :as str]
     [cljs-http.client :as http]
     [samak.helpers :as helpers])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))



(defn handle
  ""
  [c]
  (go
    (println "resp " (<! c))))


(defn send-traces
  [base-url data]
  (let [url (str base-url "spans")]
    ;; (println "send" data)
    (http/post url {:with-credentials? false
                    :json-params data})))

(defn encode
  [{:keys [:samak.pipes/meta] :as samak-trace}]
  (helpers/to-json
   {:id (:samak.pipes/span meta)
    :traceId (str/replace (str (:samak.pipes/uuid meta)) #"-" "")
    ;; :parentId (:samak.pipes/parent meta)
    :name (:samak.trace/node samak-trace)
    :timestamp (max 1 (* 1000 (helpers/to-epoch (:samak.trace/timestamp samak-trace))))
    :duration (* 1000 (:samak.trace/duration samak-trace))
    :debug true
    :kind "SERVER"
    :localEndpoint {"serviceName" (:samak.trace/runtime samak-trace)}
    ;; :remoteEndpoint nil
    :tags {"runtime" (:samak.trace/runtime samak-trace)
           "source" (:samak.pipes/source meta)
           "content" (helpers/substring (str (:samak.pipes/content samak-trace)) 100)}
    }))


(defn trace
  [tracer samak-trace]
  (if (or (:samak.trace/debug samak-trace)
            (= "1" (first (str (:samak.pipes/uuid (:samak.pipes/meta samak-trace)))))) ;; FIXME poor man's sampling
    (let [zipkin-trace (encode samak-trace)
          t (update tracer :buffer conj zipkin-trace)]
      (if (> 10 (count (:buffer t)))
        t
        (do
          (send-traces (:url (:config tracer)) (:buffer t))
          (assoc t :buffer []))))
    tracer))

(defn init
  [config]
  (println "init zipkin")
  {:trace-fn trace :config config :buffer []})
