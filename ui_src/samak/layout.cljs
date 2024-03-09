(ns samak.layout
  (:require [samak.pipes              :as pipes]
            [samak.trace              :as trace]
            [samak.transduction-tools :as tt]
            [samak.helpers            :as helpers]
            ["elkjs"                  :as elkjs]
            [cljs.core.async          :as a :refer [<! put! chan close!]])
  (:require-macros [cljs.core :refer [exists?]]))

(defn keywordize-type
  ""
  [c]
  (update c :type #(keyword "caravan" %)))

(defn update-child
  ""
  [c]
  (update c :value (fn [v] (mapv keywordize-type v))))

(def worker_threads (if (exists? js/require) (js/require "worker_threads")))

(defn make-worker
  ""
  [url]
  ;; (println "returning worker: " worker_threads)
  (if worker_threads
    (let [Worker (.-Worker worker_threads)] (Worker. url))
    (js/Worker. url)))

(def elk (atom nil))

(defn init [url]
  (set! elk (elkjs. (clj->js {"workerFactory" make-worker
                              "workerUrl" "/elk-worker.min.js"}))))
;; (def elk (elkjs. (clj->js {"workerFactory" make-worker
;;                             "workerUrl" "/elk-worker.min.js"})))


(defn compute-layout [graph options success error]
  (.log js/console (str "Computing layout... " graph))
  (->  (.layout elk (clj->js graph))
        (.then (fn [ret] (success (let [res (js->clj ret :keywordize-keys true)]
                                    (update res :children #(map update-child (map keywordize-type %)))))))
        (.catch #(error (js->clj % :keywordize-keys true))))
  )


;; Graph Layouting

(defn call-layout
  ""
  [handler data]
  (compute-layout data nil (handler :success) (handler :error)))

(def cache (atom {}))

(defn layout-call [request res]
  (let [meta (:samak.pipes/meta request)
        content (or (:samak.pipes/content request) request)
        before (helpers/now)
        handler (fn [token]
                  (fn [return]
                    (println "layout ret: " return)
                    (let [result (assoc {} token return)
                          re-wrap (tt/re-wrap meta result)]
                      (trace/trace ::layout (helpers/duration before (helpers/now)) re-wrap)
                      (swap! cache assoc content result)
                      (when (= token :error) (println (str "layout error: " return " from " content)))
                      (put! res re-wrap)
                      (close! res))))]
    (trace/trace ::layout 0 request)
    (if-let [e (get @cache content)]
      (do (put! res (tt/re-wrap meta e))
          (close! res))
      (call-layout handler content))))

(defn layout []
  (let [in-chan  (chan (a/sliding-buffer 1))
        out-chan (chan)]
    (a/pipeline-async 1 out-chan layout-call in-chan)
    (pipes/Pipethrough. in-chan (a/mult out-chan) nil nil (helpers/uuid))))

(def layout-symbols
  {'pipes/layout layout})
