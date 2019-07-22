(ns samak.stdlib
  #?@
  (:clj
   [(:require
     [clojure.spec.alpha :as s]
     [clj-http.client :as http]
     [clojure.core.async :as a :refer [<! put! chan go go-loop close!]]
     [samak.pipes :as pipes]
     [samak.code-db :as db]
     [samak.tools :as tools]
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
     [samak.code-db :as db]
     [samak.tools :as tools]
     [samak.protocols :as p]
     [net.cgrand.xforms :as x])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))


;; Utility helper

(defn debug
  ([] (pipes/pipe (chan)))
  ([spec] (pipes/checked-pipe (debug) spec spec)))

(defn log-through
  ([]
   (log-through nil))
  ([prefix]
   (pipes/map-pipe
    #(if prefix
       (fn [x] (tools/log prefix x) x)
       (fn [x] (tools/log x) x)))))

(defn log
  ([] (log nil))
  ([prefix]
   (let [log-chan (chan)]
     (go-loop []
       (when-let [x (<! log-chan)]
         (if prefix
           (tools/log prefix x)
           (tools/log x))
         (recur)))
     (pipes/sink log-chan))))

;; Networking

(defn http-call [request res]
  (go
    (let [req (http/get (:url request))]
      (a/pipeline 1 res (map :body) req))))

(defn http []
  (pipes/async-pipe http-call nil nil))


;; DB TODO: Don't think this belongs here

;; (defn db-init [args]
;;   (db/create-empty-db))

;; (defn query-call
;;   [db query]
;;   (fn [input out]
;;     (let [ast (or (db/load-by-id input) :not-found)]
;;       (put! out ast))))

;; (defn db-persist [db args]
;;   (db/parse-tree->db! db args))

;; (defn db-query [db query]
;;   (pipes/async-pipe (query-call db query) nil nil))


;; Runtime

(def notify-chan (chan 1))

(defn notify-source
  [ast]
  (put! notify-chan ast))

(defn eval-notify
  ""
  []
  (let [source (chan 1)]
    (a/pipeline 1 source (map (fn [x] (println "ast in: " x) x)) notify-chan)
    (pipes/source source)))


;; TODO: don't think this belongs here

#_(defn eval-line-call
  ""
  [input]
  (doseq [expression (lp/parse input)]
    (notify-source expression)))

#_(defn eval-line
  ""
  []
  (let [log-chan (chan)]
    (go-loop []
      (when-let [x (<! log-chan)]
        (eval-line-call x)
        (recur)))
    (pipes/sink log-chan)))


;; General purpose

(defn wrap-samak-reducer [f]
  (fn [state nxt]
    (f {:next  nxt
        :state state})))

(defn reductions* [f init]
  (pipes/transduction-pipe (x/reductions (-> f p/eval-as-fn wrap-samak-reducer)
                                         init)))


(def pipe-symbols
  {'pipes/log         log
   'pipes/log-through log-through
   'pipes/debug       debug
   'pipes/http        http
   'pipes/eval-notify eval-notify

   ;; 'pipes/eval-line   eval-line

   'pipes/reductions  reductions*})
