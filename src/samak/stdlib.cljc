(ns samak.stdlib
  #?@
  (:clj
   [(:require
     [clj-http.client :as http]
     [clojure.core.async :as a :refer [<! put! chan go go-loop]]
     [samak.pipes :as pipes]
     [samak.code-db :as db]
     [samak.tools :as tools]
     [clojure.string :as str])]
   :cljs
   [(:require
     [cljs-http.client :as http]
     [cljs.core.async :as a :refer [<! put! chan]]
     [clojure.string :as str]
     [reagent.core :as r]
     [samak.pipes :as pipes]
     [samak.code-db :as db]
     [samak.tools :as tools])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(defn debug []
  (pipes/pipe (chan)))

(defn log []
  (let [log-chan (chan)]
    (go-loop []
      (when-let [x (<! log-chan)]
        (tools/log x)
        (recur)))
    (pipes/sink log-chan)))

(defn destructure-element [x]
  (let [[tag options? & children] x]
    (if (map? options?)
      [tag options? children]
      [tag nil (cons options? children)])))

;; #?(:cljs [(defn conv [a] (do (println "a" (js->clj a)) (js->clj a)))]
;;    :clj [(defn conv [_])])

#?(:clj (defn to-clj [i] i)
   :cljs
   (defn to-clj
     [x]
     (into {} (for [k (.keys js/Object x)] [(keyword k) (aget x k)]))))

(defmulti convert-event #(:type (to-clj %)))
(defmethod convert-event "change" [ev] {:target {:value (.-value (:target (to-clj ev)))}})
(defmethod convert-event "submit" [ev] (do (.preventDefault ev) (to-clj ev)))
(defmethod convert-event nil [ev] (let [ev (to-clj ev)] (do (println "unhandled event: " ev) ev)))
(defmethod convert-event :default [ev] (let [ev (to-clj ev)] (do (println "unhandled event: " ev) ev)))

(defn to-handler [v ch]
  (fn
    ([]    (put! ch {:data  v}))
    ([evt] (put! ch {:data  v
                     :event (convert-event evt)}))))

(defn transform-element [x ch]
  (if (vector? x)
    (let [[tag options? children] (destructure-element x)]
      (into [tag (into {}
                       (for [[k v] options?]
                         [k (if (str/starts-with? (name k) "on-")
                              (to-handler v ch)
                              v)]))]
            (map #(transform-element % ch) children)))
    x))

#?(:cljs
   (defn ui []
     (let [ui-in (chan)
           ui-out (chan)]
       (go-loop []
         (when-some [x (<! ui-in)]
           (when-let [node (js/document.getElementById "generated-app")]
             (r/render (transform-element x ui-out) node))
           (recur)))
       (pipes/pipe ui-in ui-out))))

#?(:clj (defn ui []))

(defn http-call [request res]
  (go
    (let [req (http/get (:url request))]
      (a/pipeline 1 res (map :body) req))))

(defn http []
  (pipes/async-pipe http-call))

(defn db-init [args]
  (db/create-empty-db))

(defn query-call
  [db query]
  (fn [input out]
    (let [ast (or (db/load-ast db input) :not-found)]
      (put! out ast))))

(defn db-persist [args]
  )

(defn db-query [db query]
  (pipes/async-pipe (query-call db query)))

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


(def pipe-symbols
  (merge
   {'pipes/log         log
    'pipes/debug       debug
    'pipes/eval-notify eval-notify}
   #?(:cljs
      {'pipes/ui ui})))
