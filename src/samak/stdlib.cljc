(ns samak.stdlib
  #?@
  (:clj
   [(:require
     [clj-http.client :as http]
     [clojure.core.async :as a :refer [<! put! chan go go-loop]]
     [samak.pipes :as pipes]
     [samak.tools :as tools]
     [clojure.string :as str])]
   :cljs
   [(:require
     [cljs-http.client :as http]
     [cljs.core.async :as a :refer [<! put! chan]]
     [clojure.string :as str]
     [reagent.core :as r]
     [samak.pipes :as pipes]
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

(defn to-handler [v ch]
  (fn
    ([]    (put! ch {:data  v}))
    #_([evt] (put! ch {:data  v
                     :event evt}))))

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

(defn http-call [request res]
  (go (let [req (http/get (:url request))]
        (a/pipeline 1 res (map :body) req))))

(defn http []
  (pipes/async-pipe http-call))

(def pipe-symbols
  (merge
   {'pipes/log   log
    'pipes/debug debug}
   #?(:cljs
      {'pipes/ui ui})))
