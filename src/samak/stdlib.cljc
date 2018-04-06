(ns samak.stdlib
  #?@
   (:clj
    [(:require
      [clj-http.client :as http]
      [clojure.core.async :as a :refer [<! chan go go-loop]]
      [samak.pipes :as pipes]
      [samak.tools :as tools])]
    :cljs
    [(:require
      [cljs-http.client :as http]
      [cljs.core.async :as a :refer [<! chan]]
      [reagent.core :as r]
      [samak.pipes :as pipes]
      [samak.tools :as tools])
     (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(defn from-seq [col]
  (pipes/source (a/to-chan col)))

(defn log [args]
  (let [log-chan (chan)]
    (go-loop []
      (when-let [x (<! log-chan)]
        (tools/log x)
        (recur)))
    (pipes/sink log-chan)))

#?(:cljs (defn ui []
           (let [ui-chan (chan)]
             (go-loop []
               (when-some [x (<! ui-chan)]
                 (when-let [node (js/document.getElementById "generated-app")]
                   (r/render x node))
                 (recur)))
             (pipes/sink ui-chan))))

(defn http-call [request res]
  (go (let [req (http/get (:url request))]
        (a/pipeline 1 res (map :body) req))))

(defn http [args]
  (pipes/async-pipe http-call))

(def pipe-symbols
  {'pipes/from-seq from-seq
   'pipes/log      log})
