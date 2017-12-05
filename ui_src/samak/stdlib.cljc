(ns samak.stdlib
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]))
  (:require #?(:cljs
               [cljs.core.async        :as a :refer [put! chan <! >! timeout close!]]
               :clj
               [clojure.core.async     :as a :refer [put! chan <! >! timeout close!
                                                     go go-loop]])
            [samak.stdlib              :as std]
            [samak.tools               :as tools]
            #?(:cljs [cljs-http.client :as http]
               :clj  [clj-http.client  :as http])
            [reagent.core              :as r]))

(defn from-seq [col]
  (std/source (a/to-chan col)))

(defn log [args]
  (let [log-chan (chan)]
    (go-loop []
      (when-let [x (<! log-chan)]
        (tools/log x)
        (recur)))
    (std/sink log-chan)))

#?(:cljs (defn ui []
           (let [ui-chan (chan)]
             (go-loop []
               (when-some [x (<! ui-chan)]
                 (when-let [node (js/document.getElementById "generated-app")]
                   (r/render x node))
                 (recur)))
             (std/sink ui-chan))))


(defn http-call [request res]
  (go (let [req (http/get (:url request))]
        (a/pipeline 1 res (map :body) req))))

(defn http [args]
  (std/async-pipe http-call))
