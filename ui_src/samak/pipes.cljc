(ns samak.pipes
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]))
  (:require #?(:cljs
               [cljs.core.async        :as a :refer [put! chan <! >! timeout close!]]
               :clj
               [clojure.core.async     :as a :refer [put! chan <! >! timeout close!
                                                     go go-loop]])
            [samak.stdlib           :as std]
            [samak.tools            :as tools]
            #?(:cljs [cljs-http.client :as http]
               :clj  [clj-http.client  :as http])
            [reagent.core              :as r]))

(defn from-seq [col]
  (std/source (a/to-chan col)))

(defn log []
  (let [log-chan (chan)]
    (go-loop []
      (when-let [x (<! log-chan)]
        (tools/log x)
        (recur)))
    (std/sink log-chan)))

(defn tick []
  (let [tick-chan (chan)
        state (atom 0)]
    (go-loop []
      (when (= @state 0)
        (<! std/start-chan))
      (>! tick-chan @state)
      (swap! state inc)
      (<! (timeout 1000))
      (recur))
    (std/source tick-chan)))

#?(:cljs (defn ui []
           (let [ui-chan (chan)]
             (go-loop []
               (when-some [x (<! ui-chan)]
                 (when-let [node (js/document.getElementById "generated-app")]
                   (r/render x node))
                 (recur)))
             (std/sink ui-chan))))

(defn parse
  [response]
  (:body response))


(defn http-call [request res]
  (go (let [req (http/get (:url request))]
        (a/pipeline 1 res (map parse) req))))

(defn http []
  (std/async-pipe http-call))
