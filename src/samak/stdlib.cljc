(ns samak.stdlib
  #?@
  (:clj
   [(:require
     [clj-http.client :as http]
     [clojure.core.async :as a :refer [<! put! chan go go-loop close!]]
     [samak.pipes :as pipes]
     [samak.code-db :as db]
     [samak.layout :as layout]
     [samak.tools :as tools]
     [clojure.string :as str]
     [net.cgrand.xforms :as x]
     [samak.protocols :as p])]
   :cljs
   [(:require
     [cljs-http.client :as http]
     [cljs.core.async :as a :refer [<! put! chan close!]]
     [clojure.string :as str]
     [reagent.core :as r]
     [samak.pipes :as pipes]
     [samak.code-db :as db]
     [samak.layout :as layout]
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
   (pipes/transduction-pipe
    (map (if prefix
           (fn [x] (tools/log prefix x) x)
           (fn [x] (tools/log x) x))))))

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


;; GUI & Event handling

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
(defmethod convert-event "click"  [ev] (do (println (str ev)) {:target {:id (.-id (:target (to-clj ev)))}}))
(defmethod convert-event nil [ev] (let [ev (to-clj ev)] (do (println "unhandled event: " ev) ev)))
(defmethod convert-event :default [ev] (let [ev (to-clj ev)] (do (println "unhandled event: " ev) ev)))

(defn to-handler [v ch]
  (fn
    ([& args]
     (case (count args)
       0 (put! ch {:data  v})
       1 (put! ch {:data  v
                   :event (convert-event (first args))})
       2 (put! ch {:data  v
                   :event (convert-event (first args))})
       (println (str "unknown event" args))))
    ;; ([]    (put! ch {:data  v}))
    ;; ([evt] (put! ch {:data  v
    ;;                  :event (convert-event evt)}))
    ))

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

#?(:cljs
   (defn mouse []
     (let [c (chan)]
       (set! (.-onmousedown (.-body js/document))
             (fn [e] (do (put! c (let [event (js->clj e :keywordize-keys true)]
                                   {:samak.mouse/type :down
                                    :samak.mouse/button (.-button event)
                                    :samak.mouse/page-x (.-pageX event)
                                    :samak.mouse/page-y (.-pageY event)
                                    :samak.mouse/target (.-id (.-target event))}
                                   ))
                         false)))
       (set! (.-onmouseup (.-body js/document))
             (fn [e] (do (put! c (let [event (js->clj e :keywordize-keys true)]
                                   {:samak.mouse/type :up
                                    :samak.mouse/button (.-button event)
                                    :samak.mouse/page-x (.-pageX event)
                                    :samak.mouse/page-y (.-pageY event)
                                    :samak.mouse/target (.-id (.-target event))
                                   }))
                         false)))
       (set! (.-onmousemove (.-body js/document))
             (fn [e] (do (put! c (let [event (js->clj e :keywordize-keys true)]
                                   {:samak.mouse/type :move
                                    :samak.mouse/page-x (.-pageX event)
                                    :samak.mouse/page-y (.-pageY event)
                                    :samak.mouse/target (.-id (.-target event))}))
                         false)))
       (pipes/source c))))

#?(:clj (defn mouse []))

#?(:cljs
   (defn keyboard []
     (let [c (chan)]
       (set! (.-onkeypress js/document)
             (fn [e] (do (let [event (js->clj e :keywordize-keys true)]
                           (put! c {:which (.-which event)
                                    :key (.-key event)
                                    :ctrl-key (.-ctrlKey event)
                                    :meta-key (.-metaKey event)
                                    :shift-key (.-shiftKey event)
                                    :target (.-id (.-target event))
                                    :type (.-tagName (.-target event))})
                           (contains? #{"INPUT" "TEXTAREA"} (.-tagName (.-target event)))))))
       (pipes/source c))))

#?(:clj (defn keyboard []))

;; Networking

(defn http-call [request res]
  (go
    (let [req (http/get (:url request))]
      (a/pipeline 1 res (map :body) req))))

(defn http []
  (pipes/async-pipe http-call nil nil))


;; DB

(defn db-init [args]
  (db/create-empty-db))

(defn query-call
  [db query]
  (fn [input out]
    (let [ast (or (db/load-ast db input) :not-found)]
      (put! out ast))))

(defn db-persist [db args]
  (db/parse-tree->db! db args))

(defn db-query [db query]
  (pipes/async-pipe (query-call db query) nil nil))


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


;; Graph Layouting

 (defn layout-call [request res]
   (let [handler (fn [result] (put! res result) (close! res))]
     (layout/compute-layout request [] handler handler)))

(defn layout []
  (pipes/async-pipe layout-call nil nil))


;; General purpose

(defn wrap-samak-reducer [f]
  (fn [state nxt]
    (f {:next  nxt
        :state state})))

(defn reductions* [f init]
  (pipes/transduction-pipe (x/reductions (-> f p/eval-as-fn wrap-samak-reducer)
                                         init)))


(def pipe-symbols
  (merge
   {'pipes/log         log
    'pipes/log-through log-through
    'pipes/debug       debug
    'pipes/http        http
    'pipes/eval-notify eval-notify
    'pipes/ui          ui
    ;; 'pipes/eval-line   eval-line
    'pipes/mouse       mouse
    'pipes/keyboard    keyboard
    'pipes/layout      layout
    'pipes/reductions  reductions*}))
