(ns samak.ui-stdlib
  (:require [samak.layout       :as layout]
            [expound.alpha      :as expound]
            [clojure.string     :as str]
            [clojure.spec.alpha :as s]
            [reagent.core       :as r]
            [samak.pipes        :as pipes]
            [cljs.core.async    :as a :refer [<! put! chan close!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; Validation

(s/def ::hiccup
  (s/cat :tag        keyword?
         :attributes (s/? map?)
         :content    (s/* (s/or :terminal string?
                                :element  ::hiccup))))

;; GUI & Event handling

(defn destructure-element [x]
  (let [[tag options? & children] x]
    (if (map? options?)
      [tag options? children]
      [tag nil (cons options? children)])))

(defn to-clj
  [x]
  (into {} (for [k (.keys js/Object x)] [(keyword k) (aget x k)])))

(defn mouse-button-to-keyword
  ""
  [e]
  (get {0 :primary
        1 :middle
        2 :secondary}
       e
       :unknown))


(defmulti  convert-event #(:type (to-clj %)))
(defmethod convert-event "change" [ev] {:target {:value (.-value (:target (to-clj ev)))}})
(defmethod convert-event "submit" [ev] (do (.preventDefault ev) (to-clj ev)))
(defmethod convert-event "click"  [ev] (let [e (to-clj ev)] {:target {:id (.-id (:target e))}
                                                             :button (mouse-button-to-keyword (:button e))}))
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

(defn ui [n]
  (let [ui-in (chan)
        ui-out (chan)]
    (go-loop []
      (when-some [i (<! ui-in)]
        (let [x (or (:samak.pipes/content i) i)]
          (if (s/valid? ::hiccup x)
            (when-let [node (js/document.getElementById (str "samak" n))]
              (when (not n) (.warn js/console (str "render " n " - " x)))
              (r/render (transform-element x ui-out) node))
            (.warn js/console (str "invalid " n " - " (expound/expound-str ::hiccup x)))))
        (recur)))
    (pipes/pipe ui-in ui-out)))

(defn mouse []
  (let [c (chan)]
    (set! (.-onmousedown (.-body js/document))
          (fn [e] (do (put! c (let [event (js->clj e :keywordize-keys true)]
                               {:samak.mouse/type :down
                                :samak.mouse/button (mouse-button-to-keyword (.-button event))
                                :samak.mouse/page-x (.-pageX event)
                                :samak.mouse/page-y (.-pageY event)
                                :samak.mouse/target (.-id (.-target event))}
                               ))
                     false)))
    (set! (.-onmouseup (.-body js/document))
          (fn [e] (do (put! c (let [event (js->clj e :keywordize-keys true)]
                               {:samak.mouse/type :up
                                :samak.mouse/button (mouse-button-to-keyword (.-button event))
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
    (pipes/source c)))

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
    (pipes/source c)))

;; Graph Layouting

(defn layout-call [request res]
  (let [handler (fn [result] (put! res result) (close! res))]
    (layout/compute-layout request [] handler handler)))

(defn layout []
  (pipes/async-pipe layout-call nil nil))

;; Exported symbols

(def ui-symbols
  {'pipes/ui       ui
   'pipes/mouse    mouse
   'pipes/keyboard keyboard
   'pipes/layout   layout})
