(ns samak.ui-stdlib
  (:require [samak.layout       :as layout]
            [expound.alpha      :as expound]
            [clojure.string     :as str]
            [clojure.spec.alpha :as s]
            [reagent.core       :as r]
            [samak.pipes        :as pipes]
            [samak.trace        :as trace]
            [samak.transduction-tools :as tt]
            [samak.helpers :as helpers]
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

(defn put-meta!
  ""
  [ch ev source]
  (put! ch (pipes/make-paket ev source)))


(defn to-handler [v ch]
  (fn
    ([& args]
     (case (count args)
       0 (put-meta! ch {:data  v} ::handler)
       1 (put-meta! ch {:data  v
                        :event (convert-event (first args))}
                    ::handler)
       2 (put-meta! ch {:data  v
                        :event (convert-event (first args))}
                    ::handler)
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

(defn ui [n events]
  (let [ui-in (chan (a/sliding-buffer 1))
        ui-out (chan (a/sliding-buffer 1000))]
    (go-loop []
      (when-some [i (<! ui-in)]
        ;; (trace/trace ::ui 0 i)
        (let [x (or (:samak.pipes/content i) i)]
          (if (or true (s/valid? ::hiccup x))
            (when-let [node (js/document.getElementById (str "samak" n))]
              ;; (when n (.warn js/console (str "render " n " - " x)))
              (r/render (if events (transform-element x ui-out) x) node))
            (.warn js/console (str "invalid " n " - " (expound/expound-str ::hiccup x)))))
        (recur)))
    (pipes/pipe ui-in ui-out)))

(defn translate-coords
  ""
  [bound event]
  [(- (.-pageX event) (.-left bound))
   (- (.-pageY event) (.-top bound))])


(defn mouse [n]
  (let [c (chan)
        elem (if n (js/document.getElementById (str "samak" n)) (.-body js/document))
        bound (.getBoundingClientRect elem)]
    (set! (.-onmousedown elem)
          (fn [e] (do (put-meta! c (let [event (js->clj e :keywordize-keys true)
                                         [x y] (translate-coords bound event)]
                                     {:samak.mouse/type :down
                                      :samak.mouse/button (mouse-button-to-keyword (.-button event))
                                      :samak.mouse/page-x x
                                      :samak.mouse/page-y y
                                      :samak.mouse/target (.-id (.-target event))}
                                     )
                                 ::mouse)
                     false)))
    (set! (.-onmouseup elem)
          (fn [e] (do (put-meta! c (let [event (js->clj e :keywordize-keys true)
                                         [x y] (translate-coords bound event)]
                                     {:samak.mouse/type :up
                                      :samak.mouse/button (mouse-button-to-keyword (.-button event))
                                      :samak.mouse/page-x x
                                      :samak.mouse/page-y y
                                      :samak.mouse/target (.-id (.-target event))
                                      })
                                 ::mouse)
                     false)))
    (set! (.-onmousemove elem)
          (fn [e] (do (put-meta! c (let [event (js->clj e :keywordize-keys true)
                                         [x y] (translate-coords bound event)]
                                     {:samak.mouse/type :move
                                      :samak.mouse/page-x x
                                      :samak.mouse/page-y y
                                      :samak.mouse/target (.-id (.-target event))})
                                 ::mouse)
                      false)))
    (set! (.-onwheel elem)
          (fn [e] (do (put-meta! c (let [event (js->clj e :keywordize-keys true)
                                         [x y] (translate-coords bound event)]
                                     {:samak.mouse/type :wheel
                                      :samak.mouse/page-x x
                                      :samak.mouse/page-y y
                                      :samak.mouse/delta-x (.-deltaX event)
                                      :samak.mouse/delta-y (.-deltaY event)
                                      :samak.mouse/target (.-id (.-target event))})
                                 ::mouse)
                      (println "wheel")
                      false)))
    (pipes/source c)))

(defn keyboard []
  (let [c (chan)]
    (set! (.-onkeypress js/document)
          (fn [e] (do (let [event (js->clj e :keywordize-keys true)]
                        (put-meta! c {:which (.-which event)
                                      :key (.-key event)
                                      :ctrl-key (.-ctrlKey event)
                                      :meta-key (.-metaKey event)
                                      :shift-key (.-shiftKey event)
                                      :target (.-id (.-target event))
                                      :type (.-tagName (.-target event))}
                                   ::keyboard)
                       (contains? #{"INPUT" "TEXTAREA"} (.-tagName (.-target event)))))))
    (pipes/source c)))

;; Graph Layouting

(defn call-layout
  ""
  [handler data]
  (layout/compute-layout data [] (handler :success) (handler :error)))

(def cache (atom {}))

(defn layout-call [request res]
  (let [meta (:samak.pipes/meta request)
        content (or (:samak.pipes/content request) request)
        before (helpers/now)
        handler (fn [token]
                  (fn [return]
                    (let [result (assoc {} token return)]
                      (trace/trace ::layout (helpers/duration before (helpers/now)) result)
                      (swap! cache assoc content result)
                      (put! res (tt/re-wrap meta result))
                      (close! res))))]
    (trace/trace ::layout 0 request)
    (if-let [e (get @cache content)]
      (do (put! res (tt/re-wrap meta e))
          (close! res))
      (call-layout handler content))))

(defn layout []
  (let [in-chan  (chan (a/sliding-buffer 2))
        out-chan (chan)]
    (a/pipeline-async 1 out-chan layout-call in-chan)
    (pipes/Pipethrough. in-chan (a/mult out-chan) nil nil)))

;; Exported symbols

(def ui-symbols
  {'pipes/ui       ui
   'pipes/mouse    mouse
   'pipes/keyboard keyboard
   'pipes/layout   layout})
