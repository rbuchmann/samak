(ns samak.ui-stdlib
  (:require [expound.alpha      :as expound]
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

(defn events [n]
  (let [c (pipes/pipe-chan ::events nil)
        init (atom true)
        elem (if n (js/document.getElementById (str "samak" n)) (.-body js/document))
        bound (.getBoundingClientRect elem)]
    (set! (.-onresize js/window)
          (fn [e] (do (println "rez: " n)
                      (put-meta! c (let [event (js->clj e :keywordize-keys true)]
                                     {:data :resize
                                      :width (.-clientWidth (.-documentElement js/document))
                                      :height (.-clientHeight (.-documentElement js/document))
                                      :samak.view/target (.-id (.-target event))}
                                     )
                                 ::view)
                      (println "EVS: " n)
                      false)))
    (when @init
      (reset! init false)
      (put-meta! c
                 {:data :resize
                  :width (.-clientWidth (.-documentElement js/document))
                  :height (.-clientHeight (.-documentElement js/document))}
                 ::view))
    (pipes/source c)))

(def content (atom {}))

(defn render-cb
  ""
  [n node]
  (r/render (get @content n) node)
  (swap! content dissoc n))

(defn render
  ""
  [n node x events c]
  (if (not (get @content n))
    (helpers/debounce #(render-cb n node)))
  (swap! content assoc n (if events (transform-element x c) x)))


(defn ui
  ([n]
   (ui n false)) ;;FIXME
  ([n events]
   (let [ui-in (chan (a/sliding-buffer 1))
         ui-out (chan (a/sliding-buffer 1000))
         init (atom true)]
     (go-loop []
       (when-some [i (<! ui-in)]
         (println "ui-in" i)
         (trace/trace ::ui 0 i)
         (let [x (or (:samak.pipes/content i) i)]
           (if true ;; (s/valid? ::hiccup x)
             (when-let [node (js/document.getElementById (str "samak" n))]
               ;; (when (= 1 n))
               (.warn js/console (str "render " n " - ") x)
               (render n node x events ui-out))
             (.warn js/console (str "invalid " n " - " (expound/expound-str ::hiccup x) "for" x))))
         (when @init
           (reset! init false)
           (put-meta! ui-out
                      {:data :resize
                       :width (.-clientWidth (.-documentElement js/document))
                       :height (.-clientHeight (.-documentElement js/document))}
                      ::view))
         (recur)))
     ;; (set! (.-onresize js/window)
     ;;       (fn [e] (do (println "uires")(put-meta! ui-out (let [event (js->clj e :keywordize-keys true)]
     ;;                                       {:data :resize
     ;;                                        :width (.-clientWidth (.-documentElement js/document))
     ;;                                        :height (.-clientHeight (.-documentElement js/document))
     ;;                                        :samak.view/target (.-id (.-target event))}
     ;;                                       )
     ;;                              ::view)
     ;;                   false)))
     (pipes/pipe ui-in ui-out (str "render-" n)))))

(defn translate-coords
  ""
  [bound event]
  [(- (.-pageX event) (.-left bound))
   (- (.-pageY event) (.-top bound))])


(defn mouse [n]
  (let [c (pipes/pipe-chan ::mouse nil)
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

(defn convert-key-event
  [event phase]
  {:phase phase
   :which (.-which event)
   :key (.-key event)
   :ctrl-key (.-ctrlKey event)
   :meta-key (.-metaKey event)
   :shift-key (.-shiftKey event)
   :target (.-id (.-target event))
   :type (.-tagName (.-target event))})


(defn keyboard []
  (let [c (pipes/pipe-chan ::keyboard nil)]
    (set! (.-onkeypress js/document)
          (fn [e] (do (let [event (js->clj e :keywordize-keys true)]
                        (put-meta! c (convert-key-event event :press) ::keyboard)
                        (contains? #{"INPUT" "TEXTAREA"} (.-tagName (.-target event)))))))
    (set! (.-onkeyup js/document)
          (fn [e] (do (let [event (js->clj e :keywordize-keys true)]
                        (put-meta! c (convert-key-event event :up) ::keyboard)
                        (contains? #{"INPUT" "TEXTAREA"} (.-tagName (.-target event)))))))
    (pipes/source c)))

(defn ui-module
  []
  (println "init ui")
  (fn []
    (let [xid 2
          render (ui xid true)
          m (mouse xid)]
      (println "start ui:" m)
      {:sources {:events render
                 :mouse m
                 :keyboard (keyboard)}
       :sinks {:render render}})))


;; Exported symbols

(def ui-symbols
  {'modules/ui     ui-module
   'pipes/ui       ui
   'pipes/events   events
   'pipes/mouse    mouse
   'pipes/keyboard keyboard})
