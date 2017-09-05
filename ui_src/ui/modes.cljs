(ns ui.modes
  (:require [ui.tools                  :as tools]
            [ui.components.node-editor :refer [node-editor]]
            [ui.components.node-viewer :refer [node-viewer]]))

(def mode-components
  {:node-editor node-editor
   :node-viewer node-viewer})

(defn transition-to [mode]
  (fn [state]
    (assoc state :mode mode :path [])))

(defn save-node [state]
  (js/console.log "Saved node")
  state)

(def starting-mode :home)

(def modes
  (tools/to-deep-sorted
   {:home
    {:edit
     {:add-node :node-editor}
     :view
     {:view-nodes :node-viewer}}
    :node-editor
    {:save-node save-node
     :abort (transition-to :home)}}))
