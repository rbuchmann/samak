(ns ui.components.node-editor
  (:require [datascript.core    :as d]
            [ui.components      :as ui]
            [reagent.core       :as r]
            [samak.parser       :as p]
            [instaparse.core    :as insta]
            [instaparse.failure :as fail]
            [ui.code-db         :as db]))

(defn add-node [db node]
  (db/parse-tree->db db {:type :node
                         :expression node}))

(defn node-editor [_ db]
  (let [state (r/atom "")]
    (fn [_ db]
      (let [parse-result (p/parse-expression @state)
            error? (insta/failure? parse-result)
            validation-state (when error?
                               "error")]
        [:div
         [ui/col {:md 4}
          [:h1 "Node editor!"]
          [:form {:on-submit #(do
                                (.preventDefault %)
                                (when-not error?
                                  (println "Submitted: " @state "!")
                                  (add-node db parse-result)))}
           [ui/form-group {:validation-state validation-state}
            [ui/autofocused-input state validation-state]
            (when validation-state
              [ui/help-block [:pre (with-out-str (fail/pprint-failure parse-result))]])
            [ui/button {:value "send"
                        :type "submit"} "Add node"]]]]
         [ui/col {:md 4}
          [:pre (pr-str @db)]]]))))
