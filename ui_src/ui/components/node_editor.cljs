(ns ui.components.node-editor
  (:require [datascript.core   :as d]
            [ui.components     :as ui]
            [reagent.core      :as r]
            [samak.combiparser :as p]
            [samak.code-db     :as db]))

(defn node-editor [_ db]
  (let [state (r/atom "")]
    (fn [_ db]
      (let [parse-result (p/parse @state)
            error? (p/failure? parse-result)
            validation-state (when error?
                               "error")]
        [:div
         [ui/col {:md 4}
          [:h1 "Node editor!"]
          [:form {:on-submit #(do
                                (.preventDefault %)
                                (when-not error?
                                  (println "Submitted: " @state "!")
                                  (println "parsed: " parse-result)
                                  (db/parse-tree->db db (:value parse-result))))}
           [ui/form-group {:validation-state validation-state}
            [ui/autofocused-input state validation-state]
            (when validation-state
              [ui/help-block [:pre (with-out-str (cljs.pprint/pprint parse-result))]])
            [ui/button {:value "send"
                        :type "submit"} "Add node"]]]]
         [ui/col {:md 4}
          [:pre (pr-str @db)]]]))))
