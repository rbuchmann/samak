(ns ui.components.node-viewer
  (:require [ui.components   :as ui]
            [ui.samak.emit   :refer [emit-expression]]
            [datascript.core :as d]))

(defn node-viewer [_ db]
  (let [node-ids (-> (d/q '[:find ?id
                            :where [?id :type :node]] @db)
                     seq
                     flatten
                     vec)
        nodes (d/pull-many @db '[*] node-ids)]

    [ui/col {:md 4}
     [ui/unordered-list
      (map (fn [node]
             [:pre (emit-expression (:expression node))])
           nodes)]]))
