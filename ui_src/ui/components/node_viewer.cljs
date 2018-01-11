(ns ui.components.node-viewer
  (:require [ui.components   :as ui]
            [samak.emit      :refer [emit]]
            [datascript.core :as d]))

(defn node-viewer [_ db]
  (let [node-ids (-> (d/q '[:find ?id
                            :where [?id :samak.nodes/type :samak.nodes/def]] @db)
                     seq
                     flatten
                     vec)
        nodes (d/pull-many @db '[*] node-ids)]
    [ui/col {:md 8}
     [ui/unordered-list
      (map (fn [node]
             [:pre (emit node)])
           nodes)]]))
