(ns ui.components.node-viewer
  (:require [ui.components   :as ui]
            [datascript.core :as d]))

(defn node-viewer [_ db]
  (let [node-ids (-> (d/q '[:find ?id
                            :where [?id :type :node]] @db)
                     seq
                     flatten
                     vec)
        _ (println node-ids)
        nodes (d/pull-many @db '[*] node-ids)]

    [ui/col {:md 4}
     [ui/unordered-list
      (map (fn [node]
             [:pre (with-out-str
                     (cljs.pprint/pprint node))])
           nodes)]]))
