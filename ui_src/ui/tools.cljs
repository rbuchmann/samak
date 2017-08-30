(ns ui.tools)

(defn to-deep-sorted [m]
  (into (sorted-map)
        (for [[k v] m]
          [k (if (map? v)
               (to-deep-sorted v)
               v)])))
