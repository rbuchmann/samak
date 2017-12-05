(ns samak.transduction-tools)

(defprotocol Streamable
  (get-items [this]))

(defn many [col]
  (reify Streamable
    (get-items [_] col)))

(defn ignore [_]
  (reify Streamable
    (get-items [_] [])))

(defn instrumentation-xf [xf]
  (fn
    ([] (xf))
    ([a] (xf a))
    ([acc nxt]
     (if (satisfies? Streamable nxt)
       (reduce xf acc (get-items nxt))
       (xf acc nxt)))))

;; filter p = p ? id : ignore
