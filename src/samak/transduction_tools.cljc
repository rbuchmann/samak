(ns samak.transduction-tools)

(defprotocol Streamable
  (get-items [this]))

(defn many [col]
  (reify Streamable
    (get-items [_] col)))

(defn ignore [_]
  (reify Streamable
    (get-items [_] [])))

(defn wrap-streamable [xf]
  (completing (fn [acc nxt]
                (if (nil? nxt) (throw (str "received nil on " xf " - " acc)))
                (if (satisfies? Streamable nxt)
                  (reduce xf acc (get-items nxt))
                  (xf acc nxt)))))

(defn instrumentation-xf [xf]
  (comp xf wrap-streamable))
