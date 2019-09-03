(ns samak.transduction-tools
  (:require [samak.tools :as t]))

(defprotocol Streamable
  (get-items [this]))

(defn many [col]
  (reify Streamable
    (get-items [_] col)))

(defn ignore [_]
  (reify Streamable
    (get-items [_] [])))

(defn re-wrap
  ([meta-info]
   (partial re-wrap meta-info))
  ([meta-info content]
   (if (some? meta-info)
     {:samak.pipes/meta    meta-info
      :samak.pipes/content content}
     content)))

(defn instrumentation-xf [f db-id]
  (fn [rf]
    (completing
     (fn [acc nxt]
       (let [meta-info (when (map? nxt)
                         (:samak.pipes/meta nxt))
             content   (cond-> nxt
                         (some? meta-info) :samak.pipes/content)
             result    (f content)]
         (when (nil? result)
           (throw (str "received nil on " rf ", with meta: " meta-info
                       " - " acc)))
         (if (satisfies? Streamable result)
           (->> result
                get-items
                (map (re-wrap meta-info))
                (map #(t/trace db-id %))
                (reduce rf acc))
           (->> result
                (re-wrap meta-info)
                (t/trace db-id)
                (rf acc))))))))
