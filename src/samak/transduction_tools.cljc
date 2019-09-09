(ns samak.transduction-tools
  (:require [samak.trace :as t]
            [samak.helpers :as helpers]))

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
             before    (helpers/now)
             result    (f content)
             duration  (helpers/duration before (helpers/now))]
         (when (nil? result)
           (throw (str "received nil on " rf ", with meta: " meta-info
                       " - " acc)))
         (if (satisfies? Streamable result)
           (->> result
                get-items
                (map (re-wrap meta-info))
                (map #(t/trace db-id duration %))
                (reduce rf acc))
           (->> result
                (re-wrap meta-info)
                (t/trace db-id duration)
                (rf acc))))))))
