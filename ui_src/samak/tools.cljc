(ns samak.tools)

(defn log [& args]
  (let [msg (apply str args)]
    #?(:cljs (.log js/console msg)
       :clj (println msg))))
