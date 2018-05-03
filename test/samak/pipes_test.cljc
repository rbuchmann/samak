(ns samak.pipes-test
  #?@
   (:clj
    [(:require
      [clojure.core.async :as a :refer [<!! chan to-chan]]
      [clojure.test :as t]
      [samak.pipes :as sut])]
    :cljs
    [(:require
      [cljs.test :as t :include-macros true]
      [clojure.core.async :as a :refer [<!! chan to-chan]]
      [samak.pipes :as sut])]))

(defn timeout-<!! [c t]
  (first (a/alts!! [c (a/timeout t)] :priority true)))

(t/deftest basic-linkage
  (let [src (sut/source (to-chan [1 2 3]))
        res (chan)
        snk (sut/sink res)]
    (sut/link! src snk)
    (t/is (= [1 2 3] (timeout-<!! (a/into [] res) 5000)))))

(t/deftest transduction-test
  (let [src (sut/source (to-chan [1 2 3]))
        p   (sut/transduction-pipe (map inc))
        res (chan)
        snk (sut/sink res)]
    (sut/link! p snk)
    (sut/link! src p)
    (t/is (= [2 3 4] (timeout-<!! (a/into [] res) 1000)))))


(t/deftest link-ordering-test
  (let [links [[:a :b]
               [:b :c]
               [:b :d]
               [:c :e]
               [:d :e]
               [:e :f]]
        res   (sut/order-links links)]
    ;;         :c
    ;;        /  \
    ;; :a - :b    :e - :f
    ;;        \  /
    ;;         :d

    ;; Make sure the sink is always linked first, and the source is
    ;; always linked last
    (t/is (= [:e :f] (first res)))
    (t/is (= [:a :b] (last res)))))

(t/deftest dag-test
  (let [inp (chan)
        a (sut/source inp)
        b (sut/transduction-pipe (map #(str "a" %)))
        c (sut/transduction-pipe (map str))
        d (sut/transduction-pipe (map keyword))
        e (sut/transduction-pipe (map identity))
        res (chan)
        f (sut/sink res)]
    (sut/link-all! [[a b]
                    [b c]
                    [b d]
                    [c e]
                    [d e]
                    [e f]])
    (a/onto-chan inp [1])
    (t/is (= #{:a1 "a1"} (set (timeout-<!! (a/into [] res) 5000))))))
