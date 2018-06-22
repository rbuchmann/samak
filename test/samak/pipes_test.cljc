(ns samak.pipes-test
  #?@
   (:clj
    [(:require
      [clojure.core.async :as a :refer [<!! chan to-chan]]
      [clojure.spec.alpha :as s]
      [clojure.test :as t]
      [samak.pipes :as sut])]
    :cljs
    [(:require
      [cljs.test :as t :include-macros true]
      [cljs.spec.alpha :as s]
      [clojure.core.async :as a :refer [chan to-chan]]
      [samak.pipes :as sut])]))

#?(:clj (defn timeout-<!! [c t]
          (first (a/alts!! [c (a/timeout t)] :priority true))))

;;TODO: Both flaky, not sure how to best fix this
#_(t/deftest basic-linkage
  (let [src (sut/source (to-chan [1 2 3]))
        res (chan)
        snk (sut/sink res)]
    (sut/link! src snk)
    (t/is (= [1 2 3] (timeout-<!! (a/into [] res) 5000)))))

#_(t/deftest transduction-test
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

#?(:clj (t/deftest dag-test
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
            (t/is (= #{:a1 "a1"} (set (timeout-<!! (a/into [] res) 5000)))))))

(s/def ::in-spec string?)
(s/def ::out-spec (s/coll-of string?))

(t/deftest should-check-pipe
  (let [c (chan 1 (map vector))
        test-c (chan 1)
        p (sut/pipe c)
        in-spec {}
        out-spec {}
        checked (sut/checked-pipe p ::in-spec ::out-spec)]
    (a/tap (sut/out-port checked) test-c)
    (t/is (sut/pipe? checked))
    (sut/fire! checked "foo")
    (t/is (= (vector "foo") (<!! test-c)))))
