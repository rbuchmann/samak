(ns samak.oasis-test
  (:require [samak.oasis                  :as sut]
            [samak.caravan                :as caravan]
            [samak.repl                   :as repl]
            [samak.nodes                  :as nodes]
            [samak.stdlib                 :as pipes]
            [samak.code-db                :as db]
            [samak.runtime.stores         :as stores]
            [samak.runtime                :as runtime]
            [samak.core                   :as c]
            [samak.utils                  :as utils]
            [samak.trace                  :as trace]
            #?@(:clj [[clojure.test       :as t :refer [is deftest]]
                      [clojure.spec.alpha :as s]
                      [clojure.core.async :as a :refer [<! chan go go-loop]]]
                :cljs [[cljs.test         :as t :refer [is deftest] :include-macros true]
                       [cljs.spec.alpha   :as s] :include-macros true
                       [clojure.core.async :as a :refer [<! chan go go-loop]]])))

;; (def rt (runtime/make-runtime c/samak-symbols))

#_(deftest should-define
  (let [code (repl/eval-multi-exp repl/base-symbols [sut/get-val])
        func (get code 'get-val)]
    (is (< (count repl/base-symbols) (count code)))
    (is (= true (ifn? func)))
    (is (= :bar (apply func [{:samak.nodes/rhs {:samak.nodes/value :bar}}])))))

(deftest should-be-valid-ast
  (let [code (sut/start)]
    (is (= true (every? #(s/valid? :samak.spec/toplevel-exp %) code)))))

;; (deftest should-persist
;;   (let [db (:db rt)]
;;     (is (some? (sut/store db)))
;;     (let [id (db/resolve-name db 'oasis)
;;           oasis (db/load-by-id db id)]
;;       (is (= 'oasis (:samak.nodes/name oasis)))
;;       (is (s/valid? :samak.spec/toplevel-exp oasis)))))

#_(deftest should-persist
  (let [store (:store rt)]
    (is (some? (sut/store store)))
    (let [id    (stores/resolve-name store 'oasis)
          oasis (stores/load-by-id store id)]
      (is (= 'oasis (:samak.nodes/name oasis)))
      (is (s/valid? :samak.spec/toplevel-exp oasis)))))

;; (deftest should-test-oasis
;;   (let [syms (merge c/samak-symbols
;;                     c/ui-mock-symbols
;;                     {'pipes/ui       pipes/debug
;;                      'pipes/http     pipes/debug
;;                      'pipes/mouse    pipes/debug
;;                      'pipes/keyboard pipes/debug
;;                      'pipes/layout   pipes/debug}
;;                     )
;;         c (chan 1)
;;         rt (runtime/make-runtime syms)
;;         state (reduce (fn [s e] (runtime/eval-expression! s e)) rt (sut/start))]
;;     ;; (println (sut/store (:store rt)))
;;     (trace/init-tracer rt {:backend :logging})
;;     (caravan/init state)
;;     (caravan/test-oasis c)
;;     (utils/test-async
;;      (go
;;        (let [[raw port] (a/alts! [c (a/timeout 30000)])
;;              val (if (= port c) raw :timeout-overall)]
;;          (println (str "\ntraces: "))
;;          (caravan/trace-dump)
;;          (is (= :success val)))))))

(deftest should-test-oasis-ui
  (let [syms (merge {'pipes/ui (fn [_] (pipes/debug))}
                    c/samak-symbols)
        c (chan 1)
        rt (runtime/make-runtime syms)
        state (reduce (fn [s e] (runtime/eval-expression! s e)) rt (sut/start-ui))]
    (trace/init-tracer rt {:backend :logging})
    (caravan/init rt)
    (caravan/run-testsuite c 'oasis-ui {:timeout 3000})
    (utils/test-async
     (go
       (let [[raw port] (a/alts! [c (a/timeout 300000)])
             val (if (= port c) raw :timeout-overall)]
         (println (str "\ntraces: "))
         (caravan/trace-dump)
         (is (= :success val)))))))
