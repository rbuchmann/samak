(ns samak.caravan-test
  (:require [samak.caravan        :as sut]
            [samak.api            :as api]
            [samak.stdlib         :as pipes]
            [samak.oasis          :as oasis]
            [samak.core           :as core]
            [samak.runtime        :as rt]
            [samak.test-programs   :as test-programs]
            [samak.code-db        :as db]
            [samak.utils          :as utils]
            [samak.trace          :as trace]
            #?(:clj [clojure.core.async :as a :refer [<! chan go go-loop]]
               :cljs [clojure.core.async :as a :refer [<! chan go go-loop]])
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test   :as t :include-macros true])))

(def all-things-samak
  (api/defexp 'test
    (api/fn-call (api/symbol '->)
                 [(api/map
                   {(api/keyword :test)
                    (api/vector
                     [(api/integer 0)
                      (api/fn-call (api/symbol '=)
                                   [(api/integer 1)])
                      (api/string "foo")
                      (api/float 23.1)
                      (api/key-fn :baz)])})])))

(deftest should-create-cell-list
  (is (= [{:type :caravan/func  :display "func"  :value "->"    :level 1 :counter 1}
            {:type :caravan/table :display "table" :value "###"   :level 2 :counter 2}
              {:type :caravan/kw    :display "kw"    :value ":test" :level 3 :counter 3}
                {:type :caravan/list  :display "list"  :value "---"   :level 4 :counter 4}
                  {:type :caravan/int   :display "int"   :value "0"     :level 5 :counter 5}
                  {:type :caravan/func  :display "func"  :value "="     :level 5 :counter 6}
                    {:type :caravan/int   :display "int"   :value "1"     :level 6 :counter 7}
                  {:type :caravan/str   :display "str"   :value "foo"   :level 5 :counter 8}
                  ;; {:type :caravan/sym   :display "sym"   :value "bar"   :level 5 :counter 9} ;; TODO: unbreak symbol handling
                  {:type :caravan/float :display "float" :value "23.1"  :level 5 :counter 9}
                  {:type :caravan/acc   :display "acc"   :value ":-baz" :level 5 :counter 10}]
         (sut/make-cell-list all-things-samak))))

(deftest shoquld-find-cell-root
  (let [[src parent idx] (sut/find-cell (:samak.nodes/rhs all-things-samak) 0)]
    (is (= :samak.nodes/fn-call (:samak.nodes/type src)))
    (is (= nil parent))
    (is (= 0 idx))))

(deftest should-find-flat-nested-cell
  (let [result (sut/find-cell (:samak.nodes/rhs all-things-samak) 1)
        [src parent idx] result]
    (is (= :samak.nodes/map (:samak.nodes/type src)))
    (is (= :samak.nodes/fn-call (:samak.nodes/type parent)))
    (is (= 0 idx))))

(deftest should-find-nested-cell
  (let [[src parent idx] (sut/find-cell (:samak.nodes/rhs all-things-samak) 2)]
    (is (= :samak.nodes/keyword (:samak.nodes/type (:samak.nodes/mapkey src))))
    (is (= :samak.nodes/map (:samak.nodes/type parent)))
    (is (= 1 idx))))

(deftest should-find-deep-nested-cell
  (let [result (sut/find-cell (:samak.nodes/rhs all-things-samak) 7)
        [src parent idx] result]
    ;; (is (= :foo result))
    (is (= :samak.nodes/string (:samak.nodes/type src)))
    (is (= "foo" (:samak.nodes/value src)))
    (is (= :samak.nodes/vector (:samak.nodes/type parent)))
    (is (= 3 idx))))

#_(deftest should-find-position
  (is (= "foo" (:samak.nodes/value (first (sut/add-cell-internal all-things-samak 8))))))

(def builtins ['-> 'bar '=])

#_(deftest should-add-cell
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))]
    (sut/init db)
    (sut/repl-eval all-things-samak)
    (with-redefs-fn {#'sut/add-node (fn [s f]
                                      (is (= "test" s))
                                      (let [args (get-in f [:samak.nodes/rhs :samak.nodes/arguments])
                                            i (get-in args [1 :samak.nodes/node])]
                                        (is (= 2 (count args)))
                                        (is (= :samak.nodes/integer (:samak.nodes/type i)))))}
      #((sut/add-cell) {:sym "test" :cell 1 :type :integer}))))

#_(deftest should-add-map-cell
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))]
    (sut/init db)
    (sut/repl-eval all-things-samak)
    (with-redefs-fn {#'sut/add-node (fn [s f]
                                      (is (= "test" s))
                                      (let [m (get-in f [:samak.nodes/rhs :samak.nodes/arguments 0 :samak.nodes/node])
                                            assert-map (is (= :samak.nodes/map (:samak.nodes/type m)))]
                                        (is (= 2 (count (:samak.nodes/mapkv-pairs m))))
                                        (is (= :samak.nodes/integer (get-in m [:samak.nodes/mapkv-pairs 1 :samak.nodes/mapvalue :samak.nodes/type])))))}
      #((sut/add-cell) {:sym "test" :cell 2 :type :integer}))))

#_(deftest should-add-map-cell-from-key
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))]
    (sut/init db)
    (sut/repl-eval all-things-samak)
    (with-redefs-fn {#'sut/add-node (fn [s f]
                                      (is (= "test" s))
                                      (let [m (get-in f [:samak.nodes/rhs :samak.nodes/arguments 0 :samak.nodes/node])
                                            assert-map (is (= :samak.nodes/map (:samak.nodes/type m)))]
                                        (is (= 2 (count (:samak.nodes/mapkv-pairs m))))
                                        (is (= :samak.nodes/integer (get-in m [:samak.nodes/mapkv-pairs 1 :samak.nodes/mapvalue :samak.nodes/type])))))}
      #((sut/add-cell) {:sym "test" :cell 3 :type :integer}))))

#_(deftest should-add-map-cell-from-value
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))]
    (sut/init db)
    (sut/repl-eval all-things-samak)
    (with-redefs-fn {#'sut/add-node (fn [s f]
                                      (is (= "test" s))
                                      (let [m (get-in f [:samak.nodes/rhs :samak.nodes/arguments 0 :samak.nodes/node])
                                            assert-map (is (= :samak.nodes/map (:samak.nodes/type m)))]
                                        (is (= 2 (count (:samak.nodes/mapkv-pairs m))))
                                        (is (= :samak.nodes/integer (get-in m [:samak.nodes/mapkv-pairs 1 :samak.nodes/mapvalue :samak.nodes/type])))))}
      #((sut/add-cell) {:sym "test" :cell 4 :type :integer}))))

(deftest should-swap-order
  (is (= [{:order 1 :a 1} {:order 0 :a 2}]
         (sut/change-order [{:order 0 :a 1} {:order 1 :a 2}] 0 1))))

(deftest should-remove-arg
  (is (= [{:order 0 :a 2} {:order 1 :a 3}]
         (sut/remove-arg [{:order 1 :a 1} {:order 0 :a 2} {:order 2 :a 3}] 1))))

#_(deftest should-cut-cell
  (let [db (db/create-empty-db)
        builtin (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))]
    (sut/init db)
    (sut/repl-eval all-things-samak)
    (with-redefs-fn {#'sut/add-node (fn [s f]
                                      (is (= "test" s))
                                      (let [m (get-in f [:samak.nodes/rhs :samak.nodes/arguments 0 :samak.nodes/node])
                                            l (get-in m [:samak.nodes/mapkv-pairs 0 :samak.nodes/mapvalue])
                                            assert-list (is (= :samak.nodes/vector (:samak.nodes/type l)))]
                                        (is (= 5 (count (:samak.nodes/children l))))))}
      #(is (= :done ((sut/cut-cell) {:sym "test" :cell-idx 5}))))))

#_(deftest should-load-network
  (let [syms (merge {'pipes/ui       pipes/debug
                     'pipes/mouse    pipes/debug
                     'pipes/keyboard pipes/debug
                     'pipes/layout   pipes/debug}
                    core/samak-symbols)
        rt (rt/make-runtime syms)
        oasis-rt (reduce rt/eval-expression! rt (oasis/start))]
    ;; (println (str "db: " oasis-rt))
    (sut/init oasis-rt)
    (with-redefs-fn {#'sut/add-pipe (fn [p] ;; (println (str "pipe: " p))
                                      )
                     #'sut/add-node (fn [sym ast] ;; (println (str "pipe: " p))
                                      )}
      #(is (= 2 (count (keys (sut/load-oasis)))))))
  )

(deftest should-run-tests
  (let [syms (merge {'pipes/ui    pipes/debug
                     'pipes/http  pipes/debug}
                    core/samak-symbols)
        c (chan 1)
        rt (rt/make-runtime syms)
        _ (sut/init rt)
        _ (sut/test-net c test-programs/tl6 'tl)]
    (utils/test-async
      (go
        (let [val (<! c)]
          (println (str "\ntraces: "))
          (sut/trace-dump)
          (is (= :success val)))))))


(deftest should-test-chuck
  (let [syms (merge {'pipes/ui    pipes/debug
                     'pipes/http  pipes/debug}
                    core/samak-symbols)
        c (chan 1)
        rt (rt/make-runtime syms)]
    ;; (trace/init-tracer rt {:backend :logging})
    (sut/init rt)
    (sut/test-chuck c)
    (utils/test-async
     (go
       (let [[raw port] (a/alts! [c (a/timeout 30000)])
             val (if (= port c) raw :timeout-overall)]
         (println (str "\ntraces: "))
         (sut/trace-dump)
         (is (= :success val)))))))

(deftest should-test-builtin-modules
  (let [syms core/samak-symbols
        c (chan 1)
        rt (rt/make-runtime syms)]
    ;; (trace/init-tracer rt {:backend :logging})
    (sut/init rt)
    (sut/test-net c test-programs/test-builtin-modules-test 'bar)
    (utils/test-async
     (go
       (let [[raw port] (a/alts! [c (a/timeout 30000)])
             val (if (= port c) raw :timeout-overall)]
         (println (str "\ntraces: "))
         (sut/trace-dump)
         (is (= :success val)))))))

(deftest should-test-local-modules
  (let [syms core/samak-symbols
        c (chan 1)
        rt (rt/make-runtime syms)]
    ;; (trace/init-tracer rt {:backend :logging})
    (sut/init rt)
    (sut/test-net c test-programs/test-local-modules-test 'bar)
    (utils/test-async
     (go
       (let [[raw port] (a/alts! [c (a/timeout 30000)])
             val (if (= port c) raw :timeout-overall)]
         (println (str "\ntraces: "))
         (sut/trace-dump)
         (is (= :success val)))))))

(deftest should-eval-lib
  (let [syms core/samak-symbols
        c (chan 1)
        rt (rt/make-runtime syms)]
    ;; (trace/init-tracer rt {:backend :logging})
    (sut/init rt)
    (sut/persist-net test-programs/test-nested-modules-test)
    (let [res (sut/eval-bundle 'baz)]
      (is (= :foo res)))
  ))
