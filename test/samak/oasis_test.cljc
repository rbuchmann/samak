(ns samak.oasis-test
  (:require [samak.oasis                  :as sut]
            [samak.repl                   :as repl]
            [samak.nodes                  :as nodes]
            [samak.code-db                :as db]
            #?@(:clj [[clojure.test       :as t :refer [is deftest]]
                      [clojure.spec.alpha :as s]]
                :cljs [[cljs.test         :as t :refer [is deftest] :include-macros true]
                       [cljs.spec.alpha   :as s] :include-macros true])))

(def tdb (db/create-empty-db))

(deftest should-be-oasis
  (is (some #(and (= (:samak.nodes/type %) :samak.nodes/def)
                  (= (:samak.nodes/name %) 'oasis))
            (sut/start))))


#_(deftest should-define
  (let [code (repl/eval-multi-exp repl/base-symbols [sut/get-val])
        func (get code 'get-val)]
    (is (< (count repl/base-symbols) (count code)))
    (is (= true (ifn? func)))
    (is (= :bar (apply func [{:samak.nodes/rhs {:samak.nodes/value :bar}}])))))

(deftest should-be-valid-ast
  (let [code (sut/start)]
    (is (= true (every? #(s/valid? :samak.spec/toplevel-exp %) code)))))


#_(deftest should-persist
  (is (some? (sut/persist tdb)))
  (let [ui (db/load-ast tdb 'ui)]
    (is (= 'ui (:samak.nodes/name ui)))
    (is (s/valid? :samak.spec/toplevel-exp ui))))
