(ns samak.oasis-test
  (:require [samak.oasis                  :as sut]
            [samak.repl                   :as repl]
            [samak.nodes                  :as nodes]
            [samak.code-db                :as db]
            [samak.runtime.stores         :as stores]
            [samak.runtime                :as runtime]
            [samak.core                   :as c]
            #?@(:clj [[clojure.test       :as t :refer [is deftest]]
                      [clojure.spec.alpha :as s]]
                :cljs [[cljs.test         :as t :refer [is deftest] :include-macros true]
                       [cljs.spec.alpha   :as s] :include-macros true])))

(def rt (runtime/make-runtime (keys c/samak-symbols)))

#_(deftest should-be-oasis
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

;; (deftest should-persist
;;   (let [db (:db rt)]
;;     (is (some? (sut/store db)))
;;     (let [id (db/resolve-name db 'oasis)
;;           oasis (db/load-by-id db id)]
;;       (is (= 'oasis (:samak.nodes/name oasis)))
;;       (is (s/valid? :samak.spec/toplevel-exp oasis)))))

(deftest should-persist
  (let [store (:store rt)]
    (is (some? (sut/store store)))
    (let [id (.resolve-name store 'oasis)
          oasis (.load-by-id store id)]
      (is (= 'oasis (:samak.nodes/name oasis)))
      (is (s/valid? :samak.spec/toplevel-exp oasis)))))
