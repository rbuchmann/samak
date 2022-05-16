(ns samak.repl-test
  (:require [samak.repl   :as sut]
            [samak.test-programs :as tests]
            [samak.utils :as utils]
            [promesa.core :as prom]
            [clojure.test :as t :refer [deftest is]]
            [clojure.string :as str]))

(deftest should-parse-groups
  (let [res (sut/group-repl-cmds ["(def in 5)" "!f in 5"])]
    (println res)
    (is (= res 5))))

;; (deftest should-eval-inc-x2
;;   (utils/test-promise (prom/then (prom/let [s# (new java.io.StringWriter)]
;;                                    (prom/do!
;;                                     (prom/create (fn [resolve _]
;;                                                    (binding [*out* s#]
;;                                                      (resolve
;;                                                       (sut/eval-lines tests/tl)))))
;;                                     (prom/delay 1000)
;;                                     (prom/resolved (str s#))))
;;                                  #(is (str/includes? % ":content 7") (str "got:" %)))))

;; (deftest should-eval-local-modules
;;   (utils/test-promise (prom/then (prom/let [s# (new java.io.StringWriter)]
;;                                    (prom/do!
;;                                     (prom/create (fn [resolve _]
;;                                                    (binding [*out* s#]
;;                                                      (resolve
;;                                                       (sut/eval-lines tests/test-local-modules)))))
;;                                     (prom/delay 1000)
;;                                     (prom/resolved (str s#))))
;;                                  #(is (str/includes? % "!!!") (str "got:" %)))))

;; (deftest should-eval-builtin-modules
;;   (utils/test-promise (prom/then (prom/let [s# (new java.io.StringWriter)]
;;                                    (prom/do!
;;                                     (prom/create (fn [resolve _]
;;                                                    (binding [*out* s#]
;;                                                      (resolve
;;                                                       (sut/eval-lines tests/test-builtin-modules)))))
;;                                     (prom/delay 3000)
;;                                     (prom/resolved (str s#))))
;;                                  #(is (str/includes? % ":samak.caravan/pong") (str "got:" %)))))

;; (deftest should-eval-example
;;   (binding [sut/*default-timeout* 0]
;;     (utils/test-promise (prom/then (prom/let [s# (new java.io.StringWriter)]
;;                                    (prom/do!
;;                                     (prom/create (fn [resolve _]
;;                                                    (binding [*out* s#]
;;                                                      (resolve
;;                                                       (sut/eval-lines tests/tl)))))
;;                                     (prom/delay 1000)
;;                                     (prom/resolved (str s#))))
;;                                    #(is (str/includes? % ":content 7") (str "got:" %))))))
