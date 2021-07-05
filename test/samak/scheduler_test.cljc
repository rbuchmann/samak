(ns samak.scheduler-test
  (:require [samak.scheduler :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [clojure.string :as s]
            [promesa.core :as prom]
            [samak.core :as core]
            [samak.lisparser :as p]
            [samak.runtime :as rt]
            [samak.test-programs :as test-programs]
            [samak.utils :as utils]))

(t/deftest should-load-module
  (utils/test-promise
   (prom/then (prom/let [rt (rt/make-runtime core/samak-symbols)
                         parsed (p/parse-all (s/join " " test-programs/tl6))
                         _ (rt/persist-to-ids! (:store rt) (:value parsed))
                         bundle-id (rt/resolve-name rt 'tl)]
                (sut/load-bundle-by-id rt bundle-id))
              #(t/is (= {:id 227,
                         :deps [],
                         :sinks #{200 203},
                         :sources #{197 200 203},
                         :roots {:nodes '(197 200 203 206), :pipes '(219 223)}}
                        %)))))
