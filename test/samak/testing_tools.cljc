(ns samak.testing-tools
  (:require [samak.combiparser :as cp]
            [blancas.kern.core :as p]))



(def example-program
  "
  foo = inc
  bar = dec
  baz = 5
  input | ((foo . bar) baz)")

(def parsed-example
  (cp/parse example-program))
