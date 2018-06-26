(ns samak.testing-tools
  (:require [samak.lisparser :as lp]))

(def example-program
  "(def foo inc)
  (def bar dec)
  (def baz 5)
  (def input (pipes/debug))
  (| input (|> foo bar) baz)")

(def parsed-example
  (lp/parse-all example-program))
