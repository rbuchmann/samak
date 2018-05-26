(ns samak.oasis
  (:require [samak.api    :as api]
            [samak.core   :as core]
            [samak.stdlib :as pipes]))

(defn defncall
  ([sym fn-name]
   (api/defexp (api/symbol sym) (api/fn-call (api/symbol fn-name) nil)))
  ([sym fn-name & args]
   (api/defexp (api/symbol sym) (api/fn-call (api/symbol fn-name) args))))

(defn pipe
  ""
  [& args]
  (api/pipe (map api/symbol args)))


(defn start
  []
  (let [oasis [;; (api/defexp (api/symbol 'div) (api/fn-call (api/symbol 'pipes/ui) [(api/keyword :div)]))
               (defncall 'ui 'pipes/ui)
               (defncall 'd 'pipes/debug)
               (defncall 'log 'pipes/log)
               (defncall 'n 'pipes/eval-notify)

               (api/defexp (api/symbol 'graph)
                 (api/vector [(api/keyword :div)
                              (api/string "eval ")
                              (api/symbol 'id)]))
               (pipe 'n 'graph 'ui)

               (pipe 'n 'log)
               (pipe 'd 'log)
               (pipe 'ui 'log)

               (defncall 'get-event-val '|>
                 (api/key-fn :event)
                 (api/key-fn :target)
                 (api/key-fn :value))

               (api/defexp (api/symbol 'handle-input)
                 (api/vector [(api/keyword :div)
                              (api/symbol 'get-event-val)]))
               (pipe 'ui 'handle-input 'log)

               (api/defexp (api/symbol 'handle-submit)
                 (api/vector [(api/keyword :div)
                              (api/string "submit")]))

               (defncall 'is-change '=
                 (api/keyword :change))

               (defncall 'is-input '|>
                 (api/key-fn :data)
                 (api/symbol 'is-change))

               (defncall 'handle-event '|>
                 (api/fn-call (api/symbol 'if) [(api/symbol 'is-input)
                                                (api/symbol 'handle-input)
                                                (api/symbol 'handle-submit)]))

               (pipe 'ui 'handle-event 'log)

               (defncall 'oasis 'pipes/debug)
               (api/defexp (api/symbol 'repl)
                 (api/vector [(api/keyword :form) (api/map {(api/keyword :on-submit) (api/keyword :submit)})
                              (api/vector [(api/keyword :input) (api/map {(api/keyword :on-change) (api/keyword :change)})])]))
               (pipe 'oasis 'repl 'ui)
               (pipe 'oasis 'repl 'log)
               ]]
    oasis))
