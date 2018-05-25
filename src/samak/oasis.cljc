(ns samak.oasis
  (:require [samak.api    :as api]
            [samak.core   :as core]
            [samak.stdlib :as pipes]))

(defn start
  []
  (let [oasis [;; (api/defexp (api/symbol 'div) (api/fn-call (api/symbol 'pipes/ui) [(api/keyword :div)]))
               (api/defexp (api/symbol 'ui) (api/fn-call (api/symbol 'pipes/ui) nil))
               (api/defexp (api/symbol 'd) (api/fn-call (api/symbol 'pipes/debug) nil))
               (api/defexp (api/symbol 'log) (api/fn-call (api/symbol 'pipes/log) nil))
               (api/defexp (api/symbol 'n) (api/fn-call (api/symbol 'pipes/eval-notify) nil))

               ;; (api/defexp (api/symbol 'm)
               ;;   (api/fn-call  (api/symbol 'foo) (api/symbol 'foo)))
               (api/defexp (api/symbol 'graph) (api/vector [(api/keyword :div) (api/string "eval") (api/symbol 'id)]))
               (api/pipe [(api/symbol 'n) (api/symbol 'graph) (api/symbol 'ui)])

               (api/pipe [(api/symbol 'n) (api/symbol 'log)])
               (api/pipe [(api/symbol 'd) (api/key-fn :map) (api/symbol 'log)])
               (api/pipe [(api/symbol 'ui) (api/symbol 'log)])

               ;; (api/defexp (api/symbol 'input) (api/key-fn :bar))
               ;; (api/pipe [()])

               (api/defexp (api/symbol 'oasis) (api/fn-call (api/symbol 'pipes/debug) nil))
               (api/defexp (api/symbol 'repl) (api/vector [(api/keyword :input) (api/map {(api/keyword :on-change) (api/keyword :change)})]))
               (api/pipe [(api/symbol 'oasis) (api/symbol 'repl) (api/symbol 'ui)])
               (api/pipe [(api/symbol 'oasis) (api/symbol 'repl) (api/symbol 'log)])
               ]]
    (println "oasis" oasis)
    oasis))
