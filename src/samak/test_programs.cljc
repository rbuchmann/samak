(ns samak.test-programs)


(def t0
  ["(def in (-> inc inc))"])

(def t
  ["(def in (-> inc inc))"
   "(def out (pipes/log))"
   "(| in out)"
   "!f in 5"])

(def tm
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in {:foo inc} out)"
   "!f in 5"])

(def tl
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in (-> (inc _) (inc _)) out)"
   "!f in 5"])

(def tw
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in (-> (inc _) (inc _)) out)"])

(def tl2
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in [:div id] out)"
   "!f in 42"])

(def tl3
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in (if (even? _) id ignore) out)"
   "!f in 5"
   "!f in 6"])

(def tl3b
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in (only even?) out)"
   "!f in 5"
   "!f in 6"])

(def tl4
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in (pipes/reductions (-> [:-next :-state] sum) 0) out)"
   "!f in 5"
   "!f in 6"])

(def tl4b
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in (pipes/reductions (-> [:-state :-next] into) {}) out)"
   "!f in 5"
   "!f in 6"])

(def tl5
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in (mapcat (repeat 3)) out)"
   "!f in [5 6]"])

(def tl6
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(def out2 (pipes/log))"
   "(def incinc (-> (inc _) (inc _)))"
   "(| in incinc out2)"
   "(| in incinc out)"
   "(def tl {:source {:main in}
             :tests {:test {:when {\"in\" [1]}
                            :then {\"out\" [(-> (incase (= 3 _) :success))]
                                   \"out2\" [(-> (incase (= 3 _) :success))]}}
                     :test2 {:when {\"in\" [3]}
                             :then {\"out\" [(-> (incase (= 5 _) :success))]}}
                     }})"])

;; TODO Fix tests

(def test-local-modules
  ["(def in (pipes/debug))"
   "(def bar-in (pipes/log))"
   "(defmodule bar {:sinks {:log (pipes/log)}})"
   "(| in ((-> (bar) :-sinks :-log) 42))"
   "(| in ((-> (bar) :-sinks :-log) 42))"
   "!f in \"!!!\""
   ])

(def test-builtin-modules
  ["(def in (pipes/debug))"
   "(def mod (modules/caravan))"
   "(def a (-> mod :-sinks :-actions))"
   "(| in (a 42))"
   "!f in \"!!!\""
   ])

(def test-builtin-modules-test
  ["(def in (pipes/debug))"
   "(def mod (modules/caravan))"
   "(def a (-> mod :-sinks :-actions))"
   "(| in (a 42))"
   "(def bar {:source {:in in} :tests {:test {:when {\"in\" [[]]}
                                    :then {\"a\" [(incase (and (= :div (first _))
                                                                        (= 5 (count _)))
                                                                   :success)]}}}})"
   ;; "!f in \"!!!\""
   ])


(def chuck
  ["(def in (pipes/debug))
   (def ui-in (pipes/ui))
   (def ui-out (pipes/ui))
   (def http-in (pipes/debug))
   (def http-out (pipes/debug))
   (def render-joke [:li (str :-id \": \" :-joke)])
   (def render-ui [:div
                [:h1 \"The grand Chuck Norris Joke fetcher!\"]
                [:h2 \"Enter any joke id and press enter\"]
                [:form {:on-submit :submit}
                 [:input {:on-change :change}]]
                (into [:ul] (map render-joke _))])
   (def joke-input-state (pipes/reductions
                       (if (= (-> :-next :-data) :change)
                         {:event :change
                          :value (-> :-next :-event :-target :-value)}
                         {:event :submit
                          :value (-> :-state :-value)})
                       {:event :change
                        :value \"\"}))

   (| ui-in joke-input-state)
   (def handle-input (if (= :submit :-event)
         {:url (str \"http://api.icndb.com/jokes/\" :-value)}
         ignore))
   (| joke-input-state handle-input http-out)

   (def joke-list (pipes/reductions (conj :-state :-next) []))

   (def handle-http (if (= \"success\" :-type)
              :-value
              {:id -1 :joke \"Failed fetching joke\"}))
   (| http-in handle-http joke-list)
   (| in joke-list)

   (| joke-list render-ui ui-out)
   (def chuck {:source {:main in
                        :ui-in ui-in
                        :http-in http-in}
                :tests {
                        :test-response {:when {\"http-in\" [{:type \"success\" :value {:id 42 :joke \"is on you\"}}]}
                                        :then {\"ui-out\" [(incase (= (nth _ 4) [:ul]) :success)]}}
                        :test-init {:when {\"in\" [[]]}
                                    :then {\"ui-out\" [(incase (and (= :div (first _))
                                                                        (= 5 (count _)))
                                                                   :success)]}}
                        :test-event {:when {\"ui-in\" [{:data :change :event {:target {:value 42}}}
                                                       {:data :submit}]}
                                     :then {\"http-out\" [(incase (and (= \"http://api.icndb.com/jokes/42\" :-url))
                                                                    :success)]}}
}})"])
