(ns samak.test-programs)


(def t0
  ["(def in (-> (inc _) (inc _)))"])

(def t
  ["(def in (-> (inc _) (inc _)))"
   "(def out (pipes/log))"
   "(| in out)"
   "!f in 5"])

(def tm
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in {:foo (inc _)} out)"
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
   "(| in [:div _] out)"
   "!f in 42"])

(def tl3
  ["(def in (pipes/debug))"
   "(def out (pipes/log))"
   "(| in (if (even? _) _ ignore) out)"
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

(def test-local-modules
  [
   "(def in (pipes/debug))"
   "(defmodule bar {:sinks {:log (pipes/log)}})"
   "(def bar-mod (bar))"
   "(| in ((-> bar-mod :-sinks :-log) 42))"
   "!f in \"!!!\""
   ])

(def test-builtin-modules
  ["(def in (pipes/debug))"
   "(def mod (modules/caravan))"
   "(def a (-> mod :-sinks :-actions))"
   "(| in (a 42))"
   "!f in \"!!!\""
   ])

(def test-local-modules-test
  [
   "(def in (pipes/debug))"
   "(def out (pipes/debug))"
   "(| in (inc _) out)"
   "(defmodule bar {:sources {:in in}
                    :sinks {:out out}
                    :tests {:t1 {:when {\"in\" [1]}
                                 :then {\"out\" [(incase 2 :success)]}}}
   })"
   ])

(def test-builtin-modules-test
  ["(def in (pipes/debug))"
   "(def out (pipes/debug))"
   "(def mod ((modules/caravan) 42))"
   "(def a ((-> mod :-sinks :-actions) 42))"
   "(def b ((-> mod :-sources :-commands) 42))"
   "(| in a)"
   "(| b out)"
   "(def bar {:sources {:in in :fake b}
              :tests {:test {:when {\"in\" [{:ping :me}]}
                             :then {\"out\" [(incase (:-pong _) :success)]}}}})"
   ;; "!f in \"!!!\""
   ])


(def test-nested-modules-test
  ["(def in (pipes/debug))"
   "(def out (pipes/debug))"
   "(def mod ((modules/caravan) 42))"
   "(def a ((-> mod :-sinks :-actions) 42))"
   "(def b ((-> mod :-sources :-commands) 42))"
   "(| in a)"
   "(| b out)"
   "(defmodule bar {:depends {:caravan modules/caravan}
                    :sources {:in in :b b :mod mod}
                    :sinks {:out out}
                    :tests {:t1 {:when {\"in\" [1]}
                                 :then {\"out\" [(incase 2 :success)]}}}})"
   "(def s (pipes/debug))"
   "(def t (pipes/debug))"
   "(| s t)"
   "(defmodule quux {:sources {:s s}
                     :sinks {:t t}
                     :tests {:t1 {:when {\"s\" [1]}
                                  :then {\"t\" [(incase 1 :success)]}}}})"
   "(def barmod (bar))"
   "(def quuxmod (quux))"
   "(def x (pipes/debug))"
   "(def quuxin ((-> barmod :-sources :-in) 42))"
   "(def quuxout ((-> barmod :-sources :-in) 42))"
   "(def barin ((-> barmod :-sources :-in) 42))"
   "(def barout ((-> barmod :-sinks :-out) 42))"
   "(| x quuxin)"
   "(defmodule baz {:depends {:bar bar :quux quux}
                    :sources {:x x :barmod barmod}
                    :tests {:t1 {:when {\"in\" [1]}
                                 :then {\"out\" [(incase 2 :success)]}}}})"
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
   (defmodule chuck {:sources {:main in
                         :ui-in ui-in
                         :http-in http-in}
                :tests {
                        :test-response {:when {\"http-in\" [{:type \"success\" :value {:id 42 :joke \"is on you\"}}]}
                                        :then {\"ui-out\" [(incase (= (nth _ 4) [:ul]) :success)
]}}
                        :test-init {:when {\"in\" [[]]}
                                    :then {\"ui-out\" [(incase (and (= :div (first _))
                                                                        (= 5 (count _)))
                                                                   :success)]}}
                        :test-event {:when {\"ui-in\" [{:data :change :event {:target {:value 42}}}
                                                       {:data :submit}]}
                                     :then {\"http-out\" [(incase (and (= \"http://api.icndb.com/jokes/42\" :-url))
                                                                    :success)]}}
}})"])
