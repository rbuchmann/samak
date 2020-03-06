(ns samak.code-db-test
  (:require [samak.testing-tools  :as tt]
            [samak.api            :as api]
            [samak.code-db        :as db]
            [datascript.core      :as d]
            [datascript.core      :as d]
            #?(:clj [clojure.test :as t :refer [is deftest]]
               :cljs [cljs.test   :as t :include-macros true])))

(deftest should-persist-maps
  (let [ast (api/map {(api/keyword :test) (api/integer 1)
                      (api/keyword :foo)  (api/integer 42)})]
    (is (some? (db/parse-tree->db! (db/create-empty-db) [ast])))))

(def builtins ['inc 'dec 'pipes/debug '->])

(defn make-example-db [load-example?]
  (let [db (db/create-empty-db)]
    (db/parse-tree->db! db (map #(api/defexp % (api/builtin %)) builtins))
    (when load-example?
      (db/parse-tree->db! db (:value tt/parsed-example)))
    db))


(deftest db-save-test
  (let [basic-tree-db (make-example-db true)
        result        (d/q '[:find [(pull ?e [*])]
                             :in $ ?name
                             :where [?e :samak.nodes/type :samak.nodes/def]
                             [?e :samak.nodes/name ?name]]
                           @basic-tree-db
                           'foo)]
    (is (= 'foo (:samak.nodes/name (first result))))))

(deftest should-resolve-name
  (let [base   (make-example-db true)
        id     (db/resolve-name base 'foo)
        result (db/load-by-id base id)]
    (is (= 'foo (get-in result [:samak.nodes/name])))))

(deftest should-load-ast
  (let [base   (make-example-db true)
        id     (db/resolve-name base 'foo)
        result (db/load-by-id base id)]
    (is (= 'foo (get-in result [:samak.nodes/name])))
    (is (= 10 (get-in result [:samak.nodes/rhs :db/id])))))

(deftest should-load-by-id
  (let [base   (make-example-db true)
        result (db/load-by-id base 9)]
    (is (= 'foo (get-in result [:samak.nodes/name])))
    (is (= 10 (get-in result [:samak.nodes/rhs :db/id])))))

(deftest should-name-id
  (let [base  (make-example-db true)
        id    1
        names (db/name-node base id)]
    (is (= ['foo] names))
    (is (= id (:db/id (:samak.nodes/fn (:samak.nodes/rhs (db/load-by-id base (db/resolve-name base (first names))))))))
    ))


(deftest should-find-pipes-for-source
  (let [base   (make-example-db true)
        multi-net [(api/defexp 'second-node (api/fn-call (api/symbol 'pipes/debug) []))
                   (api/pipe (api/symbol 'baz) (api/symbol 'second-node))]
        _ (db/parse-tree->db! base multi-net)
        input-id (db/resolve-name base 'input)]
    (is (= 1 (count (db/find-links-from base input-id))))
    (is (= input-id (get-in (first (db/find-links-from base input-id))
                            [:samak.nodes/from :samak.nodes/fn :db/id])))
    ))


(deftest should-load-one-link-network
  (let [base   (make-example-db true)
        named-id (db/resolve-name base 'input)
        network (vals (db/load-network base named-id))
        loaded (first network)]
    (is (= 1 (count network)))
    (is (= 1 (count (:ends loaded))))
    (is (= 1 (count (:pipes loaded))))
    (is (= :samak.nodes/def (:samak.nodes/type (db/load-by-id base (first (:ends loaded))))))))


(deftest should-load-multi-link-network
  (let [db (make-example-db true)
        multi-net [(api/defexp 'second-node (api/fn-call (api/symbol 'pipes/debug) []))
                   (api/pipe (api/symbol 'baz) (api/symbol 'second-node))
                   (api/defexp 'third-node (api/fn-call (api/symbol 'pipes/debug) []))
                   (api/pipe (api/symbol 'baz) (api/symbol 'third-node))
                   (api/defexp 'fourth-node (api/fn-call (api/symbol 'pipes/debug) []))
                   (api/pipe (api/symbol 'third-node) (api/symbol 'fourth-node))
                   ]
        _ (db/parse-tree->db! db multi-net)
        named-id (db/resolve-name db 'input)
        net (:db/id (:samak.nodes/rhs (db/load-by-id db named-id)))
        network (first (vals (db/load-network db named-id)))]
    (is (= 4 (count (:ends network))))
    (is (= 4 (count (:pipes network))))
    ))

(deftest should-walk-maps-on-load-by-id
  (let [db      (make-example-db false)
        parsed  [(assoc (api/map {(api/keyword :key) (api/symbol 'inc)})
                        :db/id -1)]
        map-id  (get-in (db/parse-tree->db! db parsed) [:tempids -1])
        result  (db/load-by-id db map-id)
        kv-pair (first (:samak.nodes/mapkv-pairs result))]
    (is (= :samak.nodes/map (:samak.nodes/type result)))
    (is (map? kv-pair))
    (contains? kv-pair :samak.nodes/mapkey)
    (contains? kv-pair :samak.nodes/mapvalue)))

(deftest should-refer-fn-by-id
  (let [db        (make-example-db false)
        src       [(assoc (api/defexp 'foo (api/symbol 'inc)) :db/id -1)]
        foo-id    (get-in (db/parse-tree->db! db src) [:tempids -1])
        used      [(assoc (api/defexp 'bar (api/fn-call {:db/id foo-id} [])) :db/id -1)]
        result-id (get-in (db/parse-tree->db! db used) [:tempids -1])
        result    (db/load-by-id db result-id)]
    (is (= 'bar (get-in result [:samak.nodes/name])))
    (is (= foo-id (get-in result [:samak.nodes/rhs :samak.nodes/fn-expression :db/id])))))

(deftest should-retract-from-args
  (let [db (db/create-empty-db)
        test-ast (api/defexp 'test (api/vector [(api/integer 0) (api/integer 1)]))
        base (do (db/parse-tree->db! db [test-ast]) db)
        persisted (db/load-by-id base 2)
        updated (assoc persisted :samak.nodes/children [])
        upsert (do (db/parse-tree->db! db [[:db/retract 2 :samak.nodes/children 3]]) db)
        result (db/load-by-id upsert 2)]
    (is (= 1 (count (get-in result [:samak.nodes/children]))))))


(defn load-named-dependencies [db id]
  (map (comp :samak.nodes/name (partial db/load-by-id db))
       (db/load-dependencies db id)))

(deftest should-calculate-dependencies
  (let [db (make-example-db true)]
    (is (= '[inc] (load-named-dependencies db (db/resolve-name db 'foo))))
    (is (= '[dec] (load-named-dependencies db (db/resolve-name db 'bar))))
    (is (= '[] (load-named-dependencies db (db/resolve-name db 'baz))))))

(defn load-named-dependency-edges [db id]
  (for [edge (db/load-dependency-edges db id)]
    (map (comp :samak.nodes/name (partial db/load-by-id db)) edge)))

(deftest should-calculate-dep-edges
  (let [db (make-example-db true)]
    (is (= '[(bar dec) (input pipes/debug) (foo inc)] (load-named-dependency-edges db (db/resolve-name db 'pipe))))))
