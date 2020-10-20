(ns samak.runtime.stores
  #?@
  (:clj
   [(:require [promesa.core  :as p]
              [clojure.core.async    :as a :refer [<! >! put! chan go go-loop close!]]
              [clojure.edn   :as edn]
              [samak.code-db :as db]
              [samak.api     :as api]
              [samak.pipes  :as pipes]
              [clojure.core.async :as a])]
   :cljs
   [(:require [promesa.core :as p]
              [clojure.core.async    :as a :refer [<! >! put! chan close!]]
              [cljs.reader :as edn]
              [samak.code-db :as db]
              [samak.api :as api]
              [samak.pipes :as pipes])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])]))

(defprotocol SamakStore
  (persist-tree! [this tree])
  (load-by-id [this id])
  (load-network [this id])
  (resolve-name [this db-name]))

(defrecord LocalSamakStore [db]
  SamakStore
  (persist-tree! [_ tree]
    (p/resolved
     (-> (db/parse-tree->db! db tree)
         :tempids
         (dissoc :db/current-tx)
         vals)))
  (load-by-id [_ id]
    (p/resolved (db/load-recurse db id)))
  (load-network [_ id]
    (p/resolved (db/load-network db id)))
  (resolve-name [_ db-name]
    (p/resolved (db/resolve-name db db-name))))

(def resolve-cache (atom {}))

(defrecord RemoteSamakStore [db in out counter]
  SamakStore

  (persist-tree! [_ tree]
    (let [prom (p/deferred)
          id (swap! counter inc)
          c (chan)]
      (a/tap (pipes/out-port in) c)
      ;; (println "req persist" id "-" tree)
      (put! (pipes/in-port out) {:samak.runtime/type :samak.runtime/store :cmd :persist-tree :args {:id id :tree tree}})
      (go-loop [] ;;FIXME timeouts leak
        (when-let [i (<! c)]
          (when (and (= (:cmd i) :persist-tree) (= (:id (:args i)) id))
            (p/resolve! prom (:ids (:args i))))
          (recur)))
      prom))

  (load-by-id [_ db-id]
    (let [prom (p/deferred)
          id (swap! counter inc)
          c (chan)]
        (a/tap (pipes/out-port in) c)
      ;; (println "req load" id "-" db-id)
      (put! (pipes/in-port out) {:samak.runtime/type :samak.runtime/store :cmd :load-by-id :args {:id id :db-id db-id}})
      (go-loop [] ;;FIXME timeouts leak
        (when-let [i (<! c)]
          (when (and (= (:cmd i) :load-by-id) (= (:id (:args i)) id))
            (p/resolve! prom (:ast (:args i))))
          (recur)))
      prom))

  (load-network [_ net-id]
    (let [prom (p/deferred)
          id (swap! counter inc)
          c (chan)]
      (a/tap (pipes/out-port in) c)
      ;; (println "request net" id "-" net-id)
      (put! (pipes/in-port out) {:samak.runtime/type :samak.runtime/store :cmd :load-network :args {:id id :net-id net-id}})
      (go-loop [] ;;FIXME timeouts leak
        (when-let [i (<! c)]
          (when (and (= (:cmd i) :load-network) (= (:id (:args i)) id))
            (p/resolve! prom (:net (:args i))))
          (recur)))
      prom))

  (resolve-name [_ db-name]
    (if-let [e (find @resolve-cache db-name)]
      (val e)
      (let [prom (p/deferred)
            id (swap! counter inc)
            c (chan)]
        (a/tap (pipes/out-port in) c)
        ;; (println "req resolve" id "-" db-name)
        (put! (pipes/in-port out) {:samak.runtime/type :samak.runtime/store :cmd :resolve-name :args {:id id :db-name db-name}})
        (go-loop [] ;;FIXME timeouts leak
          (when-let [i (<! c)]
            (when (and (= (:cmd i) :resolve-name) (= (:id (:args i)) id))
              ;; (println "req resolve in" id "-" i)
              (let [res (:ids (:args i))]
                (swap! resolve-cache assoc db-name res)
                (p/resolve! prom res)))
            (recur)))
        prom))))

(defn serve-store
  ""
  [store in out]
  (println "serve" in out)
  (let [c (chan)]
    (a/tap (pipes/out-port in) c)
    (go-loop []
      (when-let [i (<! c)]
        ;; (println "serve" i (:cmd i))
        (when (= :samak.runtime/store (:samak.runtime/type i))
          (condp = (:cmd i)
            :persist-tree
            (p/then (persist-tree! store (:tree (:args i)))
                    (fn [ids]
                      ;; (println "result persist" ids)
                      (put! (pipes/in-port out) {:cmd :persist-tree :args {:id (:id (:args i)) :ids ids}})))
            :load-by-id
            (p/then (load-by-id store (:db-id (:args i)))
                    (fn [ast]
                      ;; (println "result load" ast)
                      (put! (pipes/in-port out) {:cmd :load-by-id :args {:id (:id (:args i)) :ast ast}})))
            :load-network
            (p/then (load-network store (:net-id (:args i)))
                    (fn [net]
                      ;; (println "result net" net)
                      (put! (pipes/in-port out) {:cmd :load-network :args {:id (:id (:args i)) :net net}})))
            :resolve-name
            (p/then (resolve-name store (:db-name (:args i)))
                    (fn [ids]
                      ;; (println "result resolve" ids)
                      (put! (pipes/in-port out) {:cmd :resolve-name :args {:id (:id (:args i)) :ids ids}})))
            (let [msg (str "unknown store command: " i)] (println msg) (p/rejected msg))))
        (recur))))
  store)


(defn load-builtins! [store builtins]
  (persist-tree! store (mapv (fn [s] (api/defexp s (api/builtin s))) builtins)))

(defn make-local-store []
  (println "local")
  (LocalSamakStore. (db/create-empty-db)))

(defn make-piped-store
  ""
  [in out]
  (println "piped")
  (RemoteSamakStore. (db/create-empty-db) in out (atom 0)))
