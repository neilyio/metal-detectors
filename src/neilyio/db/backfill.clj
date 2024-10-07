(ns neilyio.db.backfill
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datalevin.core :as d]
            [neilyio.db :as db]))

(def ^:dynamic *test-db* nil)

(defn db-fixture [f]
  (binding [*test-db* (d/get-conn nil db/schema)]
    (d/transact! *test-db*
                 [{:db/ident :speaker/loop
                   :db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :loop/source
                   :db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/one}])
    (f)
    (d/clear *test-db*)))

(use-fixtures :each db-fixture)

(defn backfill-loop-refs! [conn]
  (let [db (d/db conn)
        speaker-loops (d/q '[:find ?speaker ?loop
                             :where
                             [?speaker :speaker/created-at]
                             [?speaker :speaker/loop ?loop]
                             [?loop :loop/in]]
                           db)]
    (when (seq speaker-loops)
      (d/transact! conn
                   (mapcat (fn [[speaker loop-id]]
                             [[:db/retract speaker :speaker/loop loop-id]
                              [:db/add speaker :speaker/loop loop-id]])
                           speaker-loops)))))

(deftest test-backfill-refs
  (testing "backfilling numeric loop refs to proper refs"
    (let [conn *test-db*
          ;; Create source 
          source-tx (d/transact! conn [{:source/bytes (byte-array [1])
                                        :source/label "Test Source"
                                        :source/beat 0.5
                                        :source/bpm 120}])
          source-id (get-in source-tx [:tx-data 0 :e])

          ;; Create loops
          loop1-tx (d/transact! conn [{:loop/in 0
                                       :loop/out 1
                                       :loop/source source-id}])
          loop1-id (get-in loop1-tx [:tx-data 0 :e])

          loop2-tx (d/transact! conn [{:loop/in 0
                                       :loop/out 2
                                       :loop/source source-id}])
          loop2-id (get-in loop2-tx [:tx-data 0 :e])

          ;; Create speakers
          speaker1-tx (d/transact! conn [{:speaker/x 10
                                          :speaker/y 20
                                          :speaker/created-at 1000
                                          :speaker/loop loop1-id}])
          speaker1-id (get-in speaker1-tx [:tx-data 0 :e])

          speaker2-tx (d/transact! conn [{:speaker/x 30
                                          :speaker/y 40
                                          :speaker/created-at 2000
                                          :speaker/loop loop2-id}])
          speaker2-id (get-in speaker2-tx [:tx-data 0 :e])]

      ;; Verify initial state has numeric refs
      (let [initial-db (d/db conn)]
        (is (= loop1-id
               (ffirst (d/q '[:find ?loop
                              :in $ ?speaker
                              :where [?speaker :speaker/loop ?loop]]
                            initial-db speaker1-id)))
            "Speaker1 should point to loop1")

        (is (= loop2-id
               (ffirst (d/q '[:find ?loop
                              :in $ ?speaker
                              :where [?speaker :speaker/loop ?loop]]
                            initial-db speaker2-id)))
            "Speaker2 should point to loop2"))

      ;; Run backfill
      (backfill-loop-refs! conn)

      ;; Verify refs
      (let [final-db (d/db conn)]
        ;; Verify loop1 data through speaker1
        (let [[in out] (first (d/q '[:find ?in ?out
                                     :in $ ?speaker
                                     :where
                                     [?speaker :speaker/loop ?loop]
                                     [?loop :loop/in ?in]
                                     [?loop :loop/out ?out]]
                                   final-db speaker1-id))]
          (is (= [0.0 1.0] [in out])
              "Speaker1 should still point to loop1 with correct values"))

        ;; Verify loop2 data through speaker2
        (let [[in out] (first (d/q '[:find ?in ?out
                                     :in $ ?speaker
                                     :where
                                     [?speaker :speaker/loop ?loop]
                                     [?loop :loop/in ?in]
                                     [?loop :loop/out ?out]]
                                   final-db speaker2-id))]
          (is (= [0.0 2.0] [in out])
              "Speaker2 should still point to loop2 with correct values"))))))
