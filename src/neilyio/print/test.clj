(ns neilyio.print.test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [neilyio.events :as events]
            [neilyio.print :as print]
            [datalevin.core :as d]
            [clojure.set :as set]
            [neilyio.db :as db]))

(def ^:dynamic *test-db* nil)

(defn db-fixture [f]
  (binding [*test-db* (d/get-conn nil db/schema)]
    (f)
    (d/clear *test-db*)))

(use-fixtures :each db-fixture)

(deftest test-speaker-event-patterns
  (testing "event patterns for various operations"
    (let [conn *test-db*
          db (d/db conn)
          ;; Create test data
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 10
                                                  :speaker/y 20
                                                  :speaker/created-at 1000}]))
          [source] (:tx-data (d/transact! conn [{:source/bytes (byte-array [1])
                                                 :source/label "Test Source"
                                                 :source/beat 0.5
                                                 :source/bpm 120}]))
          [loop] (:tx-data (d/transact! conn [{:loop/in 0
                                               :loop/out 1
                                               :loop/source (:e source)}]))]

      ;; Link loop to speaker and select it
      (d/transact! conn [[:db/add (:e speaker) :speaker/loop (:e loop)]
                         {:selected/speaker (:e speaker)}])

      (testing "basic speaker pattern"
        (let [result (events/handle {:module :print
                                     :event [:add-speaker]
                                     :neilyio.print/db db})
              expected {:event :add-speaker
                        :speaker {:db/id (:e speaker)
                                  :speaker/x 10
                                  :speaker/y 20}}]
          (is (= expected result))))

      (testing "loop pattern"
        (let [result (events/handle {:module :print
                                     :event [:loop-beats-right-1]
                                     :neilyio.print/db db})]
          (is (set/subset? (set {:db/id (:e speaker) :speaker/x 10 :speaker/y 20})
                           (set (:speaker result))))))

      (testing "source pattern"
        (let [result (events/handle {:module :print
                                     :event [:select-next-source]
                                     :neilyio.print/db db})]
          (is (= :select-next-source (:event result)))
          (is (set/subset? (set {:db/id (:e speaker) :speaker/x 10 :speaker/y 20})
                           (set (:speaker result)))))))))

(deftest test-special-events
  (testing "events with special handling"
    (let [conn *test-db*
          db (d/db conn)
          [loop] (:tx-data (d/transact! conn [{:loop/in 0 :loop/out 1}]))]

      (testing "delete-all-loops"
        (let [result (events/handle {:module :print
                                     :event [:delete-all-loops]
                                     :neilyio.print/db db})
              expected {:event :delete-all-loops
                        :remaining-loops [[(:e loop)]]}]
          (is (= expected result)))))))

#_(deftest test-list-and-select
    (testing "list-speakers and selected-speaker"
      (let [conn *test-db*
            db (d/db conn)
          ;; Create test data
            [speaker1] (:tx-data (d/transact! conn [{:speaker/x 10
                                                     :speaker/y 20
                                                     :speaker/created-at 1000}]))
            [speaker2] (:tx-data (d/transact! conn [{:speaker/x 30
                                                     :speaker/y 40
                                                     :speaker/created-at 2000}]))
            [source] (:tx-data (d/transact! conn [{:source/bytes (byte-array [1])
                                                   :source/label "Test Source"}]))]

        (let [loop-tx (d/transact! conn [{:loop/in 0
                                          :loop/out 1
                                          :loop/source (:e source)}])
              loop-id (get-in loop-tx [:tx-data 0 :e])]
          (d/transact! conn [[:db/add (:e speaker1) :speaker/loop loop-id]])

          (testing "list-speakers"
            (let [result (events/handle {:module :print
                                         :event [:list-speakers]
                                         :neilyio.print/db db})]
              (is (= 2 (count result)))
              (is (= [{:id (:e speaker1)
                       :x 10 :y 20 :created-at 1000
                       :source {:id (:e source) :label "Test Source"}}
                      {:id (:e speaker2)
                       :x 30 :y 40 :created-at 2000
                       :source nil}]
                     result))))

          (testing "selected-speaker"
            (d/transact! conn [{:selected/speaker (:e speaker1)}])
            (let [result (events/handle {:module :print
                                         :event [:selected-speaker]
                                         :neilyio.print/db db})]
              (is (= {:db/id (:e speaker1)
                      :speaker/x 10
                      :speaker/y 20
                      :speaker/created-at 1000
                      :speaker/loop {:db/id loop-id
                                     :loop/in 0
                                     :loop/out 1
                                     :loop/source {:db/id (:e source)
                                                   :source/label "Test Source"
                                                   :source/beat nil
                                                   :source/bpm nil}}}
                     result))))))))
