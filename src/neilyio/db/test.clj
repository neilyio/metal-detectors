(ns neilyio.db.test
  {:clj-kondo/ignore [:refer-all :unused-referred-var]}
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is run-tests testing use-fixtures]]
   [datalevin.core :as d]
   [neilyio.db :as db]))

(def ^:dynamic *test-db* nil)

(defn db-fixture [f]
  ;; Passing nil for dir means it creates a temporary db.
  (binding [*test-db* (d/get-conn nil db/schema)]
    (f)
    (d/clear *test-db*)))

(use-fixtures :each db-fixture)

(deftest db-fixture-test
  (is (not (nil? *test-db*)) "The test database connection should not be nil")
  (is (d/conn? *test-db*) "The test database connection should be a valid Datalevin connection"))

(deftest ensure-defaults-test
  (let [conn *test-db*]

    ;; Initially should have no entities
    (is (empty? (d/q '[:find ?e :where [?e :speaker/created-at]] (d/db conn))))
    (is (empty? (d/q '[:find ?e :where [?e :loop/in]] (d/db conn))))
    (is (empty? (d/q '[:find ?e :where [?e :source/bytes]] (d/db conn))))

    ;; Add a test source file
    (let [test-bytes (byte-array [1 2 3])]
      (d/transact! conn [{:source/bytes test-bytes
                          :source/label "Test Source"
                          :source/bpm 120}]))

    ;; Call ensure-defaults! - should succeed now that we have a source
    (db/ensure-defaults! conn)

    ;; Should now have default speaker and loop
    (let [db (d/db conn)]
      (is (= 1 (count (d/q '[:find ?e :where [?e :speaker/created-at]] db))))
      (is (= 1 (count (d/q '[:find ?e :where [?e :loop/in]] db))))
      (is (= 1 (count (d/q '[:find ?e :where [?e :source/bytes]] db))))

      ;; Verify speaker properties
      (let [[speaker] (d/q '[:find (pull ?e [:speaker/x :speaker/y])
                             :where [?e :speaker/created-at]] db)]
        (is (= {:speaker/x 0 :speaker/y 0} (first speaker))))

      ;; Verify loop properties  
      (let [[loop] (d/q '[:find (pull ?e [:loop/in :loop/out])
                          :where [?e :loop/in]] db)]
        (is (= {:loop/in 0.0 :loop/out 1.0} (first loop)))))))

(deftest select-next-speaker-test
  (testing "speaker selection"
    (let [conn *test-db*]

;; Create test speakers
      (d/transact! conn [{:speaker/x 0
                          :speaker/y 0
                          :speaker/created-at 1000}
                         {:speaker/x 1
                          :speaker/y 1
                          :speaker/created-at 2000}
                         {:speaker/x 2
                          :speaker/y 2
                          :speaker/created-at 3000}])

      ;; Get all speakers ordered by creation time
      (let [db (d/db conn) speakers (db/find-all-speakers db)]
        (is (= 3 (count speakers)) "Should have 3 speakers")

        ;; Test initial selection (no current selection)
        (db/select-next-speaker! conn)
        (let [selected (->> (d/q '[:find ?s
                                   :where [?e :selected/speaker ?s]]
                                 (d/db conn))
                            (ffirst)
                            (d/entity db))]
          (is (= (nth speakers 2) selected) "Should select first speaker"))

        ;; Test next selection
        (db/select-next-speaker! conn)
        (let [selected (->> (d/q '[:find ?s
                                   :where [?e :selected/speaker ?s]]
                                 (d/db conn))
                            (ffirst)
                            (d/entity db))]
          (is (= (nth speakers 1) selected) "Should select second speaker"))

        ;; Test wrap-around with next
        (db/select-next-speaker! conn)
        (db/select-next-speaker! conn)
        (let [selected (->> (d/q '[:find ?s
                                   :where [?e :selected/speaker ?s]]
                                 (d/db conn))
                            (ffirst)
                            (d/entity db))]
          (is (= (nth speakers 2) selected) "Should wrap around to first speaker"))

        ;; Test prev selection
        (db/select-prev-speaker! conn)
        (let [selected (->> (d/q '[:find ?s
                                   :where [?e :selected/speaker ?s]]
                                 (d/db conn))
                            (ffirst)
                            (d/entity db))]
          (is (= (nth speakers 0) selected) "Should select last speaker"))

        ;; Test wrap-around with prev
        (db/select-prev-speaker! conn)
        (let [selected (->> (d/q '[:find ?s
                                   :where [?e :selected/speaker ?s]]
                                 (d/db conn))
                            (ffirst)
                            (d/entity db))]
          (is (= (nth speakers 1) selected) "Should wrap around backwards"))))))

(deftest source-selection-setup-test
  (testing "source and speaker setup"
    (let [conn *test-db*
          tx-result (d/transact! conn [{:speaker/x 0
                                        :speaker/y 0
                                        :speaker/created-at 1000}])
          speaker-id (get-in tx-result [:tx-data 0 :e])
          sources-tx (d/transact! conn [{:source/bytes (byte-array [1])
                                         :source/label "A"}
                                        {:source/bytes (byte-array [2])
                                         :source/label "B"}
                                        {:source/bytes (byte-array [3])
                                         :source/label "C"}])
          source-ids (->> (:tx-data sources-tx)
                          (map :e)
                          (take 3))]

      (is (number? speaker-id) "Speaker should be created with valid ID")
      (is (= 3 (count source-ids)) "Should create 3 sources")
      (is (every? number? source-ids) "Each source should have a numeric entity ID"))))

(deftest initial-source-selection-test
  (testing "initial source selection with no previous source"
    (let [conn *test-db*
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 0
                                                  :speaker/y 0
                                                  :speaker/created-at 1000}]))
          [_] (:tx-data (d/transact! conn [{:source/bytes (byte-array [1])
                                            :source/label "A"}]))]

      ;; Select speaker
      (d/transact! conn [{:selected/speaker (:e speaker)}])

      ;; Test initial selection
      (db/select-next-source! conn)
      #_(let [updated-speaker (d/entity (d/db conn) (:e speaker))]
          (let [loop-id (:speaker/loop updated-speaker)
                loop-entity (d/entity (d/db conn) loop-id)]
            (is (= (:e source1) (:loop/source loop-entity))
                "Should assign first source to speaker's loop when no source is selected"))))))

(deftest next-source-selection-test
  #_(testing "selecting next source"
      (let [conn *test-db*
            [speaker] (:tx-data (d/transact! conn [{:speaker/x 0
                                                    :speaker/y 0
                                                    :speaker/created-at 1000}]))
            _ (d/transact! conn [{:source/bytes (byte-array [1])
                                  :source/label "A"}
                                 {:source/bytes (byte-array [2])
                                  :source/label "B"}])
            source-ids (db/find-all-sources (d/db conn))]

      ;; Setup initial state
        (d/transact! conn [{:selected/speaker (:e speaker)}])
        (db/select-next-source! conn)

      ;; Test next selection
        (db/select-next-source! conn)
        #_(let [updated-speaker (d/entity (d/db conn) (:e speaker))
                loop-id (:speaker/loop updated-speaker)
                loop-entity (d/entity (d/db conn) loop-id)
                source-id (:loop/source loop-entity)
                expected-id (second source-ids)]
            (is (= source-id expected-id)
                (str "Should select next source in sequence. "
                     "Expected source ID " expected-id
                     " but got " source-id))))))

(deftest prev-source-selection-test
  (testing "selecting previous source"
    (let [conn *test-db*
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 0
                                                  :speaker/y 0
                                                  :speaker/created-at 1000}]))
          _ (d/transact! conn [{:source/bytes (byte-array [1])
                                :source/label "A"}
                               {:source/bytes (byte-array [2])
                                :source/label "B"}
                               {:source/bytes (byte-array [3])
                                :source/label "C"}])]
      ;; Setup initial state
      (d/transact! conn [{:selected/speaker (:e speaker)}])
      (db/select-next-source! conn)

      ;; Test prev selection
      (db/select-prev-source! conn)
      (let [db (d/db conn)
            updated-speaker (d/entity db (:e speaker))
            loop (:speaker/loop updated-speaker)
            source (:loop/source loop)]
        (is (= source (d/entity db 4))
            (str "Should wrap to last source when selecting prev from first source."))))))

(deftest set-loop-points-test
  (testing "setting loop in/out points"
    (let [conn *test-db*
          ;; Create test speaker and source
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 0
                                                  :speaker/y 0
                                                  :speaker/created-at 1000}]))
          [source] (:tx-data (d/transact! conn [{:source/bytes (byte-array [1])
                                                 :source/label "Test Source"}]))
          ;; Create initial loop with source
          [loop1] (:tx-data (d/transact! conn [{:loop/in 0
                                                :loop/out 1
                                                :loop/source (:e source)}]))
          ;; Assign loop to speaker and select it
          _ (d/transact! conn [[:db/add (:e speaker)
                                :speaker/loop (:e loop1)]
                               {:selected/speaker (:e speaker)}])]

      (testing "setting in point"
        (let [updated-loop (db/set-loop-in! conn 0.5)]
          (is (= 0.5 (:loop/in updated-loop)) "Should update in point")
          (is (= 1.0 (:loop/out updated-loop)) "Should not change out point")))

      (testing "setting out point"
        (let [updated-loop (db/set-loop-out! conn 1.5)]
          (is (= 0.5 (:loop/in updated-loop)) "Should not change in point")
          (is (= 1.0 (:loop/out updated-loop)) "Should update out point")))

      (testing "with no selected speaker"
        ;; Clear speaker selection
        (d/transact! conn [[:db/retract [:selected/speaker (:e speaker)] :selected/speaker (:e speaker)]])
        (is (nil? (db/set-loop-in! conn 0.75)) "Should return nil when no speaker selected")
        (is (nil? (db/set-loop-out! conn 1.75)) "Should return nil when no speaker selected")))))

(deftest create-loop-from-selected-basic-test
  (testing "basic loop creation from selected speaker"
    (let [conn *test-db*
          ;; Create test speaker and source
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 0
                                                  :speaker/y 0
                                                  :speaker/created-at 1000}]))
          [source] (:tx-data (d/transact! conn [{:source/bytes (byte-array [1])
                                                 :source/label "Test Source"}]))
          ;; Create initial loop with source
          [loop1] (:tx-data (d/transact! conn [{:loop/in 0
                                                :loop/out 1
                                                :loop/source (:e source)}]))
          ;; Assign loop to speaker and select it
          _ (d/transact! conn [[:db/add (:e speaker)
                                :speaker/loop (:e loop1)]
                               {:selected/speaker (:e speaker)}])]

      (testing "returns valid loop ID"
        (let [new-loop-id (db/create-loop-from-selected! conn)]
          (is (number? new-loop-id) "Should return new loop ID")))

      (testing "creates loop with correct structure"
        (let [new-loop-id (db/create-loop-from-selected! conn)
              new-loop (d/entity (d/db conn) new-loop-id)]
          (is (= 0.0 (:loop/in new-loop)) "Should have correct in point")
          (is (= 1.0 (:loop/out new-loop)) "Should have correct out point")
          (is (map? (:loop/source new-loop)) "Should have a source reference"))))))

(deftest add-speaker-test
  (testing "adding a speaker with default coordinates"
    (let [conn *test-db*
          result (db/add-speaker! conn)]
      (is (number? result) "Should return a numeric ID for the new speaker")
      (let [speaker (d/entity (d/db conn) result)]
        (is (nil? (:speaker/x speaker)) "Default x coordinate should be nil")
        (is (nil? (:speaker/y speaker)) "Default y coordinate should be nil")))))

(deftest retract-selected-speaker-test
  (testing "retracting the selected speaker"
    (let [conn *test-db*
          speaker-id (db/add-speaker! conn)]
      ;; Select the speaker
      (d/transact! conn [{:selected/speaker speaker-id}])
      ;; Ensure the speaker is selected
      (is (= speaker-id (ffirst (d/q '[:find ?s
                                       :where [?e :selected/speaker ?s]]
                                     (d/db conn)))))
      ;; Retract the selected speaker
      (db/retract-selected-speaker! conn)
      ;; Ensure the speaker is no longer selected
      (is (nil? (ffirst (d/q '[:find ?s
                               :where [?e :selected/speaker ?s]]
                             (d/db conn))))))))

(deftest selected-speaker-basic-test
  (testing "with no speaker selected"
    (let [conn *test-db*]
      (is (nil? (db/selected-speaker (d/db conn)))
          "Should return nil when no speaker is selected"))))

(deftest selected-speaker-simple-test
  (testing "with just speaker coordinates"
    (let [conn *test-db*
          ;; Create basic speaker
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 10
                                                  :speaker/y 20
                                                  :speaker/created-at 1000}]))]
      ;; Select the speaker
      (d/transact! conn [{:selected/speaker (:e speaker)}])

      (let [result (db/selected-speaker (d/db conn))]
        (is (= (set {:speaker/x 10
                     :speaker/y 20
                     :speaker/created-at 1000})
               (set result))
            "Should return basic speaker data")))))

(deftest selected-speaker-with-loop-test
  (testing "with speaker and basic loop"
    (let [conn *test-db*
          db (d/db conn)
          ;; Create speaker
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 10
                                                  :speaker/y 20
                                                  :speaker/created-at 1000}]))
          ;; Create loop
          [loop] (:tx-data (d/transact! conn [{:loop/in 0.5
                                               :loop/out 2.5}]))]
      ;; Link loop to speaker and select speaker
      (d/transact! conn [[:db/add (:e speaker) :speaker/loop (:e loop)]
                         {:selected/speaker (:e speaker)}])

      (let [speaker (db/selected-speaker db)]
        (is (= 0.5 (-> speaker :speaker/loop :loop/in)))
        (is (= 2.5 (-> speaker :speaker/loop :loop/out)))
        (is (set/subset? (set {:speaker/x 10
                               :speaker/y 20
                               :speaker/created-at 1000
                               :speaker/loop (d/entity db 2)})
                         (set speaker))
            "Should return speaker with loop data")))))

(deftest source-beat-test
  (testing "source creation with beat value"
    (let [conn *test-db*
          test-bytes (byte-array [1 2 3])
          source-tx (d/transact! conn [{:source/bytes test-bytes
                                        :source/label "Test Source"
                                        :source/bpm 120
                                        :source/beat 0.5}])
          source-id (get-in source-tx [:tx-data 0 :e])]

      (testing "basic source creation"
        (let [source (d/entity (d/db conn) source-id)]
          (is (= 0.5 (:source/beat source))
              "Should store correct beat value")
          (is (= "Test Source" (:source/label source))
              "Should maintain other source attributes")))

      (testing "source retrieval in loop context"
        ;; Create a loop using this source
        (let [[speaker] (:tx-data (d/transact! conn [{:speaker/x 0
                                                      :speaker/y 0
                                                      :speaker/created-at 1000}]))
              [loop] (:tx-data (d/transact! conn [{:loop/in 0
                                                   :loop/out 1
                                                   :loop/source source-id}]))]
          ;; Link loop to speaker and select speaker
          (d/transact! conn [[:db/add (:e speaker) :speaker/loop (:e loop)]
                             {:selected/speaker (:e speaker)}])

          (let [speaker (d/entity (d/db conn) (:e speaker))
                loop (:speaker/loop speaker)
                source (:loop/source loop)]
            (is (some? source) "Should include source data")
            (is (= source-id (:db/id source)) "Should have correct source ID")))))))

(deftest selected-speaker-full-test
  (testing "with speaker, loop, and source including beat value"
    (let [conn *test-db*
          db (d/db conn)
          ;; Create source first with beat value
          [source] (:tx-data (d/transact! conn [{:source/bytes (byte-array [1])
                                                 :source/label "Test Source"
                                                 :source/beat 0.75
                                                 :source/bpm 120}]))
          ;; Create loop with source
          [loop] (:tx-data (d/transact! conn [{:loop/in 0.5
                                               :loop/out 2.5
                                               :loop/source (:e source)}]))
          ;; Create and link speaker
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 10
                                                  :speaker/y 20
                                                  :speaker/created-at 1000
                                                  :speaker/loop (:e loop)}]))]
      ;; Select speaker
      (d/transact! conn [{:selected/speaker (:e speaker)}])

      (let [speaker (db/selected-speaker db)]
        ;; Test speaker fields directly from result
        (is (= #{[:speaker/x 10]
                 [:speaker/y 20]
                 [:speaker/created-at 1000]
                 [:speaker/loop (d/entity db (:e loop))]}
               (set speaker))
            "Should have correct speaker attributes")

        ;; Test loop fields through entity reference
        (is (= #{[:loop/in 0.5]
                 [:loop/out 2.5]
                 [:loop/source (d/entity db (:e source))]}
               (set (:speaker/loop speaker)))
            "Should have correct loop attributes")

        ;; Test source fields through entity reference
        (is (set/subset? #{[:source/label "Test Source"]
                           [:source/beat 0.75]
                           [:source/bpm 120]}
                         (set (-> speaker :speaker/loop :loop/source)))
            "Should have correct source attributes")))))

(deftest delete-all-loops-test
  (testing "deleting all loops"
    (let [conn *test-db*
          ;; Create test speaker and source
          [speaker1] (:tx-data (d/transact! conn [{:speaker/x 10
                                                   :speaker/y 20
                                                   :speaker/created-at 1000}]))
          [speaker2] (:tx-data (d/transact! conn [{:speaker/x 30
                                                   :speaker/y 40
                                                   :speaker/created-at 2000}]))
          [source] (:tx-data (d/transact! conn [{:source/bytes (byte-array [1])
                                                 :source/label "Test Source"}]))
          ;; Create loops with source
          [loop1] (:tx-data (d/transact! conn [{:loop/in 0
                                                :loop/out 1
                                                :loop/source (:e source)}]))
          [loop2] (:tx-data (d/transact! conn [{:loop/in 0
                                                :loop/out 1
                                                :loop/source (:e source)}]))]

      ;; Assign loops to speakers
      (d/transact! conn [[:db/add (:e speaker1)
                          :speaker/loop (:e loop1)]
                         [:db/add (:e speaker2)
                          :speaker/loop (:e loop2)]])

      ;; Verify initial state
      (let [initial-db (d/db conn)]
        (is (= 2 (count (d/q '[:find ?e
                               :where [?e :loop/in]]
                             initial-db)))
            "Should start with two loops")
        (is (= 2 (count (d/q '[:find ?e
                               :where
                               [?e :speaker/created-at]
                               [?e :speaker/loop]]
                             initial-db)))
            "Should start with two speakers with loops"))

      ;; Delete all loops
      (db/delete-all-loops! conn)

      ;; Verify final state
      (let [final-db (d/db conn)]
        (is (empty? (d/q '[:find ?e
                           :where [?e :loop/in]]
                         final-db))
            "Should have no loops after deletion")

        (is (empty? (d/q '[:find ?e
                           :where
                           [?e :speaker/created-at]
                           [?e :speaker/loop]]
                         final-db))
            "Should have no speakers with loops after deletion")))))

(deftest cardinal-direction-selection-tests
  (testing "selecting speakers in cardinal directions"
    (let [conn *test-db*
          ;; Create a grid of test speakers
          center-tx (d/transact! conn [{:speaker/x 0
                                        :speaker/y 0
                                        :speaker/created-at 1000}])
          north-tx (d/transact! conn [{:speaker/x 0
                                       :speaker/y -1
                                       :speaker/created-at 1001}])
          south-tx (d/transact! conn [{:speaker/x 0
                                       :speaker/y 1
                                       :speaker/created-at 1002}])
          east-tx (d/transact! conn [{:speaker/x 1
                                      :speaker/y 0
                                      :speaker/created-at 1003}])
          west-tx (d/transact! conn [{:speaker/x -1
                                      :speaker/y 0
                                      :speaker/created-at 1004}])
          center-id (get-in center-tx [:tx-data 0 :e])
          north-id (get-in north-tx [:tx-data 0 :e])
          south-id (get-in south-tx [:tx-data 0 :e])
          east-id (get-in east-tx [:tx-data 0 :e])
          west-id (get-in west-tx [:tx-data 0 :e])]

      ;; Select center speaker as starting point
      (d/transact! conn [{:selected/speaker center-id}])

      (testing "selecting north speaker"
        (let [result (db/select-north! conn)
              selected (db/selected-speaker (d/db conn))]
          (is (= north-id (:db/id result)) "Should return north speaker")
          (is (= north-id (:db/id selected)) "Should select north speaker")))

      (testing "selecting south speaker from north"
        (let [result (db/select-south! conn)
              selected (db/selected-speaker (d/db conn))]
          (is (= center-id (:db/id result)) "Should return center speaker")
          (is (= center-id (:db/id selected)) "Should select center speaker")))

      (testing "selecting east speaker from center"
        (let [result (db/select-east! conn)
              selected (db/selected-speaker (d/db conn))]
          (is (= east-id (:db/id result)) "Should return east speaker")
          (is (= east-id (:db/id selected)) "Should select east speaker")))

      (testing "selecting west speaker from east"
        (let [result (db/select-west! conn)
              selected (db/selected-speaker (d/db conn))]
          (is (= center-id (:db/id result)) "Should return center speaker")
          (is (= center-id (:db/id selected)) "Should select center speaker")))

      (testing "no speaker in direction"
        ;; Select north speaker
        (d/transact! conn [{:selected/speaker north-id}])
        (is (nil? (db/select-north! conn)) "Should return nil when no speaker north")
        (let [selected (db/selected-speaker (d/db conn))]
          (is (= north-id (:db/id selected)) "Should not change selection"))))))

(deftest loop-manipulation-tests
  (testing "loop manipulation functions"
    (let [conn *test-db*
          ;; Create test speaker and loop with initial points inside valid range
          [speaker] (:tx-data (d/transact! conn [{:speaker/x 0
                                                  :speaker/y 0
                                                  :speaker/created-at 1000}]))
          [loop1] (:tx-data (d/transact! conn [{:loop/in 0.2
                                                :loop/out 0.4}]))
          beat-size 0.25]

      ;; Setup initial state - link loop to speaker and select speaker
      (d/transact! conn [[:db/add (:e speaker) :speaker/loop (:e loop1)]
                         {:selected/speaker (:e speaker)}])

      (testing "shifting right by 1 beat"
        (db/loop-beats-right-1! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.45 (:loop/in updated-loop))
              "In point should shift right by one beat")
          (is (= 0.65 (:loop/out updated-loop))
              "Out point should shift right by one beat")))

      (testing "shifting left by 1 beat"
        (db/loop-beats-left-1! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.2 (:loop/in updated-loop))
              "In point should shift left by one beat")
          (is (= 0.4 (:loop/out updated-loop))
              "Out point should shift left by one beat")))

      (testing "shifting right by 4 beats - should clamp to 1.0"
        (db/loop-beats-right-4! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 1.0 (:loop/in updated-loop))
              "In point should be clamped at 1.0")
          (is (= 1.0 (:loop/out updated-loop))
              "Out point should be clamped at 1.0")))

      (testing "shifting left by 4 beats - should clamp to 0.0"
        (db/loop-beats-left-4! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.0 (:loop/in updated-loop))
              "In point should be clamped at 0.0")
          (is (= 0.0 (:loop/out updated-loop))
              "Out point should be clamped at 0.0")))

      (testing "shifting right by 16 beats - should clamp to 1.0"
        (db/loop-beats-right-16! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 1.0 (:loop/in updated-loop))
              "In point should be clamped at 1.0")
          (is (= 1.0 (:loop/out updated-loop))
              "Out point should be clamped at 1.0")))

      (testing "shifting left by 16 beats - should clamp to 0.0"
        (db/loop-beats-left-16! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.0 (:loop/in updated-loop))
              "In point should be clamped at 0.0")
          (is (= 0.0 (:loop/out updated-loop))
              "Out point should be clamped at 0.0")))

      (testing "shifting right by 0.1 beats"
        (db/loop-beats-right-01! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.0025 (:loop/in updated-loop))
              "In point should shift right by 1/10th beat")
          (is (= 0.0025 (:loop/out updated-loop))
              "Out point should shift right by 1/10th beat")))

      (testing "shifting left by 0.1 beats - should clamp to 0.0"
        (db/loop-beats-left-01! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.0 (:loop/in updated-loop))
              "In point should be clamped at 0.0")
          (is (= 0.0 (:loop/out updated-loop))
              "Out point should be clamped at 0.0")))

      (testing "resizing to 4 beats - should clamp out point to 1.0"
        (db/loop-beats-4! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.0 (:loop/in updated-loop))
              "In point should remain at 0.0")
          (is (= 1.0 (:loop/out updated-loop))
              "Out point should be clamped at 1.0")))

      (testing "doubling loop size - should clamp out point to 1.0"
        (db/loop-beats-double! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.0 (:loop/in updated-loop))
              "In point should remain at 0.0")
          (is (= 1.0 (:loop/out updated-loop))
              "Out point should be clamped at 1.0")))

      (testing "halving loop size from maximum"
        ;; First set up a loop at maximum size
        (d/transact! conn [[:db/add (:e loop1) :loop/in 0.0]
                           [:db/add (:e loop1) :loop/out 1.0]])
        (db/loop-beats-half! conn beat-size)
        (let [updated-loop (d/entity (d/db conn) (:e loop1))]
          (is (= 0.0 (:loop/in updated-loop))
              "In point should remain at 0.0")
          (is (= 0.5 (:loop/out updated-loop))
              "Out point should be halved")))

      (testing "with no selected speaker"
        ;; Clear speaker selection
        (d/transact! conn [[:db/retract [:selected/speaker (:e speaker)]
                            :selected/speaker (:e speaker)]])
        (is (nil? (db/loop-beats-right-1! conn beat-size))
            "Should return nil when no speaker selected")
        (is (nil? (db/loop-beats-4! conn beat-size))
            "Should return nil when no speaker selected")
        (is (nil? (db/loop-beats-double! conn beat-size))
            "Should return nil when no speaker selected")))))

;; Add new test for source/beat



