(ns neilyio.db
  (:require
   [datalevin.core :as d]
   [neilyio.config :as config]
   [neilyio.events :as events]
   [neilyio.utils :refer [clamp]]))

;; A few things to remember that cost you a lot of time:
;; -- It's :db/valueType, not :db.valueType
;; -- Use :db.type/double, not :db.type.float, because Clojure uses double internally
;;    and you're going to have comparison problems otherwise.
(def schema {:source/bytes {:db/valueType :db.type/bytes :db/unique :db.unique/identity}
             :source/label {:db/valueType :db.type/string}
             :source/beat {:db/valueType :db.type/double}
             :source/bpm {:db/valueType :db.type/long}

             :loop/in {:db/valueType :db.type/double}
             :loop/out {:db/valueType :db.type/double}
             :loop/source {:db/valueType :db.type/ref
                           :db/cardinality :db.cardinality/one
                           :db/type :db.type/ref}

             :speaker/x          {:db/valueType :db.type/long}
             :speaker/y          {:db/valueType :db.type/long}
             :speaker/created-at {:db/valueType :db.type/long}
             :speaker/loop       {:db/valueType :db.type/ref
                                  :db/cardinality :db.cardinality/one
                                  :db/type :db.type/ref}

             :selected/speaker {:db/valueType :db.type/ref :db/unique :db.unique/value}})

(defn module
  "Return an event handler function that takes state and returns new state.
   Args:
   - conn - database connection to use
   Returns a function that takes:
   - state - current application state
   - event - event to handle
   Returns updated state with ::conn and ::db added."
  [get-conn]
  (let [sources (d/q '[:find ?l :where [?e :source/label ?l]] (d/db (get-conn)))]
    (println "Loaded tracks:")
    (doseq [[label] sources] (println label))
    (println))
  [:db (fn [state]
         (let [conn (get-conn)
               db (d/db conn)
               {:neilyio.sound/keys [sample-rate total-frames]} state
               bpm 123
               beat-size (some-> sample-rate (* 60) (/ bpm) (/ total-frames))]
           (assoc state
                  ::beat-size beat-size
                  ::conn conn
                  ::db db)))])

(defn get-conn []
  (d/get-conn config/db-path schema))

(defn delete-all-loops!
  "Deletes all loop entities from the database, including removing references from speakers."
  [conn]
  (let [db (d/db conn)
        ;; Find all speaker->loop references that need to be retracted
        speaker-loops (d/q '[:find ?speaker ?loop
                             :where
                             [?speaker :speaker/loop ?loop]]
                           db)
        ;; Find all loop entities to retract
        loop-ids (d/q '[:find [?e ...]
                        :where [?e :loop/in]]
                      db)]
    (when (or (seq speaker-loops) (seq loop-ids))
      ;; First retract all speaker->loop references
      (when (seq speaker-loops)
        (d/transact! conn (mapv (fn [[speaker loop]]
                                  [:db/retract speaker :speaker/loop loop])
                                speaker-loops)))
      ;; Then retract all loop entities
      (when (seq loop-ids)
        (d/transact! conn (mapv (fn [id]
                                  [:db.fn/retractEntity id])
                                loop-ids))))))

(defn ensure-defaults!
  "Ensures the database has at least one loop, speaker, and source.
   Creates default entities if they don't exist."
  [conn]
  (let [db (d/db conn)
        has-speakers? (seq (d/q '[:find ?e
                                  :where [?e :speaker/created-at]]
                                db))
        has-sources? (seq (d/q '[:find ?e
                                 :where [?e :source/bytes]]
                               db))
        has-loops? (seq (d/q '[:find ?e
                               :where [?e :loop/in]]
                             db))]
    (when-not has-loops?
      (let [loop-tx (d/transact! conn [{:loop/in 0
                                        :loop/out 1}])
            loop-id (get-in loop-tx [:tx-data 0 :e])]
        (when-not has-speakers?
          (d/transact! conn [{:speaker/x 0
                              :speaker/y 0
                              :speaker/created-at (System/currentTimeMillis)
                              :speaker/loop loop-id}]))))

    (when-not has-sources?
      ;; Add a minimal source - in practice you'd want real audio data here
      (throw (ex-info "no sources added to db!" {})))))

(defn add-speaker!
  "Adds a new speaker to the database with optional x and y coordinates.
   Defaults to x=0 and y=0 if not provided."
  [conn]
  (let [tx-result (d/transact! conn [{:speaker/created-at (System/currentTimeMillis)}])]
    (get-in tx-result [:tx-data 0 :e])))

(defn find-all-sources
  "Returns all sources ordered by entity ID"
  [db]
  (->> (d/q '[:find ?e
              :where [?e :source/bytes]]
            db)
       (map first)
       (map #(d/entity db %))))

(defn- select-source!
  "Helper function to select the next/previous source for the currently selected speaker's loop.
   Direction should be :next or :prev."
  [conn direction]
  (let [db (d/db conn)
        sources (sort-by :db/id (find-all-sources db))
        speaker (d/entity db (ffirst (d/q '[:find ?s
                                            :where [?e :selected/speaker ?s]]
                                          db)))]
    (when (and speaker (seq sources))
      (let [loop (or (:speaker/loop speaker)
                     (->> [{:db/id -1 :loop/in 0 :loop/out 1}]
                          (d/transact! conn)
                          :tx-data
                          first
                          :e
                          (d/entity (d/db conn))))
            _ (when-not (:speaker/loop speaker)
                (d/transact! conn [[:db/add (:db/id speaker) :speaker/loop (:db/id loop)]]))
            current-source (:loop/source loop)
            next-source (let [current-idx (if current-source
                                            (.indexOf sources current-source)
                                            -1)
                              offset (if (= :next direction) 1 -1)
                              next-idx (mod (+ current-idx offset) (count sources))]
                          (nth sources next-idx))]
        (when (and next-source loop)
          (d/transact! conn [[:db/add (:db/id loop) :loop/source (:db/id next-source)]]))
        next-source))))

(defn select-next-source!
  "Assigns the next source to the currently selected speaker.
   If no source is currently assigned, selects the first one.
   If the last source is selected, wraps around to the first one."
  [conn]
  (select-source! conn :next))

(defn select-prev-source!
  "Assigns the previous source to the currently selected speaker.
   If no source is currently assigned, selects the first one.
   If the first source is selected, wraps around to the last one."
  [conn]
  (select-source! conn :prev))

(defn find-all-speakers
  "Returns all speakers ordered by creation time"
  [db]
  (->> (d/q '[:find ?e
              :where [?e :speaker/created-at]]
            db)
       (map first)
       (map #(d/entity db %))))

(defn selected-speaker
  "Returns the currently selected speaker entity with all nested references.
   Returns nil if no speaker is currently selected."
  [db]
  (->> (d/q '[:find ?s
              :where [?e :selected/speaker ?s]]
            db)
       ffirst
       (d/entity db)))

(defn- selected-loop
  "Helper function to get the currently selected loop ID.
   Returns nil if no speaker selected or no loop attached."
  [db]
  (some->> (selected-speaker db) :speaker/loop))

(defn- select-speaker!
  "Helper function to select the next/previous speaker.
   Direction should be :next or :prev."
  [conn direction]
  (let [db (d/db conn)
        speakers (sort-by :db/id (find-all-speakers db))
        [selected-entity current-selected]
        (->> (d/q '[:find ?e ?s
                    :where [?e :selected/speaker ?s]]
                  db)
             (first)
             (map #(d/entity db %)))
        next-speaker (if current-selected
                       (let [current-idx (.indexOf speakers current-selected)
                             offset (if (= :next direction) 1 -1)]
                         (nth speakers
                              (mod (+ current-idx offset) (count speakers))
                              (first speakers)))
                       (first speakers))]
    (when next-speaker
      ;; First retract any existing selection
      (when current-selected
        (d/transact! conn [[:db/retract (:db/id selected-entity)
                            :selected/speaker (:db/id current-selected)]]))
      ;; Then add the new selection
      (d/transact! conn [{:selected/speaker (:db/id next-speaker)}]))))

(defn select-next-speaker!
  "Makes the next speaker (by creation time) the selected speaker.
   If no speaker is currently selected, selects the first one.
   If the last speaker is selected, wraps around to the first one."
  [conn]
  (select-speaker! conn :next))

(defn select-prev-speaker!
  "Makes the previous speaker (by creation time) the selected speaker.
   If no speaker is currently selected, selects the first one.
   If the first speaker is selected, wraps around to the last one."
  [conn]
  (select-speaker! conn :prev))

(defn set-loop-point!
  "Sets either the in or out point for the selected speaker's loop.
   point-key should be either :loop/in or :loop/out
   value should be the new point value as normalized 0 - 1 float..
   Returns the updated loop entity if successful, nil if no speaker selected."
  [conn point-key value]
  (when-let [loop (selected-loop (d/db conn))]
    (d/transact! conn [[:db/add (:db/id loop) point-key (clamp value)]])
    loop))

(defn set-loop-in!
  "Sets the in point for the selected speaker's loop.
   value should be the new in point in seconds."
  [conn value]
  (set-loop-point! conn :loop/in value))

(defn set-loop-out!
  "Sets the out point for the selected speaker's loop.
   value should be the new out point in seconds."
  [conn value]
  (set-loop-point! conn :loop/out value))

(defn create-loop-from-selected!
  "Creates a new loop using the source from the selected speaker's loop.
   Returns the new loop entity ID if successful, nil if no speaker selected
   or no source available."
  [conn]
  (let [db (d/db conn)
        speaker-id (ffirst (d/q '[:find ?s
                                  :where [?e :selected/speaker ?s]]
                                db))]
    (when speaker-id
      (let [speaker (d/entity db speaker-id)
            original-loop (some-> speaker :speaker/loop)
            source (some-> original-loop :loop/source)]
      ;; Ensure speaker has a loop with a source
        (when source
        ;; Create new loop with same source
          (let [tx-result (d/transact! conn [{:loop/in 0
                                              :loop/out 1
                                              :loop/source source}])]
            (get-in tx-result [:tx-data 0 :e])))))))

(defn retract-selected-speaker!
  "Retracts the currently selected speaker from the database."
  [conn]
  (let [db (d/db conn)
        selected-speaker-id (ffirst (d/q '[:find ?s
                                           :where [?e :selected/speaker ?s]]
                                         db))]
    (when selected-speaker-id
      (d/transact! conn [[:db/retract [:selected/speaker selected-speaker-id] :selected/speaker selected-speaker-id]])
      true)))

(defn- shift-loop!
  "Helper function to shift a loop by a given amount.
   amount is in terms of beat-size (positive for right, negative for left)"
  [conn beat-size amount]
  (when-let [loop (selected-loop (d/db conn))]
    (let [shift (* amount beat-size)]
      (d/transact! conn
                   [[:db/add (:db/id loop) :loop/in (clamp (+ (or (:loop/in loop) 0) shift))]
                    [:db/add (:db/id loop) :loop/out (clamp (+ (or (:loop/out loop) 1) shift))]]))))

(defn- resize-loop!
  "Helper function to resize a loop to a specific size in beats"
  [conn beat-size num-beats]
  (let [db (d/db conn)
        resize-out #(clamp (+ % (* beat-size num-beats)))
        speaker (selected-speaker db)]
    (if-let [loop (selected-loop db)]
      (d/transact! conn [[:db/add (:db/id loop) :loop/out (resize-out (:loop/in loop))]])
      (when speaker
        (d/transact! conn [[:db/add -1 :loop/in 0 :loop-out (resize-out 0)]
                           [:db/add (:db/id speaker) :speaker/loop -1]])))))

;; Shift functions
(defn loop-beats-right-1!
  "Shift the selected loop right by 1 beat"
  [conn beat-size]
  (shift-loop! conn beat-size 1))

(defn loop-beats-left-1!
  "Shift the selected loop left by 1 beat"
  [conn beat-size]
  (shift-loop! conn beat-size -1))

(defn loop-beats-right-4!
  "Shift the selected loop right by 4 beats"
  [conn beat-size]
  (shift-loop! conn beat-size 4))

(defn loop-beats-left-4!
  "Shift the selected loop left by 4 beats"
  [conn beat-size]
  (shift-loop! conn beat-size -4))

(defn loop-beats-right-16!
  "Shift the selected loop right by 16 beats"
  [conn beat-size]
  (shift-loop! conn beat-size 16))

(defn loop-beats-left-16!
  "Shift the selected loop left by 16 beats"
  [conn beat-size]
  (shift-loop! conn beat-size -16))

(defn loop-beats-right-01!
  "Shift the selected loop right by 1/10th of a beat"
  [conn beat-size]
  (shift-loop! conn beat-size 0.1))

(defn loop-beats-left-01!
  "Shift the selected loop left by 1/10th of a beat"
  [conn beat-size]
  (shift-loop! conn beat-size -0.1))

;; Resize functions
(defn loop-beats-4!
  "Resize the loop to exactly 4 beats"
  [conn beat-size]
  (resize-loop! conn beat-size 4))

(defn loop-beats-double!
  "Double the size of the loop"
  [conn _beat-size]
  (when-let [loop (selected-loop (d/db conn))]
    (let [current-size (- (:loop/out loop) (:loop/in loop))
          new-size (* current-size 2)]
      (set-loop-out! conn (+ (:loop/in loop) new-size)))))

(defn loop-beats-half!
  "Halve the size of the loop"
  [conn _beat-size]
  (when-let [loop (selected-loop (d/db conn))]
    (let [current-size (- (:loop/out loop) (:loop/in loop))
          new-size (/ current-size 2)]
      (set-loop-out! conn (+ (:loop/in loop) new-size)))))

;; Multimethods

(defmethod events/handle [:db :add-speaker] [{::keys [conn]}]
  (add-speaker! conn))

(defmethod events/handle [:db :retract-speaker] [{::keys [conn]}]
  (retract-selected-speaker! conn))

(defmethod events/handle [:db :select-next-speaker] [{::keys [conn]}]
  (select-next-speaker! conn))

(defmethod events/handle [:db :select-prev-speaker] [{::keys [conn]}]
  (select-prev-speaker! conn))

(defmethod events/handle [:db :select-next-source] [{::keys [conn]}]
  (select-next-source! conn))

(defmethod events/handle [:db :select-prev-source] [{::keys [conn]}]
  (select-prev-source! conn))

(defmethod events/handle [:db :delete-all-loops] [{::keys [conn]}]
  (delete-all-loops! conn))

(defmethod events/handle [:db :update-schema] [{::keys [conn]}]
  (d/transact! conn (mapv (fn [[attr details]]
                            (assoc details :db/ident attr))
                          schema)))

;; Loop shifting handlers
(defmethod events/handle [:db :loop-beats-right-1] [{::keys [conn beat-size]}]
  (loop-beats-right-1! conn beat-size))

(defmethod events/handle [:db :loop-beats-left-1] [{::keys [conn beat-size]}]
  (loop-beats-left-1! conn beat-size))

(defmethod events/handle [:db :loop-beats-right-4] [{::keys [conn beat-size]}]
  (loop-beats-right-4! conn beat-size))

(defmethod events/handle [:db :loop-beats-left-4] [{::keys [conn beat-size]}]
  (loop-beats-left-4! conn beat-size))

(defmethod events/handle [:db :loop-beats-right-16] [{::keys [conn beat-size]}]
  (loop-beats-right-16! conn beat-size))

(defmethod events/handle [:db :loop-beats-left-16] [{::keys [conn beat-size]}]
  (loop-beats-left-16! conn beat-size))

(defmethod events/handle [:db :loop-beats-right-01] [{::keys [conn beat-size]}]
  (loop-beats-right-01! conn beat-size))

(defmethod events/handle [:db :loop-beats-left-01] [{::keys [conn beat-size]}]
  (loop-beats-left-01! conn beat-size))

;; Loop resizing handlers
(defmethod events/handle [:db :loop-beats-4] [{::keys [conn beat-size]}]
  (loop-beats-4! conn beat-size))

(defmethod events/handle [:db :loop-beats-double] [{::keys [conn beat-size]}]
  (loop-beats-double! conn beat-size))

(defmethod events/handle [:db :loop-beats-half] [{::keys [conn beat-size]}]
  (loop-beats-half! conn beat-size))

;; Loop point handlers
(defmethod events/handle [:db :set-loop-in] [{::keys [conn]} value]
  (set-loop-in! conn value))

(defmethod events/handle [:db :set-loop-out] [{::keys [conn]} value]
  (set-loop-out! conn value))

;; Loop creation handler
(defmethod events/handle [:db :create-loop-from-selected] [{::keys [conn]}]
  (create-loop-from-selected! conn))

(defmethod events/handle [:db :location] [{::keys [conn] :keys [event]}]
  (when-let [id (second event)]
    (dotimes [_ (mod id 15)]
      (select-next-source! conn))))

(defmethod events/handle [:db :play-toggle] [s]
  (tap> [:db-default s]))
