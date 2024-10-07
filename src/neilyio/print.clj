(ns neilyio.print
  (:require [neilyio.events :as events]
            [datalevin.core :as d]
            [neilyio.db :as db]))

(defn module [get-conn]
  [:print (fn [state]
            (assoc state ::conn (get-conn) ::db (d/db (get-conn))))])

;; Fixed pull patterns
(def basic-pattern
  '[:db/id
    :speaker/x
    :speaker/y])

(def source-pattern
  '[:db/id
    :speaker/x
    :speaker/y
    {:speaker/loop
     [:loop/in
      :loop/out
      {:loop/source
       [:db/id
        :source/label
        :source/beat
        :source/bpm]}]}])

(defn get-selected-speaker [db pattern]
  (when-let [id (ffirst (d/q '[:find ?s :where [_ :selected/speaker ?s]] db))]
    (d/pull db pattern id)))

(defn speaker-event [event pattern]
  (fn [{::keys [db]} & _]
    (when-let [speaker (get-selected-speaker db pattern)]
      {:event event :speaker speaker})))

(defn speaker-value-event [event pattern]
  (fn [{::keys [db]} value]
    (when-let [speaker (get-selected-speaker db pattern)]
      {:event event :value value :speaker speaker})))

;; Event handlers using basic pattern
(doseq [event [:add-speaker :retract-selected-speaker]]
  (defmethod events/handle [:print event] [state & args]
    (apply (speaker-event event basic-pattern) state args)))

;; Event handlers using source pattern
(doseq [event [:select-next-speaker :select-prev-speaker
               :select-next-source :select-prev-source
               :create-loop-from-selected]]
  (defmethod events/handle [:print event] [state & args]
    (apply (speaker-event event source-pattern) state args)))

;; Event handlers using loop pattern
(doseq [event [:loop-beats-right-1 :loop-beats-left-1
               :loop-beats-right-4 :loop-beats-left-4
               :loop-beats-right-16 :loop-beats-left-16
               :loop-beats-right-01 :loop-beats-left-01
               :loop-beats-4 :loop-beats-double :loop-beats-half
               :seek-left :seek-right
               :loop-in-left :loop-in-right
               :loop-out-left :loop-out-right
               :play-toggle]]
  (defmethod events/handle [:print event] [state & args]
    (apply (speaker-event event source-pattern) state args)))

;; Value event handlers
(doseq [event [:set-loop-in :set-loop-out]]
  (defmethod events/handle [:print event] [state & args]
    (apply (speaker-value-event event source-pattern) state args)))

;; Special cases
(defmethod events/handle [:print :delete-all-loops] [{::keys [db]}]
  {:event :delete-all-loops
   :remaining-loops (vec (d/q '[:find ?e :where [?e :loop/in]] db))})

(defmethod events/handle [:print :update-schema] [{::keys [_db]}]
  {:event :update-schema})

(defmethod events/handle [:print :list-speakers] [{::keys [db]}]
  (for [speaker (db/find-all-speakers db)]
    (d/pull db source-pattern speaker)))

(defmethod events/handle [:print :selected-speaker] [{::keys [db]}]
  (get-selected-speaker db source-pattern))

(defmethod events/handle [:print :default] [{:keys [event]}]
  {:event event})
