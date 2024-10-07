(ns user
  {:clj-kondo/ignore [:unused-namespace :use]}
  (:require [neilyio.tasks]
            [neilyio.sound :as sound]
            [neilyio.db :as db]
            [overtone.linter]
            [neilyio.events :as events]
            [neilyio.config :as config]
            [neilyio.repl :as repl]
            [neilyio.utils :as utils]
            [clj-reload.core]
            [portal.api]
            [babashka.fs :as fs]
            [datalevin.core :as d]))

#_(comment
    (require '[clojure.java.io :as io])

    (events/handle  {:module :db :event [:select-next-source] :state repl/state})

    (db/transact! repl/state [{:loop/in 0 :loop/out 1}])
    (db/transact! repl/state [{:selected/loop 16}])
    (db/transact! repl/state [{:speaker/created-at (System/currentTimeMillis)}])
    (db/transact! repl/state [{:selected/speaker 18}])
    (db/transact! repl/state [{:selected/source 1}])

    (doseq [path (fs/list-dir "/Users/neilhansen/Desktop/test_tracks")]
      (d/transact! (d/get-conn config/db-path db/schema)
                   [{:source/label (fs/file-name path)
                     :source/bytes (fs/read-all-bytes path)
                     :source/bpm 123}]))

    (db/find-all-sources repl/state)

    (db/transact! repl/state
                  (for [source (utils/list-sources)]
                    {:source/type :audio-file :source/file-path source}))

    (db/q repl/state '[:find ?e :where [?e :selected/source]])
    (db/pull repl/state '[*] 100)
    (db/transact! repl/state
                  [[:db/retract 100 :source/type]])

    (db/q repl/state '[:find ?e :where [?e :source/type ?t]])
    (db/transact! repl/state (map #(-> [:db/retract (first %) :source/type :audio-file])
                                  (db/q repl/state '[:find ?e :where [?e :source/type ?t]])))

;; all source entities
    (for [[id] (db/q repl/state '[:find ?e
                                  :where [?e :source/type _]])]
      (db/entity repl/state id))

  ;; all speakers
    (for [[id] (db/q repl/state '[:find ?e
                                  :where [?e :speaker/created-at _]])]
      [id (db/entity repl/state id)])

  ;; select first speaker
    (for [[id] (take 1 (db/q repl/state '[:find ?e
                                          :where [?e :speaker/created-at _]]))]
      (db/transact! repl/state [{:selected/speaker id}]))

  ;; add loop to selected speaker
    (let [speaker-id (db/find-selected-speaker repl/state)]
      (db/transact! repl/state [[:db/add speaker-id :speaker/loop 33]]))

;; find selected speaker loop

    (defn find-all-loops []
      (->> (db/q repl/state '[:find ?e :where [?e :loop/source]])
           (map first)))

    (db/transact! repl/state [{:loop/in 0 :loop/out 1 :loop/source 4}])

    (find-all-loops)
    (db/find-all-sources repl/state)

    (db/context repl/state)
    (db/find-all-speakers (-> @repl/state :neilyio.db/conn d/db))
    (db/find-all-sources repl/state)

    (db/transact! repl/state
                  [{:loop/name "whole track"
                    :loop/in 0
                    :loop/out 1}])

    (db/reset-conn! repl/state)

    nil)
