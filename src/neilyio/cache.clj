(ns neilyio.cache
  {:clj-kondo/config '{:linters {:unresolved-var {:exclude [overtone.live]}}}}
  (:require
   [babashka.fs :as fs]
   [datascript.core :as d]
   [datalevin.core :as dl]
   [overtone.live :as live]))

(declare bytes->sample)

(def all-sources       (partial dl/q '[:find [(pull ?e [*]) ...] :where [?e :source/bytes]]))
(def all-timelines     (partial d/q  '[:find [(pull ?e [*]) ...] :where [?e :timeline/speaker]]))
(def count-buffers     (partial d/q  '[:find (count ?e) . :where [?e :buffer/sample]]))
(def sample-by-source  (partial d/q  '[:find ?buffer . :in $ ?source-id
                                       :where [?e :buffer/source ?source-id] [?e :buffer/sample ?buffer]]))
(defn master [db]
  (->> (d/q '[:find ?e . :where [?e :master/synth]] db)
       (d/entity db)))

(def timeline-by-speaker (partial d/q '[:find (pull ?e [*]) . :in $ ?speaker-id
                                        :where [?e :timeline/speaker ?speaker-id]]))

(defn buffer! [conn id bytes]
  (let [sample (bytes->sample bytes)]
    (d/transact! conn [{:buffer/sample sample  :buffer/source id}])))
(defn time-bus! [conn bus] (d/transact! conn [{:bus/time bus}]))
(defn info-bus! [conn bus] (d/transact! conn [{:bus/info bus}]))
(defn timeline-bus! [conn bus] (d/transact! conn [{:bus/timeline bus}]))
(defn playcontrol-bus! [conn bus] (d/transact! conn [{:bus/playcontrol bus}]))
(defn timeline! [conn speaker looper looper-status looper-out]
  (d/transact! conn [{:timeline/speaker speaker
                      :timeline/looper looper
                      :timeline/looper-status looper-status
                      :timeline/looper-out looper-out}]))

(defn bytes->sample [bytes]
  (fs/with-temp-dir [dir {}]
    (let [path (str dir "/" (random-uuid))]
      (fs/write-bytes path bytes)
      (live/sample path))))

(defonce conn (d/create-conn))

(def ^:export q (comp d/q))

(def ^:export transact! d/transact!)

(defn init! [db cache]
  (let [sources (all-sources (dl/db db))]
    (println "Loading" (count sources) "sound files into memory, please wait...")
    (doseq [{:db/keys [id] :source/keys [bytes]} sources]
      (print (str (count-buffers @cache) "... "))
      (flush)
      (buffer! cache id bytes))
    (println "Done loading buffers!")))

(defn ctx [_ _])
