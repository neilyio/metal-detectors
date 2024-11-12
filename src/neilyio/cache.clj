(ns neilyio.cache
  (:require
   [babashka.fs :as fs]
   [datascript.core :as d]
   [neilyio.events :as events]
   [overtone.live :as live]))

(def schema
  {:buffer/label {:db/unique :db.unique/identity}})

(defn bytes->sample [bytes]
  (fs/with-temp-dir [dir {}]
    (let [path (str dir "/" (random-uuid))]
      (fs/write-bytes path bytes)
      (live/sample path))))

(defn module
  "Initialize the cache system with DataScript.
   Returns a vector of [namespace handler-fn] for the events system."
  [get-conn]
  (let [cache-conn (d/create-conn schema)
        db (d/db (get-conn))
        sources (d/q '[:find [(pull ?e [:source/label :source/bytes]) ...]
                      :where [?e :source/label]]
                    db)]
    
    (println "Loading" (count sources) "sound files into memory, please wait...")
    (doseq [{:source/keys [label bytes]} sources]
      (print (str (d/q '[:find (count ?e) .
                        :where [?e :buffer/label]] 
                      @cache-conn) "... "))
      (flush)
      (d/transact! cache-conn [{:buffer/label label
                               :buffer/sample (bytes->sample bytes)}]))
    (println "Done loading buffers!")

    [:cache (fn [_] {:cache/conn cache-conn})]))

(defn get-buffer
  "Get a buffer by its label from the cache"
  [cache-conn label]
  (-> (d/q '[:find ?sample .
             :in $ ?label
             :where 
             [?e :buffer/label ?label]
             [?e :buffer/sample ?sample]]
           @cache-conn
           label)
      (or 0))) ; fallback to 0 if not found
