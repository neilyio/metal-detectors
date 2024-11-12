(ns neilyio.cache
  (:require
   [babashka.fs :as fs]
   [datascript.core :as d]
   [neilyio.events :as events]
   [overtone.live :as live]))

(defn bytes->sample [bytes]
  (fs/with-temp-dir [dir {}]
    (let [path (str dir "/" (random-uuid))]
      (fs/write-bytes path bytes)
      (live/sample path))))

(defn module
  "Initialize the cache system with DataScript.
   Returns a vector of [namespace handler-fn] for the events system."
  [get-conn]
  (let [cache-conn (d/create-conn {})
        db (d/db (get-conn))
        sources (d/q '[:find [(pull ?e [*]) ...]
                      :where [?e :source/bytes]]
                    db)]
    
    (println "Loading" (count sources) "sound files into memory, please wait...")
    (doseq [{:db/keys [id] :source/keys [bytes]} sources]
      (print (str (d/q '[:find (count ?e) .
                        :where [?e :buffer/data]] 
                      @cache-conn) "... "))
      (flush)
      (d/transact! cache-conn [{:buffer/data (bytes->sample bytes)
                               :buffer/source id}]))
    (println "Done loading buffers!")

    [:cache (fn [_] {:cache/conn cache-conn})]))

(defn get-buffer
  "Get a buffer by its source id from the cache"
  [cache-conn source-id]
  (-> (d/q '[:find ?buffer .
             :in $ ?source-id
             :where 
             [?e :buffer/source ?source-id]
             [?e :buffer/data ?buffer]]
           @cache-conn
           source-id)
      (or 0))) ; fallback to 0 if not found
