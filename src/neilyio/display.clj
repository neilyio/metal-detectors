(ns neilyio.display
  (:require [clojure.core.async :refer [<! go-loop]]
            [clojure.data.json :as json]
            [neilyio.util :refer [load-uuid-name-map]]))

(def DEBUG (Boolean/parseBoolean (System/getenv "DEBUG"))) ; Set DEBUG=true in environment to enable debugging
;; Or set it directly in code
;(def DEBUG true)

(def uuid-name-map (load-uuid-name-map "resources/uuid-to-device.csv"))

(defn debug-log [& messages]
  (when DEBUG
    (apply println messages)))



;; Atom to store the latest data per device UUID
(def device-data (atom {}))
(defn display-loop [data-channel]
  "Continuously reads from the channel and displays UUID, device name, RSSI, and distance on one line per device."
  (println "Current Node Data (UUID, Device Name, RSSI, Distance):")
  (go-loop []
    (if-let [data (<! data-channel)]
      (do
        ;; Parse JSON data string into Clojure map
        (let [parsed-data (json/read-str data :key-fn keyword)
              uuid (:uuid parsed-data)
              rssi (:rssi_raw parsed-data)
              distance (:distance_filtered parsed-data)
              device-name (get uuid-name-map uuid "NONE")]
          ;; Update device data in atom
          (swap! device-data assoc uuid {:name device-name :rssi rssi :distance distance})

          ;; Clear screen and re-display all device data on one line per device
          (println "\033[H\033[2J") ; Clear the screen (ANSI escape code)
          (println "Current Node Data (UUID, Device Name, RSSI, Distance):")
          (doseq [[uuid {:keys [name rssi distance]}] @device-data]
            (println (format "UUID: %s  |  Device Name: %s  |  RSSI: %s  |  Distance: %.2f meters"
                             uuid name rssi (or distance "N/A")))))
        (recur))
      (println "Data channel closed."))))
