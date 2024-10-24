(ns neilyio.server
  (:require [clojure.data.json :as json]
            [clojure.core.async :refer [chan go-loop <! >! put!]])
  (:import [java.net DatagramSocket DatagramPacket InetAddress]))

;; Atom to store UUID -> data mapping (storing RSSI, distance, etc.)
(def uuid-data-map (atom {}))

;; Create a core.async channel for UDP packets
(def udp-channel (chan 100))

(defn create-socket [port]
  (DatagramSocket. port))

(defn receive-packet [socket buffer-size]
  (let [buffer (byte-array buffer-size)
        packet (DatagramPacket. buffer buffer-size)]
    (.receive socket packet)
    packet))

(defn packet-data [packet]
  (String. (.getData packet) 0 (.getLength packet)))

(defn process-json-data [json-string]
  (try
    (let [data (json/read-str json-string)
          uuid (get data "uuid")
          rssi-raw (get data "rssi_raw")
          rssi-filtered (get data "rssi_filtered")
          distance-raw (get data "distance_raw")
          distance-filtered (get data "distance_filtered")
          tx-power (get data "tx_power")]

      ;; Ensure all necessary fields are present
      (when (and uuid rssi-raw rssi-filtered distance-raw distance-filtered tx-power)
        ;; Update the atom with the new data
        (swap! uuid-data-map assoc uuid
               {:rssi-raw rssi-raw
                :rssi-filtered rssi-filtered
                :distance-raw distance-raw
                :distance-filtered distance-filtered
                :tx-power tx-power})

        ;; Print the updated map to show the new values
        (println "Updated UUID-data map:" @uuid-data-map)))
    (catch Exception e
      (println "Error processing JSON:" (.getMessage e)))))

(defn start-listening [port]
  "Creates a UDP socket, listens for incoming packets, and puts them on the core.async channel."
  (let [socket (create-socket port)]
    (println "UDP Server listening on port" port)
    (go-loop []
      (let [packet (receive-packet socket 1024)
            data (packet-data packet)
            sender-address (.getAddress packet)
            sender-port (.getPort packet)]
        (println (str "Received from " sender-address ":" sender-port ": " data))
        ;; Put received data into the channel
        (put! udp-channel data)
        ;; Continue listening for the next packet
        (recur)))))

(defn start-processing []
  "Starts a core.async loop that processes messages from the channel."
  (go-loop []
    (let [json-string (<! udp-channel)]
      ;; Process the JSON data received from the channel
      (process-json-data json-string))
    ;; Continue processing the next message
    (recur)))

(defn run-server
  "Runs a UDP server on the specified port. If no port is provided, uses the default port 5000."
  ([] (run-server 5000))
  ([port]
   ;; Start the listening and processing functions
   (start-listening port)
   (start-processing)

   ;; Block the main thread so it doesn't exit immediately
   (println "Press Enter to stop the server.")
   (read-line))) ;; <-- This will block and wait for user input, keeping the server alive

(defn -main
  "Starts the UDP server. Can optionally provide a port number as a command-line argument."
  [& args]
  (if (seq args)
    (run-server (Integer/parseInt (first args)))
    (run-server)))
