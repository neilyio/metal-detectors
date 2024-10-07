(ns neilyio.tasks
  "Helper functions for bb tasks. Needs to be loadable by babashka."
  (:require
   [babashka.process :refer [shell]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [neilyio.events :as events]
   [puget.printer :as puget]
   [neilyio.utils :as utils]))

(defn read-1
  "Read a single key press from a terminal, return key code as char."
  []
  (try (some-> (shell {:out :string :err nil :continue true}
                      "bash -c" "read -n1 -s c; printf %d \"'$c\"")
               :out not-empty (Integer/parseInt) char)
       (catch Throwable _))) ;; throws all kinds of pointless errors on exit

(defn ^:export read-event
  "Read a single key press from a terminal, and parse it as an event."
  []
  (let [event (-> (read-1) (events/keymap))]
    event))

(defn ^:export nc
  "Send a quoted clojure body as a string to a port with netcat."
  [port & forms]
  (let [code (apply pr-str forms)  ; Use pr-str to convert body to string
        proc (babashka.process/process {:out :string} "nc localhost" (str port))
        procin (io/writer (:in proc))]
    (binding [*out* procin]
      (println code))
    (.close procin)
    (let [result (deref proc)]
      (when-not (zero? (:exit result))
        (throw (ex-info "nc eval error" result)))
      (:out result))))

(defn ^:export print-event [out]
  (let [edn (edn/read-string out)]
    (if-not (map? edn)
      (when edn (println edn))
      (-> [(:event edn) (-> edn (dissoc :event) (utils/truncate-map))]
          (puget/cprint-str)
          (println)))))

(defn ^:export print-info [module out]
  (puget/cprint (-> (edn/read-string out)
                    (get-in [module :out]))))

(defn ^:export menu []
  (shell "
    tmux display-menu -T 'md task'

    reload r \"run-shell -c /Users/neilhansen/Documents/metal-detectors 'bb reload' \"

    "))
