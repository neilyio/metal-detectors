(ns neilyio.repl
  (:require
   [clj-reload.core]
   [clojure.core.server]
   [datalevin.core :as d]
   [neilyio.config :as config]
   [neilyio.db :as db]
   [neilyio.events :as events]
   [neilyio.print :as print]
   [neilyio.sound :as sound]
   [neilyio.utils :as utils]
   [portal.api]
   [puget.printer :as puget]))

(defn portal-init
  "Initialize Portal if not already running.
   Returns the Portal instance."
  []
  (when (zero? (count (portal.api/sessions)))
    (let [portal (portal.api/open {:app false})]
      (add-tap (resolve 'portal.api/submit))
      portal)))

;; We want to load portal as soon as we run.
(defonce ^:export _ (portal-init))

(defn module
  "Initialize REPL module state and return an event handler function."
  []
  [:repl (fn [state] state)])

(def state (atom {}))
(def ^:dynamic conn nil)
(defn get-conn
  "Initialized a thread-local conn variable."
  []
  (when (or (nil? conn) (d/closed? conn))
    #_{:clj-kondo/ignore [:inline-def]}
    (def conn (db/get-conn)))
  conn)

(def modules [(db/module get-conn)
              (module)
              (sound/module get-conn #(swap! state merge %))
              (print/module get-conn)])

(defn dev-repl-print [out]
  (puget/cprint out))

(defn dev-data-eval [in]
  (try
    (assert in "no input to dev-data-eval")
    (into {}
          (for [[module-key module-fn] modules
                :let [out (events/handle (assoc (module-fn @state)
                                                :module module-key
                                                :event in))]]
            [module-key {:in in :out out}]))
    (catch Throwable e
      (tap> (ex-info "error handling event" {:error e :in in}))
      {:neilyio.repl/error {:error e :in in}})))

(defn dev-data-print [event-data]
  (if (::error event-data)
    (println (utils/pr-edn {:err (::error event-data)}))
    (println (-> event-data :print :out pr-str))))

;; Purposefully empty, we don't want a prompt here.
(defn dev-data-prompt [])

(defn ^:export socket-repl [& _]
  (require '[neilyio.sound])

  (clojure.core.server/start-server
   {:name "dev-data-server"
    :port config/data-port
    :server-daemon false
    :accept 'clojure.main/repl
    :args [:prompt #'dev-data-prompt
           :print #'dev-data-print
           :eval #'dev-data-eval
           :read clojure.core.server/repl-read]})

  (clojure.core.server/start-server
   {:name "dev-repl-server"
    :port config/repl-port
    :server-daemon false
    :accept 'clojure.main/repl
    :args [:print #'dev-repl-print
           :read clojure.core.server/repl-read]})
  (println config/ascii-logo)
  (println)
  (println (str "socket repl at " config/repl-port ", rock on.")))

(defmethod events/handle [:repl :reload] [_ #_{[tag] :event}]
  (tap> (clj-reload.core/reload)))

