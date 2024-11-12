(ns neilyio.repl
  (:require
   [clj-reload.core]
   [clojure.core.server]
   [neilyio.config :as config]
   [neilyio.db :as db]
   [neilyio.events :as events]
   [neilyio.print :as print]
   [neilyio.sound :as sound]
   [neilyio.cache :as cache]
   [neilyio.utils :as utils]
   [portal.api]
   [puget.printer :as puget]))

;; We want to load portal as soon as we run.
;; Set up all the modules here.
(defonce ^:export _
  (do (utils/portal-init)
      (db/init!    (db/get-conn) cache/conn)
      (cache/init! (db/get-conn) cache/conn)
      (sound/init! (db/get-conn) cache/conn)
      (print/init! (db/get-conn) cache/conn)))

(defn dev-repl-print [out]
  (puget/cprint out))

(defn dev-data-eval [in]
  (try
    (assert in "no input to dev-data-eval")
    ;; Manually handle repl events.
    (into {}
          (for [[key ctx] [[:db    #'db/ctx]
                           [:cache #'cache/ctx]
                           [:sound #'sound/ctx]
                           [:print #'print/ctx]
                           [:repl  (constantly {})]]
                :let [out (-> (ctx (db/get-conn) cache/conn)
                              (assoc :module key :event in)
                              (events/handle))]]
            [key {:in in :out out}]))
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

(defmethod events/handle [:repl :reload] [_]
  (tap> (clj-reload.core/reload)))

