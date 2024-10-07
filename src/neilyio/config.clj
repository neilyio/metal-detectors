(ns neilyio.config
  "Configuration data. Needs to be loadable by babashka.")

(def db-path "resources/db")

(def repl-port 31993)
(def data-port 31994)

(def ascii-logo
  (slurp "resources/logo.txt"))



