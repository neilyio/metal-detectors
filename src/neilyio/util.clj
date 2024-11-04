(ns neilyio.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-uuid-name-map [file-path]
  "Loads the UUID-to-name mapping from a CSV file and returns it as a map."
  (with-open [reader (io/reader file-path)]
    (into {} (for [line (line-seq reader)]
               (let [[uuid name] (str/split line #",")]
                 [uuid name])))))
