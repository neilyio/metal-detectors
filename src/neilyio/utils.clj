(ns neilyio.utils
  (:require
   [clojure.edn :as edn]
   [clojure.java.io]
   [clojure.walk]
   [babashka.fs :as fs]
   [neilyio.events :as events]))

(defn clamp [n]
  (-> n (min 1) (max 0)))

(defn pr-edn [x]
  (try (let [s (pr-str x)]
         (edn/read-string s)
         s)
       (catch Throwable _
         (pr-str (str x)))))

(defn list-sources []
  (->> (str (System/getProperty "user.dir") "/resources/sources")
       (fs/list-dir)
       (map #(fs/relativize (System/getProperty "user.dir") %))
       (map str)))

(defn truncate-4 [num]
  (/ (Math/floor (* num 10000.0)) 10000.0))

(defn truncate-map [m]
  (clojure.walk/postwalk
   (fn [x]
     (if (number? x)
       (truncate-4 x)
       x))
   m))

(defmethod events/handle [:utils :list-sources] [_]
  {:sources (list-sources)})
