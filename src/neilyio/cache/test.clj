(ns neilyio.cache.test
  (:require [expectations.clojure.test :refer [defexpect expect]]
            [neilyio.cache :as cache]
            [datascript.core :as d]
            [babashka.fs :as fs]
            [clojure.test :refer [use-fixtures]]))

;; Mock overtone.live/sample function
(def ^:dynamic *mock-sample-result* (atom nil))

(defn mock-sample [path]
  (reset! *mock-sample-result* {:path path})
  {:mock-buffer true})

(use-fixtures :each
  (fn [f]
    (with-redefs [overtone.live/sample mock-sample]
      (f))))

(defexpect test-bytes->sample
  (let [test-bytes (.getBytes "test data")
        result (cache/bytes->sample test-bytes)]
    (expect true (:mock-buffer result))
    (expect string? (:path @*mock-sample-result*))))

