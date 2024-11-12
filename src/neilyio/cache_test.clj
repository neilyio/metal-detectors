(ns neilyio.cache-test
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

(defexpect test-module
  (let [test-conn (d/create-conn {})
        ;; Add some test data to the connection
        _ (d/transact! test-conn [{:db/id 1
                                  :source/bytes (.getBytes "test audio 1")}
                                 {:db/id 2
                                  :source/bytes (.getBytes "test audio 2")}])
        [namespace handler] (cache/module (constantly test-conn))
        result (handler nil)
        cache-conn (:cache/conn result)]
    
    ;; Test that the module returns correct namespace
    (expect :cache namespace)
    
    ;; Test that cache connection contains our test buffers
    (expect 2 (count (d/q '[:find ?e
                           :where [?e :buffer/data]]
                         @cache-conn)))
    
    ;; Test get-buffer functionality
    (let [buffer1 (cache/get-buffer cache-conn 1)
          buffer2 (cache/get-buffer cache-conn 2)]
      (expect true (:mock-buffer buffer1))
      (expect true (:mock-buffer buffer2)))))
