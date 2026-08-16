(ns draft-day.ingestion.parallel-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.parallel :as parallel])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(deftest tasks-run-concurrently-and-keep-their-keys
  ;; Each task parks until every task has arrived, so anything evaluated in turn
  ;; never gets past the first wait.
  (let [latch (CountDownLatch. 4)
        task  (fn [k] (fn [] (.countDown latch)
                        (when (.await latch 10 TimeUnit/SECONDS) k)))]
    (is (= {:a :a :b :b :c :c :d :d}
           (parallel/all (into {} (map (juxt identity task)) [:a :b :c :d]))))))

(deftest an-empty-task-map-is-not-a-deadlock
  (is (= {} (parallel/all {}))))

(deftest a-throwing-task-propagates-and-cancels-the-rest
  ;; Thunks are wrapped by the caller (`pipeline/best-effort`), so a throw
  ;; reaching here is the abnormal path — an Error, or a bug. What must not
  ;; happen is the siblings outliving the call: on a cold load that is six
  ;; scrapes still holding response buffers with nobody left to read them.
  (let [started    (CountDownLatch. 1)
        cancelled? (promise)]
    (testing "the throw is not swallowed"
      ;; array-map so the deref order is the insertion order: :boom is awaited
      ;; first, and only after :slow has confirmed it is actually running.
      (is (thrown? Exception
                   (parallel/all
                    (array-map
                     :boom (fn []
                             (.await started 10 TimeUnit/SECONDS)
                             (throw (ex-info "boom" {})))
                     :slow (fn []
                             (.countDown started)
                             (try (Thread/sleep 30000)
                                  (deliver cancelled? false)
                                  (catch InterruptedException _
                                    (deliver cancelled? true)))))))))
    (testing "the sibling is interrupted rather than left running"
      (is (true? (deref cancelled? 10000 :never-finished))))))
