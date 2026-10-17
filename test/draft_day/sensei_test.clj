(ns draft-day.sensei-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.sensei :as sensei]))

(def ^:private answered
  [{:role :user :text "q1"} {:role :assistant :text "a1" :tools [] :status :done}
   {:role :user :text "q2"} {:role :assistant :text "partial" :tools [] :status :stopped}
   {:role :user :text "q3"} {:role :assistant :text "" :tools [] :status :error :error "x"}])

(deftest only-answered-questions-are-resent
  (is (= [{:role "user" :text "q1"} {:role "assistant" :text "a1"}]
         (sensei/history answered))
      "a stopped or failed answer leaves with its question"))

(deftest the-request-names-the-league-and-nothing-secret
  (is (= {:message "hi" :history [] :league "sleeper:1"}
         (sensei/request-body [] "hi" "sleeper:1")))
  (is (= {:message "hi" :history []} (sensei/request-body [] "hi" nil))))

(deftest events-build-the-answer-in-progress
  (let [ms (-> (sensei/add-turn [] "q")
               (sensei/apply-event {:session-id "s"})
               (sensei/apply-event {:tool-start "my_team"})
               (sensei/apply-event {:text "Pick "})
               (sensei/apply-event {:text "Mason"})
               (sensei/apply-event {:done true}))]
    (is (= {:role :assistant :text "Pick Mason" :tools ["my_team"] :status :done}
           (peek ms)))
    (is (= {:role :user :text "q"} (first ms)))))

(deftest an-error-event-ends-the-answer-with-its-message
  (let [m (peek (sensei/apply-event (sensei/add-turn [] "q") {:error "boom"}))]
    (is (= :error (:status m)))
    (is (= "boom" (:error m)))
    (is (sensei/terminal? {:error "boom"}))
    (is (sensei/terminal? {:done true}))
    (is (not (sensei/terminal? {:text "x"})))))

(deftest stopping-keeps-what-arrived
  (let [ms (-> (sensei/add-turn [] "q") (sensei/apply-event {:text "half"})
               (sensei/fail :stopped nil))]
    (is (= {:status :stopped :text "half"} (select-keys (peek ms) [:status :text])))
    (is (sensei/retryable? ms))
    (is (= "q" (sensei/last-question ms)))))

(deftest width-stays-usable
  (is (= 300 (sensei/clamp-width 10)))
  (is (= 720 (sensei/clamp-width 5000)))
  (is (= 380 (sensei/clamp-width nil)))
  (is (= 450 (sensei/clamp-width 450))))

(deftest events-with-no-turn-are-ignored
  (is (= [] (sensei/apply-event [] {:text "x"})))
  (is (= [] (sensei/fail [] :error "x")))
  (testing "nothing to retry before a first question"
    (is (not (sensei/retryable? [])))))
