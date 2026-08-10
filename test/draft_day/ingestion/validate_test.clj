(ns draft-day.ingestion.validate-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.validate :as validate]))

(deftest valid-id-requires-something-to-key-on
  (is (validate/valid-id? "4984"))
  (is (validate/valid-id? "ARI"))
  (testing "numbers are acceptable — only blankness disqualifies"
    (is (validate/valid-id? 4984)))
  (testing "nil, empty and whitespace-only are not keys"
    (is (not (validate/valid-id? nil)))
    (is (not (validate/valid-id? "")))
    (is (not (validate/valid-id? "   ")))))

(deftest validate-universe-drops-unusable-rows
  (testing "a clean universe passes through untouched"
    (let [players [{:player-id "1"} {:player-id "2"}]
          {:keys [players report]} (validate/validate-universe players)]
      (is (= [{:player-id "1"} {:player-id "2"}] players))
      (is (= 2 (:n report)))
      (is (= 2 (:kept report)))
      (is (zero? (:dropped-blank-id report)))
      (is (zero? (:dropped-duplicate report)))))

  (testing "blank ids are dropped and named, so the cause is findable"
    (let [{:keys [players report]}
          (validate/validate-universe [{:player-id "1" :player-name "Keep"}
                                       {:player-id nil :player-name "Nil Guy"}
                                       {:player-id "" :player-name "Blank Guy"}])]
      (is (= ["1"] (mapv :player-id players)))
      (is (= 2 (:dropped-blank-id report)))
      (is (= ["Nil Guy" "Blank Guy"] (:blank-id-names report)))))

  (testing "among duplicates the first occurrence wins"
    (let [{:keys [players report]}
          (validate/validate-universe [{:player-id "1" :player-name "First"}
                                       {:player-id "1" :player-name "Shadow"}
                                       {:player-id "2" :player-name "Other"}])]
      (is (= ["First" "Other"] (mapv :player-name players)))
      (is (= 1 (:dropped-duplicate report)))
      (is (= ["1"] (:duplicate-ids report)))))

  (testing "an empty universe is reported, not an error"
    (let [{:keys [players report]} (validate/validate-universe [])]
      (is (= [] players))
      (is (= 0 (:n report)))
      (is (= 0 (:kept report)))))

  (testing "samples are capped so a broken feed cannot flood the log"
    (let [rows (mapv (fn [i] {:player-id nil :player-name (str i)}) (range 50))
          {:keys [report]} (validate/validate-universe rows)]
      (is (= 50 (:dropped-blank-id report)))
      (is (= validate/sample-limit (count (:blank-id-names report)))))))

(deftest dropped-rate-is-a-share-of-the-input
  (is (= 0.0 (validate/dropped-rate {:n 0 :kept 0})))
  (is (= 0.0 (validate/dropped-rate {:n 100 :kept 100})))
  (is (= 0.5 (validate/dropped-rate {:n 100 :kept 50}))))

(deftest systemic-failure-separates-a-bad-row-from-a-bad-feed
  (let [thresholds {:min-kept 100 :max-dropped-rate 0.01}]
    (testing "a healthy universe losing one row in a thousand is fine"
      (is (not (validate/systemic-failure? {:n 1000 :kept 999} thresholds))))

    (testing "losing a tenth of the feed is not"
      (is (validate/systemic-failure? {:n 1000 :kept 900} thresholds)))

    (testing "a suspiciously small universe trips the floor even if all kept"
      (is (validate/systemic-failure? {:n 40 :kept 40} thresholds)))

    (testing "an empty universe trips it — it would render as a blank board"
      (is (validate/systemic-failure? {:n 0 :kept 0} thresholds))))

  (testing "the single-arity form uses the shipped thresholds"
    (is (not (validate/systemic-failure? {:n 1000 :kept 1000})))
    (is (validate/systemic-failure? {:n 10 :kept 10}))))
