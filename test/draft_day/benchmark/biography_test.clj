(ns draft-day.benchmark.biography-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.sources.ids :as ids]
            [draft-day.ingestion.player-ids :as player-ids]
            [draft-day.benchmark.vintage :as vintage]))

(defn dp-row [gsis {:keys [year round pick ovr birth]}]
  {"gsis_id" gsis "name" "A Player" "position" "RB" "sleeper_id" "1"
   "fantasypros_id" "NA"
   "draft_year"  (or year "NA")
   "draft_round" (or round "NA")
   "draft_pick"  (or pick "NA")
   "draft_ovr"   (or ovr "NA")
   "birthdate"   (or birth "NA")})

(deftest biography-parses-draft-capital-and-birth-year
  (let [b (player-ids/biography-from-rows
           [(dp-row "00-1" {:year "2023" :round "1" :pick "8" :ovr "8" :birth "2002-01-11"})])]
    (is (= {:draft-year 2023 :draft-round 1 :draft-pick 8 :draft-overall 8 :birth-year 2002}
           (get b "00-1")))))

(deftest biography-treats-NA-as-absent
  ;; DynastyProcess writes the literal string "NA"; counting it as present would
  ;; produce draft capital of 0, which reads as the first overall pick.
  (let [b (player-ids/biography-from-rows [(dp-row "00-2" {})])]
    (is (nil? (get b "00-2")))))

(deftest biography-keeps-partial-records
  (let [b (player-ids/biography-from-rows [(dp-row "00-3" {:birth "1995-06-02"})])]
    (is (= {:birth-year 1995} (get b "00-3")))
    (is (nil? (:draft-overall (get b "00-3"))))))

(deftest rookie-is-derived-per-season-not-stored
  ;; The same player is a rookie in exactly one season; storing a flag would make
  ;; the row wrong for every other season it appears in.
  (let [bio {:draft-year 2023 :draft-overall 8 :birth-year 2002}
        r23 (vintage/with-biography {:gsis-id "00-1"} 2023 bio)
        r24 (vintage/with-biography {:gsis-id "00-1"} 2024 bio)]
    (is (true? (:rookie? r23)))
    (is (false? (:rookie? r24)))))

(deftest age-is-computed-against-the-season-being-scored
  (let [bio {:birth-year 2002}]
    (is (= 21 (:age (vintage/with-biography {} 2023 bio))))
    (is (= 23 (:age (vintage/with-biography {} 2025 bio))))))

(deftest a-player-with-no-biography-is-left-untouched
  (let [row {:gsis-id "00-9" :position "WR"}]
    (is (= row (vintage/with-biography row 2023 nil)))
    (testing "no :rookie? key is invented, so 'unknown' stays distinct from 'veteran'"
      (is (not (contains? (vintage/with-biography row 2023 nil) :rookie?))))))

(deftest attach-biography-preserves-every-player
  (with-redefs [ids/biography (constantly {"00-1" {:draft-overall 8 :draft-year 2023}})]
    (let [ps  [{:gsis-id "00-1"} {:gsis-id "00-2"}]
          out (vintage/attach-biography ps 2023)]
      (is (= 2 (count out)))
      (is (= 8 (:draft/overall (first out))))
      (is (nil? (:draft/overall (second out)))))))
