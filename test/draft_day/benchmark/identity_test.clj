(ns draft-day.benchmark.identity-test
  "Name collisions silently resolved to the WRONG player, who then had no
  nflverse outcome row and scored zero realized points — penalising whichever
  model ranked the real player highly. Three productive 2023 receivers were
  affected. Each failure mode is pinned here."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.sources.ids :as ids]
            [draft-day.ingestion.match :as match]))

(defn row [gsis name pos year]
  {"gsis_id" gsis "name" name "position" pos "sleeper_id" "" "fantasypros_id" "NA"
   "draft_year" (str year) "draft_round" "NA" "draft_pick" "NA" "draft_ovr" "NA"
   "birthdate" "NA"})

;; Real data: three Mike Williamses, all receivers.
(def mike-williams
  [(row "00-0033536" "Mike Williams" "WR" 2017)
   (row "00-0027702" "Mike Williams" "WR" 2010)
   (row "00-0023452" "Mike Williams" "WR" 2005)])

(deftest same-name-same-position-resolves-by-era
  (let [c (ids/name-candidates-from-rows mike-williams)
        k (match/key-for "Mike Williams" "WR")]
    (is (= 3 (count (get c k))) "all three are kept as candidates")
    (testing "each season gets the receiver who was actually in the league"
      (is (= "00-0023452" (ids/pick-candidate (get c k) 2007)))
      (is (= "00-0027702" (ids/pick-candidate (get c k) 2012)))
      (is (= "00-0033536" (ids/pick-candidate (get c k) 2023))))))

(deftest a-defensive-namesake-cannot-displace-the-receiver
  ;; DynastyProcess files some players under a junk position, so those rows are
  ;; fanned out across the skill positions to stay findable. Before this, the
  ;; 2009 CORNERBACK D.J. Moore outranked the 2018 receiver and Michael Thomas
  ;; resolved to a safety.
  (let [rows [(row "00-0034827" "D.J. Moore" "WR" 2018)
              (row "00-0026907" "D.J. Moore" "CB" 2009)]
        c    (ids/name-candidates-from-rows rows)
        k    (match/key-for "D.J. Moore" "WR")]
    (is (= "00-0034827" (ids/pick-candidate (get c k) 2023)))))

(deftest era-beats-position-when-they-disagree
  ;; A player in the league that season outranks a cleaner position match from a
  ;; later era — otherwise the same zero-outcome bug returns in disguise.
  (let [rows [(row "00-0000018" "Split Name" "WR" 2018)
              (row "00-0000005" "Split Name" "XX" 2005)]
        c    (ids/name-candidates-from-rows rows)
        k    (match/key-for "Split Name" "WR")]
    (is (= "00-0000005" (ids/pick-candidate (get c k) 2010))
        "in 2010 only the 2005 player existed, junk position or not")
    (is (= "00-0000018" (ids/pick-candidate (get c k) 2020)))))

(deftest a-junk-position-player-is-still-findable
  ;; The reason fan-out exists at all: Rondale Moore is filed as "XX".
  (let [c (ids/name-candidates-from-rows [(row "00-0036936" "Rondale Moore" "XX" 2021)])]
    (is (= "00-0036936" (ids/pick-candidate (get c (match/key-for "Rondale Moore" "WR")) 2023)))
    (is (= "00-0036936" (ids/pick-candidate (get c (match/key-for "Rondale Moore" "RB")) 2023)))))

(deftest a-genuine-position-is-not-fanned-out
  (let [c (ids/name-candidates-from-rows [(row "00-0000001" "Real Receiver" "WR" 2020)])]
    (is (seq (get c (match/key-for "Real Receiver" "WR"))))
    (is (empty? (get c (match/key-for "Real Receiver" "RB"))))))

(deftest nicknames-still-resolve-after-the-candidate-rewrite
  (let [c (ids/name-candidates-from-rows [(row "00-0035662" "Marquise Brown" "WR" 2019)])]
    (is (= "00-0035662"
           (ids/pick-candidate (get c (match/key-for "Hollywood Brown" "WR")) 2023)))))

(deftest no-candidates-resolves-to-nil-rather-than-a-guess
  (is (nil? (ids/pick-candidate nil 2023)))
  (is (nil? (ids/pick-candidate [] 2023))))

(deftest a-player-drafted-after-the-season-is-still-returned-as-a-last-resort
  ;; Better to resolve to the only known bearer of the name than to drop the row
  ;; silently; the outcome lookup will simply miss and that is visible.
  (let [c (ids/name-candidates-from-rows [(row "00-0040000" "Future Guy" "WR" 2024)])]
    (is (= "00-0040000" (ids/pick-candidate (get c (match/key-for "Future Guy" "WR")) 2010)))))
