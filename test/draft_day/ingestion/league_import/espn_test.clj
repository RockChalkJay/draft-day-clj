(ns draft-day.ingestion.league-import.espn-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.db :as db]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.espn :as espn]
            [draft-day.scoring :as scoring]))

;; Keys are keywords, including the numeric ones: `draft-day.json/mapper`
;; keywordizes every decoded key, so ESPN's stat ids and slot ids arrive as
;; `:16` rather than `16` or `"16"`.
(def ^:private raw
  {:id 12345
   :seasonId 2026
   :settings
   {:name "The Big Show"
    :size 12
    :scoringSettings
    {:scoringItems [{:statId 3  :points 0.04}
                    {:statId 4  :points 4.0}
                    {:statId 20 :points -2.0}
                    {:statId 24 :points 0.1}
                    {:statId 25 :points 6.0}
                    {:statId 42 :points 0.1}
                    {:statId 43 :points 6.0}
                    {:statId 53 :points 1.0 :pointsOverrides {:6 1.5}}
                    {:statId 72 :points -2.0}
                    {:statId 23 :points 0.0}
                    {:statId 89 :points 5.0}
                    {:statId 101 :points 6.0}
                    {:statId 127 :points -0.01}]}
    :rosterSettings
    {:lineupSlotCounts {:0 1 :2 2 :3 0 :4 2 :6 1 :16 1 :17 1 :20 6 :21 1 :23 1}}
    :acquisitionSettings {:isUsingAcquisitionBudget true :acquisitionBudget 100}
    :draftSettings {:type "AUCTION" :auctionBudget 200}}})

(defn- imported [] (league-import/normalize-league :espn raw))

(deftest every-mapped-stat-id-is-a-key-the-app-can-actually-score
  (doseq [[id k] espn/stat-ids]
    (is (contains? (set scoring/stat-keys) k)
        (str "statId " id " maps to " k ", which no scoring config holds"))))

(deftest rushing-yards-is-24-not-23
  ;; The band 23/24/25 is attempts/yards/TDs. Read in the wrong order it prices
  ;; every back on carries, at about a tenth of what he is worth, silently.
  (is (= :rush_yd (espn/stat-ids 24)))
  (is (nil? (espn/stat-ids 23)) "attempts are not a stat the app scores"))

(deftest the-scoring-config-carries-the-rules-the-app-can-score
  (let [s (:scoring (imported))]
    (is (= 0.04 (:pass_yd s)))
    (is (= 4.0 (:pass_td s)))
    (is (= -2.0 (:pass_int s)))
    (is (= 1.0 (:rec s)) "the base weight of an overridden rule still counts")
    (is (not (contains? s :def_td))
        "ESPN splits defensive touchdowns across several ids; one flat weight
         summed from them would be a number nobody set")))

(deftest a-position-specific-rule-is-reported-even-though-its-stat-maps
  ;; The case a select-keys would miss. ESPN writes a TE reception premium as a
  ;; per-position override on an ordinary receptions rule, so keeping the base
  ;; weight yields a config that looks complete and scores differently.
  (let [u (:unsupported-scoring (imported))]
    (is (some #(re-find #"position-specific" %) u))
    (is (= 1.0 (:rec (:scoring (imported)))))))

(deftest an-unscorable-rule-is-named-not-numbered
  (let [u (set (:unsupported-scoring (imported)))]
    (is (contains? u "points allowed 0"))
    (is (contains? u "kickoff return TD"))
    (is (contains? u "yards allowed"))
    (is (not (contains? u "rushing attempts"))
        "a rule set to zero costs the league nothing and is not worth reporting")))

(deftest an-unnamed-rule-still-says-which-one-it-was
  (is (= ["ESPN stat 991"] (espn/unsupported-scoring [{:statId 991 :points 3.0}]))))

(deftest the-roster-config-pools-every-flex-and-benches-what-it-cannot-express
  (is (= {:qb 1 :rb 2 :wr 2 :te 1 :flex 1 :k 1 :dst 1 :bench 6}
         (:roster (imported))))
  (testing "an IDP seat is bench depth, which is what it is"
    (is (= 7 (:bench (espn/roster-config {:0 1 :2 2 :4 2 :6 1 :16 1 :17 1
                                          :20 6 :23 1 :10 1}))))))

(deftest ir-counts-as-neither-a-starter-nor-the-bench
  ;; roster-size is the one number `waiver/drop-candidate` asks to decide
  ;; whether a claim costs a drop, and a parked seat inflates it.
  (let [with-ir    (espn/roster-config {:0 1 :20 6 :21 2})
        without-ir (espn/roster-config {:0 1 :20 6})]
    (is (= with-ir without-ir))
    (is (= 6 (:bench with-ir)))))

(deftest the-seats-land-in-the-vocabulary-the-lineup-speaks
  (let [seats (espn/roster-positions (get-in raw [:settings :rosterSettings :lineupSlotCounts]))]
    (is (= ["QB" "RB" "RB" "WR" "WR" "TE" "DST" "K"
            "BENCH" "BENCH" "BENCH" "BENCH" "BENCH" "BENCH" "IR" "FLEX"]
           seats))
    (doseq [s (remove db/held-slots seats)]
      (is (or (contains? (set db/positions) s) (contains? db/flex-slots s))
          (str s " is a seat nothing downstream can fill")))))

(deftest a-slot-set-to-zero-is-not-a-seat
  (is (not (some #{"WRRB_FLEX"} (espn/roster-positions {:3 0 :0 1})))))

(deftest a-swid-keeps-its-braces-whichever-way-it-was-pasted
  ;; ESPN's `owners` array carries them. A cookie authenticates either way, so
  ;; a braceless SWID matches no team and the manager's own roster silently
  ;; reads as somebody else's.
  (is (= "{ABC-123}" (espn/normalize-swid "{ABC-123}")))
  (is (= "{ABC-123}" (espn/normalize-swid "ABC-123")))
  (is (= "{ABC-123}" (espn/normalize-swid "  ABC-123  ")))
  (is (nil? (espn/normalize-swid ""))))

(deftest a-rejected-cookie-is-not-an-upstream-outage
  ;; 401 and 403 are different instructions to the manager: reconnect, versus
  ;; this league is not yours. Collapsed, one of them sends him to re-paste a
  ;; cookie that was fine.
  (is (= 401 (first (espn/status-error 401))))
  (is (= 403 (first (espn/status-error 403))))
  (is (= 404 (first (espn/status-error 404))))
  (is (= 502 (first (espn/status-error 500)))))

(deftest the-waiver-budget-is-read-from-the-league-not-guessed
  (is (= {:type :faab :budget 100} (espn/waiver-settings raw)))
  (is (= :rolling (:type (espn/waiver-settings {}))))
  (is (= 0 (:budget (espn/waiver-settings {})))
      "an absent budget is 0, so no share rule divides by nobody's number"))

(deftest the-import-names-the-league-and-its-season
  (let [c (imported)]
    (is (= "The Big Show" (:name c)))
    (is (= "2026" (:season c)) "a string, as every other provider's season is")
    (is (= 12 (:num-teams c)))))

(deftest an-auction-league-brings-its-budget
  (is (= 200 (:starting-bankroll (imported))))
  (testing "ESPN fills auctionBudget in a snake league too, so the type decides"
    (is (nil? (espn/auction-budget
               (assoc-in raw [:settings :draftSettings :type] "SNAKE")))))
  (is (nil? (espn/auction-budget {})) "and a league that says nothing gets nil"))
