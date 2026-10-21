(ns draft-day.rankings.role-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.role :as role]))

(defn- log
  "A game log from `{week snaps}` against a 60-snap team."
  [weeks]
  (mapv (fn [[w s]] {:week w :stats {} :snaps (double s) :team-snaps 60.0}) (sort weeks)))

(defn- player [id name pos team weeks & {:as more}]
  (merge {:player-id id :player-name name :position pos :team team
          :realized/game-log (log weeks)}
         more))

(def achane (player "a" "De'Von Achane" "RB" "MIA" {1 50 2 45 3 4} :sleeper/injury-status "IR"))
(def gordon (player "g" "Ollie Gordon" "RB" "MIA" {1 4 2 11 3 61 4 22 5 43}))
(def wright (player "w" "Jaylen Wright" "RB" "MIA" {1 4 2 3 4 12 5 9}))

(deftest a-starter-who-left-is-a-vacancy-from-the-week-after-his-last-hold
  (is (= 3 (role/change-point achane #{1 2 3 4 5} 5)) "50 and 45 of 60, then 4")
  (is (nil? (role/change-point (assoc achane :realized/game-log (log {1 50 2 45 3 40})) #{1 2 3} 3))
      "still playing"))

(deftest the-teammate-who-gained-snaps-took-the-role
  (let [[a g w] (role/with-role [achane gordon wright] 5)]
    (is (= 3 (:role/since g)))
    (is (= "De'Von Achane" (:role/vacated-by g)))
    (is (= "IR" (:role/vacated-status g)))
    (is (not (contains? w :role/since)) "a backup whose snaps did not rise is not tagged")
    (is (not (contains? a :role/since)) "nor is the man who left")))

(deftest only-a-serious-designation-makes-a-vacancy
  (let [healthy (dissoc achane :sleeper/injury-status)
        [_ g]   (role/with-role [(assoc healthy :sleeper/injury-status "Questionable") gordon] 5)]
    (is (not (contains? g :role/since)) "a quiet stretch is not a vacancy")))

(deftest a-vacancy-belongs-to-its-team-and-position
  (let [[_ other] (role/with-role [achane (assoc gordon :team "NYJ")] 5)
        [_ wr]    (role/with-role [achane (assoc gordon :position "WR")] 5)]
    (is (not (contains? other :role/since)))
    (is (not (contains? wr :role/since)))))

(deftest snap-shares-are-over-the-weeks-played
  (let [g (first (role/with-role [gordon] 5))]
    (is (< 0.46 (:snap-share-season g) 0.48) "(4 + 11 + 61 + 22 + 43) / 5 over 60")
    (is (< 0.69 (:snap-share g) 0.71) "the last three weeks: 61, 22 and 43 over 60")))

(deftest a-board-with-no-snap-counts-gains-nothing
  (let [bare {:player-id "x" :position "RB" :team "MIA" :realized/game-log [{:week 1 :stats {}}]}]
    (is (= [bare] (role/with-role [bare] 5)))
    (is (= [bare] (role/with-role [bare] 0)) "preseason: nothing to read")))

(deftest a-teams-bye-is-not-a-zero
  (let [byed (fn [p weeks] (assoc p :realized/game-log (log weeks)))
        ;; MIA plays weeks 1-3 and 5-6; week 4 is its bye, and no one has an entry for it.
        a (byed achane {1 50 2 45 3 4 5 2 6 1})
        g (byed gordon {1 4 2 11 3 30 5 12 6 8})
        [_ out] (role/with-role [a g] 6)]
    (is (= 3 (:role/since out)))
    (is (some? (:role/since out))
        "(30 + 12 + 8) / 3 weeks played over 60 is 28%; counting the bye as a zero it would be 21%")))
