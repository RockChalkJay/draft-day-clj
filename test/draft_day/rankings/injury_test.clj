(ns draft-day.rankings.injury-test
  "The scale's job is to be *right about who it has no opinion about* at least as
  much as to rank the fragile. Most of what follows pins the nil cases."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.injury :as injury]))

(def ^:private lengths
  "What ingestion fetched: three 17-game seasons."
  {2023 17 2024 17 2025 17})

(defn- player
  ([years-exp games] (player years-exp games nil lengths))
  ([years-exp games status] (player years-exp games status lengths))
  ([years-exp games status season-lengths]
   {:sleeper/years-exp        years-exp
    :sleeper/injury-status    status
    :nflverse/games-by-season games
    :nflverse/games-seasons   season-lengths}))

(defn- level [& args] (:injury-risk (injury/risk-for (apply player args))))

(deftest bands-rank-real-players-the-way-a-manager-would
  ;; Real games-played lines from 2023-25. If these ever reorder, the scale has
  ;; stopped meaning what its docstring says it means.
  (testing "three full seasons is the floor of the scale"
    (is (= 1 (level 3 {2023 17.0 2024 17.0 2025 17.0}))))
  (testing "a game or two a year is still comfortably durable"
    (is (= 2 (level 5 {2023 16.0 2024 17.0 2025 16.0}))))
  (testing "five missed games across three years is the middle"
    (is (= 3 (level 9 {2023 14.0 2024 16.0 2025 16.0}))))
  (testing "one lost half-season pushes into fragile"
    (is (= 4 (level 9 {2023 16.0 2024 4.0 2025 17.0}))))
  (testing "chronically unavailable tops out"
    (is (= 5 (level 6 {2023 1.0 2024 13.0 2025 10.0})))))

(deftest band-edges-are-inclusive-at-the-threshold
  ;; The thresholds are "games missed per season"; a player sitting exactly on
  ;; one belongs to the worse band, so the bands tile without a gap.
  (is (= [1 1 2 2 3 3 4 4 5 5]
         (mapv injury/band [0.0 0.49 0.5 1.49 1.5 2.99 3.0 4.99 5.0 20.0]))))

(deftest a-rookie-has-no-history-and-therefore-no-score
  ;; The trap this whole namespace is shaped around: scored over a flat window a
  ;; rookie looks like he missed every game of the seasons before he existed, and
  ;; last year's rookie class comes out as the most fragile players in football.
  (testing "a true rookie gets nil, not 1 and not 5"
    (is (nil? (level 0 {}))))
  (testing "last year's rookie is judged on last year alone"
    (is (= 1 (level 1 {2025 17.0})))
    (is (= 1 (:injury/seasons (injury/risk-for (player 1 {2025 17.0})))))))

(deftest the-denominator-is-years-in-the-league-not-the-window-width
  (testing "a two-year player is averaged over two seasons, not three"
    (let [r (injury/risk-for (player 2 {2024 15.0 2025 4.0}))]
      (is (= 2 (:injury/seasons r)))
      (is (= 15.0 (:injury/games-missed r)))
      (is (= 5 (:injury-risk r)) "7.5 a season, not 5.0 spread over a phantom third")))
  (testing "a veteran is capped at the fetched window, not his whole career"
    (is (= [2023 2024 2025] (injury/window lengths 12)))))

(deftest a-season-the-network-lost-is-not-charged-to-anybody
  ;; Ingestion records which seasons it actually fetched. If a release 404s, the
  ;; window narrows; the alternative is every player in the league silently
  ;; wearing seventeen missed games.
  (is (= [2024 2025] (injury/window {2024 17 2025 17} 9)))
  (is (= 1 (level 9 {2024 17.0 2025 17.0} nil {2024 17 2025 17}))))

(deftest a-season-with-no-row-is-a-season-missed
  ;; ...but only for a player who has *some* row in the window. Otherwise a
  ;; practice-squad body who never took a snap would read as maximally fragile.
  (testing "a starter who lost a year is charged for it"
    (is (= 5 (level 3 {2023 17.0 2025 17.0}))))
  (testing "a player with no rows at all draws no conclusion"
    (is (nil? (level 5 {})))))

(deftest a-serious-designation-floors-the-scale
  ;; The Inj column is off by default, so without this floor a player on
  ;; season-ending IR could show a durable bar and nothing on screen to say
  ;; otherwise.
  (testing "IR overrides a spotless history"
    (is (= 5 (level 3 {2023 17.0 2024 17.0 2025 17.0} "IR"))))
  (testing "matching is case- and whitespace-insensitive, as Sleeper is not"
    (is (= 5 (level 3 {2023 17.0 2024 17.0 2025 17.0} "  ir "))))
  (testing "a suspension counts — games lost are games lost"
    (is (= 5 (level 3 {2023 17.0 2024 17.0 2025 17.0} "Sus"))))
  (testing "it floors rather than replaces: history can never be lowered by it"
    (is (= 5 (level 6 {2023 1.0 2024 13.0 2025 10.0} "IR"))))
  (testing "a designation alone scores a player the history cannot reach"
    (is (= 5 (level 0 {} "IR"))))
  (testing "Questionable is August noise and must not move a durability score"
    (is (= 1 (level 3 {2023 17.0 2024 17.0 2025 17.0} "Questionable")))
    (is (= 1 (level 3 {2023 17.0 2024 17.0 2025 17.0} "Doubtful"))))

  (testing "Out is a gameday call, not a duration, and does not floor either"
    ;; Floored on it, this player read "Risk 5 of 5 — Out, 0.0 games missed per
    ;; season over 3 seasons": a cell contradicting the evidence it cites.
    (is (= 1 (level 3 {2023 17.0 2024 17.0 2025 17.0} "Out")))
    (let [r (injury/risk-for (player 3 {2023 17.0 2024 17.0 2025 17.0} "Out"))]
      (is (re-find #"Risk 1 of 5" (:injury/reason r)))
      (is (not (re-find #"Out" (:injury/reason r))))))

  (testing "the COVID list is defunct and no longer floors"
    (is (= 1 (level 3 {2023 17.0 2024 17.0 2025 17.0} "COV")))))

(deftest games-are-clamped-to-the-season-they-were-played-in
  ;; The 2025 file carries an 18 for at least one player. Unclamped he earns a
  ;; negative missed-game total that averages away somebody else's real absence.
  (is (= 1 (level 3 {2023 18.0 2024 17.0 2025 17.0})))
  (is (= 0.0 (:injury/games-missed (injury/risk-for (player 3 {2023 18.0 2024 17.0 2025 17.0}))))))

(deftest the-reason-is-the-cells-only-text
  ;; The board renders the level as a bar with no number in it, so this string is
  ;; the whole of what a hover or a screen reader gets.
  (let [r (injury/risk-for (player 9 {2023 16.0 2024 4.0 2025 17.0}))]
    (is (re-find #"Risk 4 of 5" (:injury/reason r)))
    (is (re-find #"4\.7 games missed per season over 3 seasons" (:injury/reason r))))
  (testing "a floored player is told which designation did it"
    (is (re-find #"IR" (:injury/reason (injury/risk-for (player 3 {2025 17.0} "IR"))))))
  (testing "one season is not \"1 seasons\""
    (is (re-find #"over 1 season\b" (:injury/reason (injury/risk-for (player 1 {2025 15.0})))))))

(deftest with-injury-risk-leaves-the-unopinionated-alone
  ;; An absent key renders as the board's dash. A 0, or a level 1, would both be
  ;; claims the evidence does not support.
  (let [[rookie vet] (injury/with-injury-risk
                       [(player 0 {}) (player 3 {2023 17.0 2024 17.0 2025 17.0})])]
    (is (not (contains? rookie :injury-risk)))
    (is (= 1 (:injury-risk vet)))))
