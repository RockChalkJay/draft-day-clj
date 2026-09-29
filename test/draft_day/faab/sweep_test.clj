(ns draft-day.faab.sweep-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.faab.sweep :as sweep]))

(deftest log-loss-scores-the-outcome-that-happened
  (is (< (Math/abs (- (Math/log 2.0) (sweep/log-loss 0.5 1.0))) 1e-12))
  (is (< (sweep/log-loss 0.9 1.0) (sweep/log-loss 0.9 0.0)))
  (testing "a certain chance that misses is bounded, not infinite"
    (is (< (sweep/log-loss 1.0 0.0) 10.0))))

(deftest value-gain-is-saving-when-kept-and-a-cost-when-lost
  (is (= 7.0 (sweep/value-gain {:value 5 :amount 12 :top 4})) "still beats the top rival")
  (is (= 12.0 (sweep/value-gain {:value 0 :amount 12 :top nil})) "nobody else bid")
  (is (= (- sweep/lost-claim) (sweep/value-gain {:value 3 :amount 12 :top 4})))
  (is (= (- (* 0.5 8) (* 0.5 sweep/lost-claim))
         (sweep/value-gain {:value 4 :amount 12 :top 4}))
      "a tie is half of each"))

(defn- near? [a b] (< (Math/abs (- (double a) (double b))) 1e-12))

(def ^:private row {:league-id "L" :stratum {:season "2025"}})

(def ^:private run
  {:who  [{:week 1 :p 0.5 :bid? true} {:week 1 :p 0.1 :bid? false} {:week 2 :p 0.2 :bid? false}]
   :bids [{:week 1 :won? true :p-win 0.8 :p-none 0.6 :top 4 :top-range [3 9] :value 5 :walk-away 9 :amount 12}
          {:week 2 :won? false :p-win 0.3 :p-none 0.4 :top nil :top-range nil}]})

(deftest a-league-is-summed-by-league-and-by-season-week
  (let [{:keys [by-league by-week]} (sweep/league-sums row run)
        l (by-league "L")]
    (is (= #{["2025" 1] ["2025" 2]} (set (keys by-week))) "weeks are keyed by season, which shares its news")
    (is (= [3 2 2] [(:who-n l) (:win-n l) (get-in by-week [["2025" 1] :who-n])]))
    (is (near? 0.8 (:who-p l)))
    (is (= 1.0 (:who-y l)))
    (is (= [1 0.0 1.0] [(:contest-n l) (:p50 l) (:p90 l)]) "a top rival bid of 4: over the p50 of 3, under the p90 of 9")
    (is (= [1 7.0 1.0] [(:gain-n l) (:gain l) (:kept l)]) "a winner the value bid kept at 5 against his 12")
    (is (= 1.0 (:none-y l)) "one of the two bids had nobody else")))

(deftest the-summary-divides-the-sums
  (let [s (sweep/summary (sweep/league-sums row run))]
    (is (near? (/ 0.8 3) (:who-p s)))
    (is (near? 0.5 (:none-y s)))
    (is (near? 0.5 (:none-p s)))
    (is (= [1 2 1] [(:leagues s) (:bids s) (:winners s)]))
    (is (nil? (:p50 (sweep/summary {:by-league {"L" {:win-n 1}}}))) "no contested bid, no coverage")))

(deftest ratio-stat-pools-over-counts-not-blocks
  (is (= 0.25 ((sweep/ratio-stat :d :n) [{:d 1.0 :n 1} {:d 0.0 :n 3}]))
      "a block of three outweighs a block of one")
  (is (= 0.0 ((sweep/ratio-stat :d :n) [])) "nothing to pool"))

(deftest paired-rows-difference-the-same-blocks
  (let [[r] (sweep/paired-rows {"L" {:who-n 10 :who-ll 2.0 :win-n 2 :win-ll 1.5 :gain-n 1 :gain 3.0}}
                               {"L" {:who-n 10 :who-ll 1.5 :win-n 2 :win-ll 1.3 :gain-n 1 :gain 4.0}})]
    (is (= "L" (:season r)) "the bootstrap's block")
    (is (= [10 -0.5] [(:who-n r) (:who-d r)]))
    (is (< (Math/abs (- -0.2 (:win-d r))) 1e-12))
    (is (= [1 1.0] [(:gain-n r) (:gain-d r)]))))

(deftest the-wider-interval-is-the-one-reported
  (is (= {:lo -1.0 :hi 2.0} (sweep/wider {:lo -1.0 :hi 2.0} {:lo 0.0 :hi 1.0})))
  (is (= {:point 1.0} (sweep/wider {:lo 0.0 :hi 1.0} {:point 1.0})) "too few blocks for one is no interval"))

(deftest a-constant-keyed-by-kind-is-swept-as-one-number
  (let [spread (first (filter #(= "cluster-spread" (:name %)) sweep/settings))
        cs     (sweep/configs-for #{"cluster-spread"} {})]
    (is (= {:redraft 0.5 :keeper 0.5 :dynasty 0.5} (sweep/value-of spread 0.5)))
    (is (some #(sweep/shipped? spread %) (:values spread)) "the shipped spread is among the values tried")
    (is (every? (fn [[_ o]] (map? (get o #'draft-day.rankings.faab/cluster-spread))) (rest cs))
        "each configuration sets every kind")
    (is (= [#'draft-day.rankings.faab/cluster-spread {:redraft 2.0 :keeper 2.0 :dynasty 2.0}]
           (sweep/parse-set "cluster-spread=2")))))

(deftest a-sweep-runs-the-shipped-constants-then-every-other-value
  (let [cs (sweep/configs-for #{"pseudo-bids"} {})]
    (is (= ["shipped" {}] (first cs)))
    (is (= (dec (count (:values (first (filter #(= "pseudo-bids" (:name %)) sweep/settings)))))
           (count (rest cs)))
        "every value but the shipped one")
    (is (every? #(re-matches #"pseudo-bids=.*" (first %)) (rest cs))))
  (is (= [["shipped" {}] ["combined" {#'clojure.core/*print-length* 3}]]
         (sweep/configs-for #{} {#'clojure.core/*print-length* 3}))))

(deftest cached-rival-needs-reuses-a-team-only-on-the-same-board
  (let [calls    (atom 0)
        original (fn [teams _ _ by-id _ _ fas]
                   (swap! calls inc)
                   (mapv (fn [t] {:roster-id (:roster-id t)
                                  :needs (zipmap (map :player-id fas)
                                                 (map #(- (:ros-points %)
                                                          (get-in by-id [(first (:active-ids t)) :ros-points]))
                                                      fas))})
                         teams))
        needs    (sweep/cached-rival-needs original)
        teams    [{:roster-id 1 :active-ids ["a"]} {:roster-id 2 :active-ids ["b"]}]
        by-id    {"a" {:ros-points 10.0} "b" {:ros-points 4.0}}
        fas      [{:player-id "x" :ros-points 12.0}]]
    (reset! sweep/needs-cache {})
    (is (= [{:roster-id 2 :needs {"x" 8.0}}]
           (map #(select-keys % [:roster-id :needs]) (needs teams 1 nil by-id 9 [] fas)))
        "every team but mine")
    (is (= 1 @calls))
    (needs teams 3 nil by-id 9 [] fas)
    (is (= 2 @calls) "team 1 is new; team 2 comes from the cache")
    (needs teams 3 nil (assoc-in by-id ["a" :ros-points] 11.0) 9 [] fas)
    (is (= 3 @calls) "a board that moved his players is a new question")
    (is (= {"x" 1.0} (:needs (first (needs teams 3 nil (assoc-in by-id ["a" :ros-points] 11.0) 9 [] fas))))
        "and the answer is the new board's")))

(def mode "Which configuration a stubbed replay runs under." :a)

(deftest a-league-one-configuration-lost-is-left-out-of-every-one
  (let [dir  (str (System/getProperty "java.io.tmpdir") "/sweep-" (random-uuid))
        rows [{:league-id "L1" :stratum {:season "2025"}} {:league-id "L2" :stratum {:season "2025"}}]]
    (with-redefs [sweep/replay-league (fn [row]
                                        (when-not (and (= mode :b) (= "L2" (:league-id row)))
                                          (sweep/league-sums row run)))]
      (let [out (sweep/run-configs {} rows [["a" {}] ["b" {#'mode :b}]] dir)]
        (is (= #{"L1"} (set (keys (get-in out ["a" :by-league]))))
            "L2 failed under b, so a does not score it either")
        (is (= #{"L1"} (set (keys (get-in out ["b" :by-league])))))))))

(deftest a-run-is-named-for-the-constants-it-was-measured-against
  (let [rows [{:league-id "L1"}]
        cs   [["shipped" {}]]]
    (is (= (sweep/run-dir rows cs) (sweep/run-dir rows cs)))
    (is (not= (sweep/run-dir rows cs)
              (with-redefs [draft-day.bid-history/pseudo-bids 99.0] (sweep/run-dir rows cs)))
        "shipping a new constant is a new baseline, not a chunk read back from disk")))

(defn- run-of
  "Sums for one league whose value bid kept `kept` of `n` winners and saved
  `saved` dollars on those it kept."
  [n kept saved]
  {:by-league {"L" {:win-n n :gain-n n :kept (double kept)
                    :gain (- (double saved) (* sweep/lost-claim (- n kept)))}}
   :by-week {}})

(deftest break-even-says-which-lost-claim-costs-a-comparison-survives
  (let [base (run-of 100 80 400.0)]
    (is (= :always (sweep/break-even base (run-of 100 82 420.0))) "keeps more and saves more")
    (is (= :never (sweep/break-even base (run-of 100 78 380.0))) "keeps fewer and saves less")
    (let [[side cost] (sweep/break-even base (run-of 100 78 430.0))]
      (is (= :below side) "saves more by losing more")
      (is (< (Math/abs (- 15.0 cost)) 1e-9) "$0.30 a winner more for 2% more claims lost"))
    (let [[side cost] (sweep/break-even base (run-of 100 82 390.0))]
      (is (= :above side) "keeps more by saving less")
      (is (< (Math/abs (- 5.0 cost)) 1e-9)))
    (is (< (Math/abs (- 4.0 (:saved (sweep/summary base)))) 1e-9) "saved a winner: $400 over 100")))
