(ns draft-day.bid-history-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.bid-history :as bh]
            [draft-day.bid-prior :as prior]))

(defn- bid [owner amount won?]
  {:roster-id (str "r-" owner) :owner-id owner :amount amount :won? won?})

(defn- auction [week & bids] {:week week :at week :player-id "p" :bids (vec bids)})

(defn- season [budget & auctions] {:season "2025" :budget budget :auctions (vec auctions)})

(defn- typical
  "The dollar bid that sits exactly on the backbone for `bidders` in `week`, in
  a $100 league."
  [bidders week]
  (* 100 (bh/typical-share bidders week 100)))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest a-typical-bid-reads-as-no-multiplier-at-all
  (let [t  (typical 1 3)
        h  {:seasons [(season 100 (auction 3 (bid "u1" t true)))]}
        bs (bh/weighted-bids (:seasons h))]
    (is (close? 0.0 (:log-ratio (first bs))))
    (is (close? 0.0 (bh/league-log-multiplier bs)))))

(deftest a-league-that-pays-double-is-read-as-paying-double-once-it-has-shown-it
  (let [t    (typical 1 3)
        many (apply season 100 (map #(auction 3 (bid (str "u" %) (* 2 t) true)) (range 300)))
        few  (season 100 (auction 3 (bid "u1" (* 2 t) true)))]
    (is (< 0.6 (bh/league-log-multiplier (bh/weighted-bids [many])) (Math/log 2))
        "three hundred bids at twice the price read as nearly twice")
    (is (< 0.0 (bh/league-log-multiplier (bh/weighted-bids [few])) 0.05)
        "one bid barely moves a league off the Sleeper-wide price")))

(deftest a-bid-in-another-budget-is-read-against-that-budgets-typical
  (let [shift (get-in prior/budget-shift [1 :log-shift])
        t1000 (* 1000 (bh/typical-share 1 3 1000))]
    (is (close? (* (bh/typical-share 1 3 100) (Math/exp shift)) (bh/typical-share 1 3 1000)))
    (is (close? 0.0 (:log-ratio (first (bh/weighted-bids
                                         [(season 1000 (auction 3 (bid "u1" t1000 true)))]))))
        "a $1000 league's ordinary bid is ordinary, not small")))

(deftest a-manager-with-no-evidence-is-the-typical-sleeper-manager
  (let [{:keys [profiles]} (bh/profiles {:seasons [(season 100 (auction 1 (bid "u1" 0 true)))]})
        p (first profiles)]
    (is (< (Math/abs (- (:per-week p) (get-in prior/managers [:per-week :p50]))) 0.1)
        "one bid in one week is two pseudo-weeks of the median rate and a little of his own")
    (is (< (get-in prior/managers [:zero-share :p50]) (:zero-share p) 0.6)
        "one $0 bid leans him only a little toward bidding $0")))

(deftest a-manager-who-bids-big-in-his-own-league-reads-as-aggressive
  (let [t     (typical 1 3)
        crowd (map #(auction 3 (bid (str "u" %) t true)) (range 60))
        big   (map (fn [_] (auction 3 (bid "big" (* 3 t) true))) (range 30))
        {:keys [profiles log-multiplier]} (bh/profiles {:seasons [(apply season 100 (concat crowd big))]})
        p     (some #(when (= "big" (:manager %)) %) profiles)]
    (is (< 0.5 (:aggression p)) "well above his league")
    (is (close? (:log-multiplier p) (+ log-multiplier (:aggression p))))
    (is (< 0.8 (:log-multiplier p) (Math/log 3))
        "his bids raise the level he is read against; together they carry his price, less shrinkage")
    (is (= :big-spender (:style p)))))

(deftest last-season-counts-but-less
  (let [t    (typical 1 3)
        now  (season 100 (auction 3 (bid "u1" t true)))
        then (apply season 100 (map (fn [_] (auction 3 (bid "u1" (* 3 t) true))) (range 20)))
        with (first (:profiles (bh/profiles {:seasons [now then]})))
        full (first (:profiles (bh/profiles {:seasons [(apply season 100 (concat (:auctions now)
                                                                               (:auctions then)))]})))]
    (is (pos? (:log-multiplier with)) "last season's big bids still say something")
    (is (< (:log-multiplier with) (:log-multiplier full)) "but less than if they were this season's")
    (is (= 21 (:bids with)))
    (is (= 1 (:current-bids with)))))

(deftest a-manager-new-to-the-league-is-not-charged-for-last-season
  (let [then (apply season 100 (map #(auction % (bid "veteran" 1 true)) (range 1 18)))
        now  (apply season 100 (mapcat (fn [w] [(auction w (bid "rookie" 1 true))
                                                (auction w (bid "rookie" 1 true))
                                                (auction w (bid "rookie" 1 true))])
                                       [1 2 3]))
        by   (into {} (map (juxt :manager identity))
                   (:profiles (bh/profiles {:seasons [now then]})))]
    (is (< 2.0 (:per-week (by "rookie")))
        "three claims a week this season, with no silent season behind him")
    (is (< (:per-week (by "veteran")) 1.0)
        "the veteran, who played last season, is charged for this one too")))

(deftest a-manager-follows-his-owner-id-and-an-ownerless-bid-its-roster
  (let [h {:seasons [(season 100 (auction 1 {:roster-id 4 :owner-id nil :amount 1 :won? true}))]}]
    (is (= ["roster:4"] (map :manager (:profiles (bh/profiles h)))))
    (is (= "roster:4" (:manager (bh/team-profile {:roster-id 4 :owner-id "someone-new"}
                                                  (:profiles (bh/profiles h)))))
        "a team whose owner never bid falls back to what its roster bid")
    (is (= "77" (:manager (bh/team-profile {:roster-id 9 :owner-id 77}
                                           [{:manager "77"}])))
        "the sync's numeric owner id matches the history's string one")
    (is (nil? (bh/team-profile {:roster-id 9 :owner-id "x"} [])))))

(deftest recent-silence-reads-as-quiet
  (let [early (map #(auction % (bid "u1" 5 true)) [1 2 3])
        later (map #(auction % (bid "u2" 5 true)) [4 5 6 7])
        {:keys [profiles weeks-run]} (bh/profiles {:seasons [(apply season 100 (concat early later))]})
        by    (into {} (map (juxt :manager identity)) profiles)]
    (is (= 7 weeks-run))
    (is (= :quiet (:style (by "u1"))) "nothing in the last three weeks")
    (is (not= :quiet (:style (by "u2"))))
    (is (< (:per-week (by "u1")) (:per-week (by "u2")))
        "the same number of bids, but his are older and decayed")))

(deftest the-styles-follow-their-thresholds
  (let [base {:bids 30 :current-bids 20 :recent-bids 3 :per-week 1.0 :zero-share 0.5 :aggression 0.0}]
    (is (= :typical (bh/style base 8)))
    (is (= :new (bh/style (assoc base :bids 0) 8)))
    (is (= :quiet (bh/style (assoc base :recent-bids 0) 8)))
    (is (not= :quiet (bh/style (assoc base :recent-bids 0) 2)) "too early in the season to call")
    (is (= :sniper (bh/style (assoc base :per-week 0.5 :aggression 0.5) 8)))
    (is (= :sniper (bh/style (assoc base :per-week 0.5 :top-share 0.3) 8))
        "one bid for a quarter of the budget is a sniper, however his average reads")
    (is (= :sniper (bh/style (assoc base :per-week 0.5 :top-share 0.3 :recent-bids 0) 8))
        "going quiet and then pouncing is what a sniper does")
    (is (= :big-spender (bh/style (assoc base :per-week 2.0 :aggression 0.7) 8)))
    (is (= :zero-flyer (bh/style (assoc base :per-week 2.0 :zero-share 0.85) 8)))))

(deftest a-style-line-says-what-it-rests-on
  (is (= "Sniper — 0.6 claims a week, 40% at $0, top bid $38"
         (bh/style-line {:style :sniper :bids 12 :per-week 0.6 :zero-share 0.4 :max-bid 38})))
  (is (= "No bids yet" (bh/style-line nil)))
  (is (= "$0 flyer — 3.0 claims a week, 90% at $0"
         (bh/style-line {:style :zero-flyer :bids 40 :per-week 3.0 :zero-share 0.9 :max-bid 0}))
      "no top bid to report when he has only ever bid $0"))

(deftest win-over-second-reads-only-contested-wins
  (let [auctions [(auction 1 (bid "u1" 20 true) (bid "u2" 10 false))
                  (auction 2 (bid "u1" 6 true) (bid "u2" 0 false))
                  (auction 3 (bid "u2" 9 true) (bid "u1" 3 false))]]
    (is (= 2.0 (bh/win-over-second auctions "u1")) "the $0 runner-up gives no ratio")
    (is (= 3.0 (bh/win-over-second auctions "u2")))
    (is (nil? (bh/win-over-second auctions "nobody")))))

(deftest an-empty-history-has-no-profiles
  (is (nil? (bh/profiles {:seasons []})))
  (is (empty? (:profiles (bh/profiles {:seasons [(season 100)]})))))
