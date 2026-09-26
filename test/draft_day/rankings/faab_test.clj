(ns draft-day.rankings.faab-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.bid-prior :as prior]
            [draft-day.rankings.faab :as faab]))

(defn- near? [a b tolerance] (< (Math/abs (- (double a) (double b))) tolerance))

(defn- rival
  "A rival who bids with chance `p`, from a hand-made distribution
  `{amount chance}` over a $100 budget."
  [p amounts & {:keys [tie] :or {tie 0.5}}]
  (let [pmf (double-array 101)]
    (doseq [[b v] amounts] (aset pmf b (double v)))
    {:p p :pmf pmf :cdf (faab/cumulative pmf) :tie tie}))

(deftest a-rivals-claims-are-shared-out-and-sum-to-his-rate
  (let [fas   [{:player-id "a" :ros-vorp 20.0} {:player-id "b" :ros-vorp -5.0}
               {:player-id "c"} {:player-id "d" :ros-vorp 0.0}]
        rates (faab/claim-rates {"a" 10.0 "b" 30.0 "c" -12.0} fas 1.5)]
    (is (near? 1.5 (reduce + (vals rates)) 1e-9) "every claim he makes lands somewhere")
    (is (near? (* 1.5 (/ 20.0 50.0)) (rates "a") 1e-9)
        "need 10 plus half the 20 he clears replacement by, out of 50")
    (is (= #{"a" "b"} (set (keys rates)))
        "a negative need and nothing over replacement draw nothing"))
  (is (= {} (faab/claim-rates {} [{:player-id "b" :ros-vorp -5.0}] 1.5))
      "a rival nobody interests claims nobody"))

(deftest a-quiet-rival-with-one-target-can-still-sit-the-week-out
  (let [rate (get (faab/claim-rates {"a" 25.0} [{:player-id "a"}] 0.3) "a")]
    (is (near? 0.3 rate 1e-9))
    (is (< 0.25 (faab/bid-chance rate) 0.27)
        "a claim every three weeks is not a certain bid, as a fixed count makes it")))

(deftest a-rivals-bid-is-a-distribution-over-whole-dollars
  (doseq [bucket [1 2 3 4] phase [:early :mid :late] budget [100 1000] scale [0.4 1.0 2.5]]
    (let [pmf (faab/bid-pmf (prior/bid-share [bucket phase]) scale 0.5 budget 0 budget)
          cdf (vec (faab/cumulative pmf))]
      (is (every? #(<= 0.0 %) pmf))
      (is (every? (fn [[a b]] (<= a (+ b 1e-12))) (partition 2 1 cdf)) "the CDF never falls")
      (is (near? 1.0 (peek cdf) 1e-9) "and reaches 1"))))

(deftest a-typical-manager-bids-zero-as-often-as-his-cell-does
  (let [cell (prior/bid-share [2 :mid])
        typical (get-in prior/managers [:zero-share :p50])]
    (is (near? (:p-zero cell) (aget (faab/bid-pmf cell 1.0 typical 100 0 100) 0) 1e-9))
    (is (< (:p-zero cell) (faab/zero-chance (:p-zero cell) 0.83)) "a $0 flyer more often")
    (is (> (:p-zero cell) (faab/zero-chance (:p-zero cell) 0.14)) "a sniper less")))

(deftest a-positive-bid-sits-where-the-backbone-puts-it
  (let [cell (prior/bid-share [2 :mid])
        pts  (faab/cdf-points (:positive cell) 1.0 100)]
    (is (near? 0.5 (faab/cdf-at pts 6.0) 1e-9) "the median positive bid is 6% of $100")
    (is (near? 0.5 (faab/cdf-at (faab/cdf-points (:positive cell) 2.0 100) 12.0) 1e-9)
        "and twice as aggressive a manager's is twice that")))

(deftest bids-heap-on-round-numbers-as-often-as-sleeper-managers-do
  (doseq [[budget unit table] [[100 5 :budget-100] [1000 50 :budget-1000]]]
    (let [cell  (prior/bid-share [2 :mid])
          pos   (faab/with-heaps (faab/positive-pmf (faab/cdf-points (:positive cell) 1.0 budget)
                                                    budget)
                                 budget)
          above (reduce + (drop unit pos))
          share (fn [pred] (/ (reduce + (keep-indexed (fn [i v] (when (and (>= i unit) (pred i)) v)) pos))
                              above))
          rates (table prior/heaping)]
      (is (near? 1.0 (reduce + pos) 1e-9) "heaping moves mass, it does not add any")
      (is (near? (:round rates) (share #(zero? (mod % unit))) 1e-9))
      (is (near? (:round-2x rates) (share #(zero? (mod % (* 2 unit)))) 1e-9))
      (is (near? (:one-over rates) (share #(= 1 (mod % unit))) 1e-9)))))

(deftest a-rival-bids-no-more-than-he-has-and-no-less-than-the-minimum
  (let [cell (prior/bid-share [4 :late])
        pmf  (faab/bid-pmf cell 1.0 0.5 100 1 12)]
    (is (zero? (aget pmf 0)) "no $0 bids under a $1 minimum")
    (is (every? zero? (drop 13 pmf)) "nothing over the $12 he has left")
    (is (near? 1.0 (reduce + pmf) 1e-9))
    (is (< 0.3 (aget pmf 12)) "wanting more than he has, he bids all of it")
    (is (nil? (faab/bid-pmf cell 1.0 0.5 100 1 0)) "under the minimum he cannot bid at all")))

(deftest a-broke-rival-cannot-beat-a-dollar
  (let [broke (faab/bid-pmf (prior/bid-share [4 :early]) 1.0 0.5 100 0 0)
        r     {:p 1.0 :pmf broke :cdf (faab/cumulative broke) :tie 0.5}]
    (is (near? 1.0 (aget broke 0) 1e-9) "everything he would bid is the nothing he has")
    (is (near? 1.0 (faab/win-chance [r] 1) 1e-9))
    (is (near? 0.5 (faab/win-chance [r] 0) 1e-9) "at $0 it is a tie, and the tie is a coin flip")))

(deftest ties-go-by-waiver-order
  (let [at5 #(rival 1.0 {5 1.0} :tie %)]
    (is (= 1.0 (faab/win-chance [(at5 1.0)] 5)) "ahead of him in waiver order")
    (is (= 0.0 (faab/win-chance [(at5 0.0)] 5)) "behind him")
    (is (= 0.5 (faab/win-chance [(at5 0.5)] 5)) "either order unknown")
    (is (= 1.0 (faab/win-chance [(at5 0.0)] 6)) "a dollar more settles it"))
  (is (= 1.0 (faab/tie-chance 2 7)) "the lower waiver position goes first")
  (is (= 0.0 (faab/tie-chance 7 2)))
  (is (= 0.5 (faab/tie-chance nil 2)))
  (is (= 0.5 (faab/tie-chance 2 nil))))

(deftest every-rival-has-to-be-beaten
  (let [a (rival 0.5 {3 1.0})
        b (rival 0.4 {8 1.0})]
    (is (near? (* 0.75 0.6) (faab/win-chance [a b] 3) 1e-9)
        "a sits it out or ties and loses the coin flip; b sits it out")
    (is (near? 0.6 (faab/win-chance [a b] 4) 1e-9) "past a, and b still has to stay home")))

(deftest a-crowd-at-ten-dollars-is-beaten-by-eleven
  (is (= 11 (faab/value-bid [(rival 0.9 {3 0.2 10 0.8})] 30 0 100))
      "$10 ties the crowd and wins half of those; $11 beats all of it"))

(deftest nobody-else-bidding-means-the-league-minimum
  (is (= 0 (faab/value-bid [] 40 0 100)))
  (is (= 1 (faab/value-bid [] 40 1 100)) "the floor, where the league sets one")
  (is (= 1.0 (faab/win-chance [] 0)))
  (is (nil? (faab/value-bid [] 40 1 0)) "with less left than the minimum there is no bid"))

(deftest the-value-bid-never-exceeds-what-he-is-worth
  (let [hot [(rival 0.9 {20 0.5 40 0.5}) (rival 0.8 {30 1.0})]]
    (doseq [w [0 5 15 25 35 60]]
      (is (<= (faab/value-bid hot w 0 100) w) (str "walk-away " w)))
    (is (= 1 (faab/value-bid hot 0 1 100)) "except where the league minimum is above it")
    (is (<= (faab/value-bid hot 60 0 25) 25) "or past what is left")))

(deftest the-sure-bid-is-the-cheapest-that-wins-nine-times-in-ten
  (let [rs [(rival 0.7 {0 0.3 5 0.3 12 0.3 25 0.1}) (rival 0.5 {1 0.5 8 0.5})]
        s  (faab/sure-bid rs 0 100)]
    (is (>= (faab/win-chance rs s) faab/sure-win))
    (is (< (faab/win-chance rs (dec s)) faab/sure-win))
    (is (nil? (faab/sure-bid rs 0 10)) "nothing he can afford gets there")))

(deftest the-top-rival-bid-is-read-given-that-somebody-bids
  (let [rs [(rival 0.5 {4 0.5 10 0.5}) (rival 0.2 {2 1.0})]]
    (is (= [4 10] (faab/top-bid rs 100))
        "nobody bids 40% of the time; given somebody does, $2 is a sixth, $4 two fifths"))
  (is (nil? (faab/top-bid [] 100)) "nobody to read"))

(deftest threats-are-the-likeliest-bidders-at-most-three
  (let [rs (map (fn [[id p left]]
                  (assoc (rival p {1 1.0}) :roster-id id :name (str "T" id)
                         :faab-left left :style :typical))
                [[1 0.02 90] [2 0.4 10] [3 0.4 80] [4 0.9 5] [5 0.3 50]])
        ts (faab/threats rs)]
    (is (= [4 3 2] (mapv :roster-id ts)) "by chance, then by the money behind it")
    (is (= #{:roster-id :name :faab-left :style :p} (set (keys (first ts)))))))

(def ^:private waiver {:type :faab :budget 100 :min-bid 0})

(def ^:private me {:roster-id 1 :faab-left 80 :waiver-position 3})

(defn- fa [id w] {:player-id id :player-name id :position "RB" :walk-away w :ros-vorp -5.0})

(defn- a-rival [id needs & {:as more}]
  (merge {:roster-id id :owner-id (str "u" id) :name (str "Team " id) :faab-left 90
          :waiver-position 5 :needs needs}
         more))

(defn- market [fas rivals & {:keys [habits waiver] :or {waiver waiver}}]
  (faab/with-market fas {:rivals rivals :me me :waiver waiver :week 5
                         :habits (or habits (faab/league-habits nil 12))}))

(deftest a-player-nobody-else-wants-goes-for-the-minimum
  (let [[p] (market [(fa "stash" 12)] [(a-rival 2 {})])]
    (is (= 0 (:bid p)))
    (is (= 1.0 (:win-prob p)))
    (is (= 0 (:bid-sure p)))
    (is (= 0.0 (:rivals p)))
    (is (= {:top nil :uncontested 1.0 :threats []} (:competition p)))))

(deftest a-rival-who-needs-him-is-bid-against
  (let [[hot cold] (market [(fa "hot" 40) (fa "cold" 40)] [(a-rival 2 {"hot" 60.0})])]
    (is (< 0.6 (:rivals hot) 0.8) "one claim a week, all of it on him: 1 - e^-1.19")
    (is (< 0 (:bid hot) (:walk-away hot)))
    (is (> (:bid hot) (:bid cold)))
    (is (>= (:bid-sure hot) (:bid hot)))
    (is (= [2] (mapv :roster-id (get-in hot [:competition :threats]))))
    (let [[p50 p90] (get-in hot [:competition :top])]
      (is (<= 0 p50 p90)))))

(deftest no-walk-away-means-no-bid-at-all
  ;; A league that does not run FAAB, or a budget spent: nil, not $0.
  (let [[p] (market [(fa "x" nil)] [(a-rival 2 {"x" 60.0})])]
    (is (nil? (:bid p)))
    (is (not (contains? p :competition)))))

(deftest a-rival-under-the-league-minimum-is-no-rival
  (let [[p] (market [(fa "hot" 40)] [(a-rival 2 {"hot" 60.0} :faab-left 0)]
                    :waiver (assoc waiver :min-bid 1))]
    (is (= 0.0 (:rivals p)))
    (is (= 1 (:bid p)) "the minimum, and it wins")))

(deftest an-active-rival-is-likelier-to-bid-than-a-silent-one
  (let [bids    (map (fn [w] {:week w :player-id "p"
                              :bids [{:roster-id 2 :owner-id "u2" :amount 5 :won? true}]})
                     (mapcat #(repeat 4 %) (range 1 7)))
        history {:seasons [{:season "2026" :budget 100 :auctions (vec bids)}]}
        habits  (faab/league-habits history 12)
        [p]     (market [(fa "hot" 40)] [(a-rival 2 {"hot" 60.0}) (a-rival 3 {"hot" 60.0})]
                        :habits habits)
        by-team (into {} (map (juxt :roster-id :p)) (get-in p [:competition :threats]))]
    (is (> (by-team 2) 0.9) "four claims a week, off the league's history")
    (is (< (by-team 3) 0.4) "no bid in it: six weeks silent")))

(deftest with-no-history-the-bids-are-sleeper-wide
  (is (= {:source :sleeper-wide :auctions 0 :seasons [] :league-multiplier 1.0 :min-bid 0}
         (faab/bidding (faab/league-habits nil 12) waiver))))

(deftest a-big-league-starts-below-sleepers-price
  (is (near? (Math/exp (get-in prior/team-shift [:large :log-shift]))
             (:league-multiplier (faab/bidding (faab/league-habits nil 14) waiver))
             1e-3)
      "more than twelve teams bid a sixth less before their own history says otherwise"))

(deftest a-history-is-the-leagues-own-evidence
  (let [history {:seasons [{:season "2026" :budget 100
                            :auctions [{:week 3 :player-id "p"
                                        :bids [{:roster-id 2 :owner-id "u2" :amount 12 :won? true}]}]}
                           {:season "2025" :budget 100 :auctions []}]}
        b       (faab/bidding (faab/league-habits history 12) waiver)]
    (is (= :league (:source b)))
    (is (= 1 (:auctions b)))
    (is (= ["2026" "2025"] (:seasons b)))
    (testing "the league's price level moves off 1.0 only as far as its bids carry it"
      (is (< 1.0 (:league-multiplier b) 1.2)))))
