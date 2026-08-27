(ns draft-day.rankings.pos-rank-test
  "The rank's whole job is to be a *stable identifier*, so most of what follows
  pins the properties an ordinal has to have to be one: it restarts per position,
  it is total (no two players share one, no player silently loses one), and two
  runs over the same board agree."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.scoring :as scoring]
            [draft-day.rankings.engine :as engine]
            [draft-day.rankings.pos-rank :as pos-rank]))

(defn- player [id pos points]
  (cond-> {:player-id id :position pos}
    points (assoc :points points)))

(def ^:private board
  [(player "r1" "RB" 210.0)
   (player "w1" "WR" 260.0)
   (player "r2" "RB" 188.5)
   (player "k1" "K"  130.0)
   (player "w2" "WR" 240.0)
   (player "r3" "RB" 300.0)
   (player "d1" "DST" 118.0)
   (player "w3" "WR" 205.0)])

(defn- ranks [b]
  (into {} (map (juxt :player-id :pos-rank)) (pos-rank/with-pos-rank b)))

(deftest ranks-restart-at-one-inside-each-position
  (let [r (ranks board)]
    (testing "best at the position is 1, regardless of where he sits overall"
      (is (= 1 (r "r3")))
      (is (= 1 (r "w1"))))
    (testing "the rest count down by points, not by board order"
      (is (= [2 3] [(r "r1") (r "r2")]))
      (is (= [2 3] [(r "w2") (r "w3")])))))

(deftest kickers-and-defenses-get-a-rank
  ;; The case :vorp could not serve: the engine gives K/DST no replacement level,
  ;; so their :vorp is nil by design. Ordering on :points is what covers them.
  (let [r (ranks board)]
    (is (= 1 (r "k1")))
    (is (= 1 (r "d1")))))

(deftest an-unscored-row-gets-no-rank-rather-than-a-wrong-one
  ;; The model leaves :points off rows it never scored. Ranking those last would
  ;; put a number on a player the board has no opinion about; the cell is
  ;; supposed to fall back to the bare position instead.
  (let [b (conj board (player "ghost" "RB" nil))
        r (ranks b)]
    (is (nil? (r "ghost")))
    (testing "and it does not consume an ordinal the real players need"
      (is (= #{1 2 3} (set (keep r ["r1" "r2" "r3"])))))))

(deftest row-order-is-preserved
  ;; Downstream stages and the wire order both read the board as emitted; this
  ;; step indexes, it does not sort.
  (is (= (mapv :player-id board)
         (mapv :player-id (pos-rank/with-pos-rank board)))))

(deftest identical-points-still-produce-a-stable-total-order
  ;; Sleeper rounds, so ties on the tail are the common case. Without a tiebreak
  ;; a player's RB47 could become RB48 between two recomputes of the same board.
  (let [tied [(player "b" "RB" 90.0) (player "a" "RB" 90.0) (player "c" "RB" 90.0)]
        r    (ranks tied)]
    (is (= #{1 2 3} (set (vals r))) "every tied player still gets a distinct rank")
    (is (= r (ranks (reverse tied)))
        "and the same board in a different order ranks them the same")))

;; ---- the shipped chain ----
;; The column arriving blank because a key was dropped between the engine and the
;; wire is a bug this repo has shipped before (see
;; `rankings.scoring-propagation-test`), and the *static* half of the promise —
;; RB1 stays RB1 after RB1 is drafted — can only be checked end to end.

(defn- stat-player [id pos yards]
  {:player-id id :player-name id :position pos
   :stats {:rush_yd yards :rush_td 5 :rec 20 :rec_yd 200 :rec_td 1}})

(defn- deep-board []
  (into [] (mapcat (fn [pos]
                     (map #(stat-player (str pos %) pos (- 1500 (* % 25)))
                          (range 40))))
        ["QB" "RB" "WR" "TE"]))

(def ^:private league
  {:teams (vec (repeat 12 {:bankroll 200.0
                           :roster (vec (repeat 15 {:pos "BENCH" :player-id nil}))}))
   :drafted-player-ids #{} :starting-bankroll 200 :picks []})

(defn- shipped
  "Board id -> :pos-rank after the full static + live chain, under `drafted`."
  [drafted]
  (->> (engine/live-valuation
        (engine/static-rankings (deep-board) (:ppr scoring/presets) 12
                                {:replacement-config {:qb 1 :rb 2 :wr 2 :te 1 :flex 1}})
        (assoc league :drafted-player-ids drafted))
       :players
       (into {} (map (juxt :player-id :pos-rank)))))

(deftest pos-rank-survives-the-live-layer
  (let [r (shipped #{})]
    (is (= 1 (r "RB0")) "the best RB by points is RB1 on the shipped board")
    (is (= 40 (count (into #{} (keep r) (map #(str "RB" %) (range 40)))))
        "and every RB carries a distinct rank all the way through")))

(deftest pos-rank-does-not-renumber-when-players-are-drafted
  ;; The whole reason this lives in `static-rankings` and not beside `tcm` in the
  ;; live layer. If it ever moves, RB1 becomes a scarcity signal and stops being
  ;; an identifier — and this is the test that says so out loud.
  (is (= (shipped #{}) (shipped #{"RB0" "RB1" "WR0"}))))
