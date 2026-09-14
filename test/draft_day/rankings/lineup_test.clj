(ns draft-day.rankings.lineup-test
  "The optimizer, and the cases `:upgrade` gets wrong.

  Most of what follows pins the one property the whole change exists for: a
  player who would not start is worth nothing, however far he clears the last
  man on the bench."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.db :as db]
            [draft-day.rankings.lineup :as lineup]))

(defn- p [id pos pts] {:player-id id :position pos :ros-points pts})

(def ^:private slots (db/starting-slots db/default-roster))

(defn- pts [players] (lineup/lineup-points players slots :ros-points))

;; QB 1, RB 2, WR 2, TE 1, FLEX 1, K 1, DST 1 — the shape a real Sleeper league
;; reports, and the bench is deliberately absent from the slot list.
(def ^:private roster
  [(p "qb1" "QB" 260.0)
   (p "rb1" "RB" 180.0) (p "rb2" "RB" 150.0) (p "rb3" "RB" 90.0)
   (p "wr1" "WR" 170.0) (p "wr2" "WR" 120.0)
   (p "te1" "TE" 95.0)
   (p "k1"  "K"  100.0)
   (p "d1"  "DST" 80.0)
   ;; A real bench. Without these every rostered player starts, and "would he
   ;; start" stops being a question the fixture can even ask.
   (p "wr3" "WR" 40.0) (p "rb5" "RB" 30.0)])

(def ^:private bench-drop (p "rb5" "RB" 30.0))
(def ^:private starting-drop (p "rb3" "RB" 90.0))

(deftest the-bench-is-not-part-of-the-total
  (is (= 9 (count slots)) "QB RB RB WR WR TE FLEX K DST")
  (is (not-any? #{"BENCH"} slots)))

(deftest flex-takes-the-best-leftover-not-the-first
  ;; rb3 (90) beats wr-nobody; the FLEX seat must be filled after the dedicated
  ;; ones or it takes a back the RB seat needed.
  (let [seated (into {} (map (fn [[s pl]] [s (:player-id pl)])) 
                     (map (juxt first second) (lineup/best-lineup roster slots :ros-points)))]
    (is (= "rb3" (seated "FLEX")))
    (is (= 1245.0 (pts roster)) "260+180+150+170+120+95+90+100+80")))

(deftest flex-is-filled-last-whatever-order-the-slots-arrive-in
  ;; The ns docstring's whole correctness argument, and it is invisible against
  ;; `roster-template`, which already emits FLEX after RB/WR/TE — so reordering
  ;; is a no-op there and a mutation that drops it passes every other test.
  ;; Fed FLEX-first, a naive fill takes rb1 (180) for FLEX and leaves the RB
  ;; seats to rb2 and rb3, which scores the same *set* here but not in general:
  ;; with only one RB on the roster it would strand the dedicated seat empty.
  ;; One back, one receiver, seats FLEX and RB. Filling FLEX first takes the
  ;; only back (180) and strands the RB seat, since no receiver can fill it —
  ;; 180. Filling RB first seats the back and leaves the receiver for FLEX —
  ;; 350. The ordering is worth 170 points here, so it is not a tidiness rule.
  (let [players    [(p "rb1" "RB" 180.0) (p "wr1" "WR" 170.0)]
        flex-first ["FLEX" "RB"]]
    (is (= 350.0 (lineup/lineup-points players flex-first :ros-points)))
    (let [seated (into {} (map (juxt first (comp :player-id second)))
                       (lineup/best-lineup players flex-first :ros-points))]
      (is (= "rb1" (seated "RB")) "the dedicated seat gets the only man who fits")
      (is (= "wr1" (seated "FLEX"))))))

(deftest a-backup-quarterback-is-worth-nothing
  ;; The headline case. He clears the worst bench player by 150 points and adds
  ;; nothing, because he would never start.
  (let [qb2 (p "qb2" "QB" 180.0)]
    (is (zero? (lineup/upgrade (pts roster) roster qb2 bench-drop slots :ros-points))
        "180 points of quarterback behind a 260 starter changes nothing")
    (testing "while the bench delta calls him a massive add"
      (is (= 150.0 (- (:ros-points qb2) (:ros-points bench-drop)))))))

(deftest dropping-a-starter-for-a-non-starter-is-negative-and-says-so
  ;; Not a guard case — `drop-candidate` names the lowest-scoring player holding
  ;; an active seat, and on a thin bench that man is in the lineup. Clamping
  ;; this at 0 would hide the one thing the manager most needs to know.
  (is (= -50.0 (lineup/upgrade (pts roster) roster (p "qb2" "QB" 180.0)
                               starting-drop slots :ros-points))
      "rb3 (90) held FLEX; losing him promotes wr3 (40), so the lineup drops 50"))

(deftest a-better-starter-is-worth-the-difference
  ;; wr enters at WR1 (200) and pushes rb3 (90) out of FLEX, since wr2 (120) now
  ;; takes the second WR seat and 120 > 90.
  (is (= 110.0 (lineup/upgrade (pts roster) roster (p "wrX" "WR" 200.0)
                               bench-drop slots :ros-points))
      "1355 - 1245"))

(deftest kickers-and-defenses-fill-slots-and-score
  ;; The case `replacement-config` would have dropped: it excludes K and DST
  ;; because replacement prices neither, which is right there and wrong here.
  (let [without (remove #(#{"k1" "d1"} (:player-id %)) roster)]
    (is (= 180.0 (- (pts roster) (pts without))))
    (is (= 100.0 (lineup/upgrade (pts without) without (p "kX" "K" 100.0) nil slots :ros-points)))))

(deftest an-open-seat-means-the-whole-line
  ;; No drop, and a slot nobody fills — the claim is worth everything he brings.
  (let [thin [(p "qb1" "QB" 260.0)]]
    (is (= 170.0 (lineup/upgrade (pts thin) thin (p "wrX" "WR" 170.0) nil slots :ros-points)))))

(deftest a-short-roster-yields-a-short-lineup-rather-than-throwing
  (is (= 260.0 (pts [(p "qb1" "QB" 260.0)])))
  (is (zero? (pts [])))
  (is (zero? (pts nil))))

(deftest an-unvalued-row-is-skipped-not-seated-at-zero
  ;; Same distinction `waiver/drop-candidate` draws: "no projection for him" and
  ;; "projected to score nothing" are different claims.
  (let [ghost (dissoc (p "ghost" "WR" nil) :ros-points)]
    (is (= (pts roster) (pts (conj roster ghost))))))

(deftest a-bench-stash-is-zero-whatever-his-position
  ;; The common case, and the one `:upgrade` gets loudest-wrong: nobody who
  ;; cannot crack the lineup is an upgrade to it.
  (doseq [cand [(p "x" "WR" 1.0) (p "y" "QB" 5.0) (p "z" "DST" 0.0)]]
    (is (zero? (lineup/upgrade (pts roster) roster cand bench-drop slots :ros-points))
        (str (:player-id cand)))))

;; ---- seats wider than FLEX ----
;; `league-import` folds SUPER_FLEX into the bench for the draft board, so these
;; only bite once a lineup is built from a synced league's own seats.

(deftest slot-breadth-orders-the-seats-narrowest-first
  (is (= 1 (lineup/slot-breadth "RB")))
  (is (= 1 (lineup/slot-breadth "K")))
  (is (= 2 (lineup/slot-breadth "WRRB_FLEX")))
  (is (= 3 (lineup/slot-breadth "FLEX")))
  (is (= 4 (lineup/slot-breadth "SUPER_FLEX")))
  (is (= 1 (lineup/slot-breadth "DL"))
      "an IDP seat nobody has named accepts exactly its own position"))

(deftest superflex-is-filled-after-flex
  ;; The generalization, and the case the old hardcoded "FLEX last" got wrong
  ;; one seat over: SUPER_FLEX accepts everything FLEX does plus a quarterback,
  ;; so filling it first takes the back FLEX needed.
  ;;
  ;; One back and one quarterback, seats SUPER_FLEX and RB. Filled SF-first the
  ;; SF takes rb1 (180) and the RB seat strands, since no QB can fill it — 180.
  ;; Narrowest-first seats the back and leaves the QB for SF — 440. The ordering
  ;; is worth 260 points here, so it is not a tidiness rule.
  (let [players  [(p "rb1" "RB" 180.0) (p "qb1" "QB" 260.0)]
        sf-first ["SUPER_FLEX" "RB"]]
    (is (= 440.0 (lineup/lineup-points players sf-first :ros-points)))
    (let [seated (into {} (map (juxt first (comp :player-id second)))
                       (lineup/best-lineup players sf-first :ros-points))]
      (is (= "rb1" (seated "RB")) "the dedicated seat gets the only man who fits")
      (is (= "qb1" (seated "SUPER_FLEX"))))))

(deftest flex-is-filled-before-superflex-when-both-are-present
  ;; Both wide seats at once. FLEX (3 positions) must fill before SUPER_FLEX
  ;; (4), or SF takes the best back and FLEX is left a worse one.
  (let [players [(p "rb1" "RB" 180.0) (p "qb1" "QB" 260.0) (p "wr1" "WR" 170.0)]
        slots   ["SUPER_FLEX" "FLEX"]
        seated  (into {} (map (juxt first (comp :player-id second)))
                      (lineup/best-lineup players slots :ros-points))]
    (is (= "rb1" (seated "FLEX")) "FLEX takes the best it can seat")
    (is (= "qb1" (seated "SUPER_FLEX")) "SUPER_FLEX takes the quarterback only it can seat")
    (is (= 440.0 (lineup/lineup-points players slots :ros-points)))))

(deftest a-superflex-league-starts-its-second-quarterback
  ;; The whole point of the seat, and what folding SUPER_FLEX into the bench
  ;; hid: a QB2 worth nothing in a one-QB league is a starter here.
  (let [sf-slots ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "SUPER_FLEX" "K" "DST"]
        qb2      (p "qb2" "QB" 180.0)]
    (is (= 140.0 (- (lineup/lineup-points (conj roster qb2) sf-slots :ros-points)
                    (lineup/lineup-points roster sf-slots :ros-points)))
        "he takes the SUPER_FLEX seat wr3 (40) held, so 180 - 40")
    (testing "and the one-QB league it was measured against still wants none of him"
      (is (zero? (lineup/upgrade (pts roster) roster qb2 bench-drop slots :ros-points))))))
