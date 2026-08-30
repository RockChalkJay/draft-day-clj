(ns draft-day.rankings.inflation-index-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.inflation-index :as idx]))

(deftest index-rises-on-overpay
  (let [board [{:player-id "a" :position "RB" :value 40}]]
    (is (= 15.0 (idx/inflation-index board [{:player-id "a" :position "RB" :price 55}])))))

(deftest index-negative-on-underpay
  (let [board [{:player-id "a" :position "RB" :value 40}]]
    (is (= -10.0 (idx/inflation-index board [{:player-id "a" :position "RB" :price 30}])))))

(deftest index-zero-with-no-picks
  (is (= 0.0 (idx/inflation-index [{:player-id "a" :position "RB" :value 40}] []))))

(deftest positional-run-tilts-that-position-up
  (let [board [{:player-id "r1" :position "RB" :value 40}
               {:player-id "r2" :position "RB" :value 30}
               {:player-id "w1" :position "WR" :value 40}]
        ls    {:picks [{:player-id "r1" :position "RB" :price 60}    ; RBs bought over par
                       {:player-id "r2" :position "RB" :price 45}
                       {:player-id "w1" :position "WR" :price 40}]}  ; WR at par
        m     (idx/per-position-inflation board ls 1.0)]
    (is (> (m "RB") (m "WR")))        ; the RB run tilts RB inflation up
    (is (= 1.0 (m "WR")))))           ; WR at par stays at the global factor

(deftest no-picks-passes-global-through
  (let [m (idx/per-position-inflation [{:player-id "a" :position "RB" :value 40}] {} 0.9)]
    (is (= 0.9 (m "RB")))
    (is (= 0.9 (m "WR")))))

(deftest a-cheap-pick-barely-moves-its-position
  ;; The tilt is a scale-free ratio, so without shrinkage $3 for a $1 flier reads
  ;; as a 3x overpay and pins the position at the top of the band on the first
  ;; the draft. A hard par threshold only moves that cliff — $6 on a $2 player is
  ;; the same 3x — so the weight grades with how much par is actually at stake.
  (let [board [{:player-id "flier" :position "RB" :value 1}
               {:player-id "two"   :position "TE" :value 2}
               {:player-id "stud"  :position "WR" :value 40}]
        infl  (fn [id pos price]
                ((idx/per-position-inflation
                  board {:picks [{:player-id id :position pos :price price}]} 1.0) pos))]
    (is (< (infl "flier" "RB" 3) 1.06)
        "$3 on a $1 flier moves RB a few percent, not to the ceiling")
    (is (< (infl "two" "TE" 6) 1.10)
        "and $6 on a $2 player is the same 3x, so it is treated the same way")
    (is (> (infl "stud" "WR" 60) 1.15)
        "while the same ratio on a real price speaks at close to full volume")))

(deftest a-run-of-cheap-picks-still-registers
  ;; The threshold this replaced filtered minimum-bid picks out entirely, so a
  ;; whole endgame run at 6x par reported as nothing — a false negative on
  ;; exactly what this namespace exists to catch.
  (let [board (mapv (fn [i] {:player-id (str "te" i) :position "TE" :value 1}) (range 9))
        picks (mapv (fn [i] {:player-id (str "te" i) :position "TE" :price 6}) (range 9))
        one   ((idx/per-position-inflation board {:picks (take 1 picks)} 1.0) "TE")
        run   ((idx/per-position-inflation board {:picks picks} 1.0) "TE")]
    (is (< one 1.15) "one cheap flier is noise")
    (is (> run 1.3) "nine of them bought at 6x is a run")
    (is (> run one) "and the signal grows with the money at stake")))

(deftest real-buys-mixed-with-cheap-ones-still-count
  ;; The threshold also discarded the loudest overpay signal in the room — a
  ;; player bought well above a par the board set low.
  (let [board [{:player-id "stud" :position "RB" :value 50}
               {:player-id "punt" :position "RB" :value 1}]
        m     (idx/per-position-inflation
               board {:picks [{:player-id "stud" :position "RB" :price 50}
                              {:player-id "punt" :position "RB" :price 20}]} 1.0)]
    (is (> (m "RB") 1.05) "paying $20 over par is not nothing")))

(deftest the-index-ignores-k-and-dst
  ;; The board never prices them, so every dollar spent on one scored as pure
  ;; overpay and a room that paid par on every pick still reported a warning.
  (let [board [{:player-id "k1" :position "K" :value 0}
               {:player-id "d1" :position "DST" :value 0}
               {:player-id "r1" :position "RB" :value 40}]]
    (is (= 0.0 (idx/inflation-index board [{:player-id "k1" :position "K" :price 2}
                                           {:player-id "d1" :position "DST" :price 3}])))
    (is (= 5.0 (idx/inflation-index board [{:player-id "r1" :position "RB" :price 45}
                                           {:player-id "k1" :position "K" :price 2}])))))

(deftest per-position-inflation-is-one-factor-not-the-answer
  ;; It used to clamp to [0.6,1.6], a band described as *softer* than the global
  ;; [0.5,1.8] but strictly narrower — so a global factor of 1.8 came back as 1.6
  ;; even for a position with no picks at all. The tilt is now returned raw and
  ;; `inflation/clamp-to-band` holds the finished multiplier once, at the end.
  (let [board [{:player-id "r1" :position "RB" :value 200}]]
    (is (> ((idx/per-position-inflation
             board {:picks [{:player-id "r1" :position "RB" :price 2000}]} 1.0) "RB")
           1.8)
        "a wild overpay is left wild here; the band catches it downstream")
    (is (< ((idx/per-position-inflation
             board {:picks [{:player-id "r1" :position "RB" :price 0}]} 1.0) "RB")
           0.6)
        "and so is a wild underpay")))

(deftest a-position-with-no-picks-really-does-pass-the-global-through
  ;; The docstring always claimed this; the old band made it false at the top of
  ;; the global range, which is exactly where a manager most needs it right.
  (doseq [global [0.5 0.9 1.0 1.5 1.8]]
    (let [m (idx/per-position-inflation
             [{:player-id "r1" :position "RB" :value 40}]
             {:picks []} global)]
      (is (= global (m "WR")) (str "global " global " passes through untouched")))))
