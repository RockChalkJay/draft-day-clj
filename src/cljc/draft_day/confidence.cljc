(ns draft-day.confidence
  "Whether the gap between two weekly projections is worth believing.

  Measured, not assumed. For same-position pairs in the same week, how often did
  the higher-projected player actually outscore the lower? Scored from 2025
  weekly projections against 2025 actuals — one vendor, one season, half-PPR.

  | rank gap | QB | RB | WR | TE | K |
  |---|---|---|---|---|---|
  | 1 | 52% | 51% | 48% | 49% | 47% |
  | 3 | 51% | 53% | 48% | 52% | 50% |
  | 8 | 58% | 59% | 54% | 56% | 48% |
  | 12 | 59% | 64% | 56% | 64% | 51% |
  | 30 | – | 84% | 67% | 81% | – |

  Three readings. Adjacent ranks are a coin flip, and several score *below* 50%.
  Kickers never separate at any gap. And discrimination is weakest exactly where
  a waiver board lives — outside the top 36, a gap of 3 scores RB 51%, WR 47%,
  TE 46% — because everyone good is already rostered.

  RANK GAP RATHER THAN A POINTS THRESHOLD. The obvious alternative is 'gaps
  under about four points are noise', taken from median |projection − actual|.
  It is wrong twice. Error is not constant: it grows with the projection, near
  `1.4 × sqrt(projection)`, which is what points built out of count events do.
  And it is not scoring-invariant: double a league's weights and projections
  double while the square root grows by root two, so a constant calibrated on
  half-PPR is silently wrong in every other league. A rank gap needs no error
  model and is identical under any scoring config.

  WHAT IS DELIBERATELY NOT ANSWERED. Two ranks at different positions are two
  different scales, so a cross-position pair gets no verdict — WR8 against RB19
  is not a gap of 11. Neither does a position the table never covered (DST). In
  both cases `separation` is nil and the caller keeps whatever it did before
  this namespace existed: absence of a measurement is not evidence of noise, and
  claiming 'too close to call' without having measured it would be the same
  overclaim in the opposite direction.")

(def win-rates
  "The table above as data, keyed `[position gap]`, so re-deriving it later is a
  data change rather than an archaeology exercise. Nothing reads this — it is
  the provenance for `separating-gaps`, which is what the code consumes."
  {"QB" {1 0.52 3 0.51 8 0.58 12 0.59}
   "RB" {1 0.51 3 0.53 8 0.59 12 0.64 30 0.84}
   "WR" {1 0.48 3 0.48 8 0.54 12 0.56 30 0.67}
   "TE" {1 0.49 3 0.52 8 0.56 12 0.64 30 0.81}
   "K"  {1 0.47 3 0.50 8 0.48 12 0.51}})

(def slight-threshold 0.55)
(def clear-threshold 0.65)

(def separating-gaps
  "Position -> the smallest *measured* gap whose win rate clears each threshold.

  Read straight off `win-rates` with no interpolation, because five sampled gaps
  do not support a curve. RB at a gap of 12 scores 64% and so stays `:slight`
  rather than being rounded up to clear; 30 is the next measured point.

  nil means the table never got there — QB tops out at 59%, and K never leaves
  the coin-flip band at all. A position absent from this map was never measured;
  see the ns docstring on why that is not the same answer."
  {"QB" {:slight 8  :clear nil}
   "RB" {:slight 8  :clear 30}
   "WR" {:slight 12 :clear 30}
   "TE" {:slight 8  :clear 30}
   "K"  {:slight nil :clear nil}})

(defn level
  "`:clear`, `:slight` or `:coin-flip` for a rank gap at a position, or nil when
  the position was never measured."
  [position gap]
  (when-let [{:keys [slight clear]} (get separating-gaps position)]
    (cond
      (and clear (>= gap clear))   :clear
      (and slight (>= gap slight)) :slight
      :else                        :coin-flip)))

(defn coin-flip?
  "Did the measurement look at this pair and decline to separate them?

  False for a pair it never looked at — see `separation` on the three refusals.
  A predicate rather than the caller testing the keyword, so `:coin-flip` stays
  inside the namespace that defines the bands."
  [sep]
  (= :coin-flip (:level sep)))

(defn separation
  "`{:level :clear|:slight|:coin-flip :gap n}` for two players, or nil.

  nil in the three cases that are not a verdict: either side has no weekly rank,
  the two play different positions, or the position was never measured. A caller
  must keep those apart from `:coin-flip`, which is a measured claim."
  [a b]
  (let [ra (:week-pos-rank a)
        rb (:week-pos-rank b)]
    (when (and (number? ra) (number? rb)
               (= (:position a) (:position b)))
      (let [gap (abs (- ra rb))]
        (when-let [lvl (level (:position a) gap)]
          {:level lvl :gap gap})))))
