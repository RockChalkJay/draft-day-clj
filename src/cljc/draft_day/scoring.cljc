(ns draft-day.scoring
  "Piece 0: configurable scoring -> a single :points value per player.

  A player carries its projected stat line under :stats {stat-key value}. A
  ScoringConfig is a flat {stat-key weight} map over those same keys, so points
  is simply Σ(stat * weight). Using Sleeper's stat keys (`pass_yd`, `rush_td`,
  `rec`, ...) means the league's Sleeper scoring_settings map in directly, and a
  position that lacks a stat (a TE has no :pass_yd) contributes 0 — never an
  error.

  Shared cljc rather than backend-only (the `db.cljc` precedent) because the
  frontend needs `presets` and `stat-keys` synchronously: seeding the custom
  scoring editor from an async `/api/scoring/presets` fetch meant a manager who
  picked Custom before it landed got a nil scoring config and a Settings page
  that threw. One definition, both sides, no fetch.")

(defn- preset [reception-pts]
  {:pass_yd 0.04 :pass_td 4.0 :pass_int -2.0 :pass_2pt 0.0
   :rush_yd 0.1 :rush_td 6.0 :rush_2pt 0.0
   :rec reception-pts :rec_yd 0.1 :rec_td 6.0 :rec_2pt 0.0
   :fum_lost -2.0
   ;; kicking
   :fgm 3.0 :xpm 1.0 :blk_kick 0.0
   ;; team defense (linear stats only)
   :sack 1.0 :int 2.0 :fum_rec 2.0 :ff 1.0 :def_td 6.0 :safe 2.0})

(def presets
  {:standard (preset 0.0)
   :half-ppr (preset 0.5)
   :ppr      (preset 1.0)})

(def stat-keys
  "Every stat key the custom scoring editor and league import may touch."
  (vec (keys (:standard presets))))

(defn usable-weight
  "`x` as a number we can safely multiply by, or 0.0.

  Anything unusable costs that one stat its contribution rather than taking the
  whole board down. This matters because a cleared input box in the custom
  scoring editor sends NaN, which `JSON.stringify` writes as null: the previous
  `(zero? weight)` threw on it, surfaced as an HTTP 400, and blanked the board."
  [x]
  (if (and (number? x)
           #?(:clj  (let [d (double x)]
                      (not (or (Double/isNaN d) (Double/isInfinite d))))
              :cljs (js/isFinite x)))
    (double x)
    0.0))

(def formats
  "The scoring formats outside vendors publish against, in reception order.
  FantasyPros' ECR and auction calculator and Sleeper's ADP all vary by these."
  [:standard :half-ppr :ppr])

(defn format-label
  "The `:sources` label for one vendor column at one scoring format:
  `(format-label :fantasypros/ecr :half-ppr)` -> `:fantasypros/ecr-half-ppr`.

  Shared cljc rather than living with ingestion because both ends of the wire
  need it: ingestion writes these labels into the universe's `:sources`, and the
  browser reads them back out of `/api/players` to say which vendor columns its
  league's format is missing. A second hand-written copy of the scheme is a
  rename away from turning that check into a permanent silent no-op."
  [source fmt]
  (keyword (namespace source) (str (name source) "-" (name fmt))))

(defn resolve-config
  "Coerce a scoring field — a preset keyword, its string spelling, or a full
  {stat weight} map — into a {stat weight} map.

  The browser stores `(:config :scoring)` as either a preset keyword or a custom
  map, and the API accepts both plus the string spellings JSON leaves behind.
  Three copies of this `cond` had already drifted: the client resolved the string
  \"standard\" to *PPR* while the server resolved it to Standard, which is enough
  to have the Settings page warn about the wrong format's vendor columns."
  [s]
  (cond
    (map? s)                      s
    (or (string? s) (keyword? s)) (get presets (keyword s) (:ppr presets))
    :else                         (:ppr presets)))

(def ^:private half-ppr-cutoff 0.25)
(def ^:private ppr-cutoff 0.75)

(defn format-of
  "The published format closest to this scoring config.

  Vendor columns are published per format and a custom config does not name one,
  but receptions are what actually separate the three — so the :rec weight picks
  the nearest. A custom 0.4-per-catch league reads as half-PPR; a 1.5-per-catch
  TE-premium league reads as PPR."
  [scoring]
  (let [rec (usable-weight (:rec scoring))]
    (cond (< rec half-ppr-cutoff) :standard
          (< rec ppr-cutoff)      :half-ppr
          :else                   :ppr)))

(def unprojected-stats
  "Stat keys we score but that Sleeper's projections never carry — checked
  against both the live universe and the bundled sample, where all four appear
  zero times. Kickers are projected for extra points but not field goals, and
  team defenses for sacks/interceptions/fumble recoveries but not forced fumbles,
  defensive touchdowns or safeties. A weight on these cannot move any player's
  points, so the editor shows them but will not pretend they are editable."
  #{:fgm :ff :def_td :safe})

(defn scores-anything?
  "True when at least one weight can actually move a player's points. An empty
  or all-zero config is not a league, it is a board where everyone is worth $0.

  `unprojected-stats` do not count. They are the only weights the editor will
  not let you clear, so zeroing every weight a manager *can* reach leaves them
  standing — and taking them as evidence of a real league let exactly the
  all-zero board this predicate exists to catch through."
  [scoring]
  (boolean (some #(not (zero? (usable-weight %)))
                 (vals (apply dissoc scoring unprojected-stats)))))

(defn player-points
  "Σ over the scoring map of (stat weight * player's projected stat), defaulting
  missing stats to 0."
  [player scoring]
  (let [stats (:stats player)]
    (reduce-kv (fn [acc stat weight]
                 (let [w (usable-weight weight)]
                   (if (zero? w)
                     acc
                     (+ acc (* w (usable-weight (get stats stat 0)))))))
               0.0 scoring)))

(defn with-points
  "Return board with a :points value on each player."
  [board scoring]
  (mapv #(assoc % :points (player-points % scoring)) board))
