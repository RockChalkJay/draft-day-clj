(ns draft-day.scoring
  "Shared scoring logic for the backend and frontend.

  A player stores projected stats under `:stats`, and a scoring config is a
  `{stat-key weight}` map over those same keys. `:points` is the weighted sum of
  the player's stat line. The config is shared in `cljc` so the browser can seed
  the editor without any async fetch.")

(def ^:private stated-only
  "Stat keys a host may expose but no preset prices.
  These stay at `0.0` in the built-in presets so the three preset formats remain
  byte-identical while the league import can still accept a fuller scoring map."
  [;; passing
   :pass_cmp :pass_fd :pass_cmp_40p :pass_td_40p :pass_td_50p :pass_int_td
   :bonus_pass_cmp_25 :bonus_pass_yd_300 :bonus_pass_yd_400
   ;; rushing
   :rush_fd :rush_40p :rush_td_40p :rush_td_50p
   :bonus_rush_att_20 :bonus_rush_yd_100 :bonus_rush_yd_200
   ;; receiving
   :rec_fd :rec_20_29 :rec_30_39 :rec_40p :rec_td_40p :rec_td_50p
   :bonus_rec_yd_100 :bonus_rec_yd_200
   ;; from scrimmage, rushing and receiving together
   :bonus_rush_rec_yd_100 :bonus_rush_rec_yd_200
   ;; `:fum` is every fumble, where `:fum_lost` is only the ones that turned over
   :fum :fum_rec_td
   ;; the misses the presets do not already price, by distance
   :fgmiss :fgmiss_0_19 :fgmiss_20_29 :fgmiss_30_39
   ;; a defense's points and yards allowed, one bucket per game
   :pts_allow_0 :pts_allow_1_6 :pts_allow_7_13 :pts_allow_14_20
   :pts_allow_21_27 :pts_allow_28_34 :pts_allow_35p
   :yds_allow_0_100 :yds_allow_100_199 :yds_allow_200_299 :yds_allow_300_349
   :yds_allow_350_399 :yds_allow_400_449 :yds_allow_450_499 :yds_allow_500_549
   :yds_allow_550p
   ;; special teams, and the defense's own share of them
   :def_2pt :def_3_and_out :def_4_and_stop :def_kr_yd :def_pr_yd
   :def_st_ff :def_st_fum_rec :def_st_td :st_ff :st_fum_rec :st_td])

(defn- preset [reception-pts]
  (merge
   (zipmap stated-only (repeat 0.0))
   {:pass_yd 0.04 :pass_td 4.0 :pass_int -2.0 :pass_2pt 0.0
    :rush_yd 0.1 :rush_td 6.0 :rush_2pt 0.0
    :rec reception-pts :rec_yd 0.1 :rec_td 6.0 :rec_2pt 0.0
    :fum_lost -2.0
    ;; Every host scores a field goal by how far it was kicked, so the
    ;; near-universal 3/4/5 grid is what a preset means by "field goals".
    :fgm 3.0 :fgm_0_19 3.0 :fgm_20_29 3.0 :fgm_30_39 3.0
    :fgm_40_49 4.0 :fgm_50p 5.0
    ;; A miss costs nothing unless a league says otherwise.
    :fgmiss_40_49 0.0 :fgmiss_50p 0.0 :xpmiss 0.0
    :xpm 1.0 :blk_kick 0.0
    ;; team defense (linear stats only)
    :sack 1.0 :int 2.0 :fum_rec 2.0 :ff 1.0 :def_td 6.0 :safe 2.0}))

(def presets
  {:standard (preset 0.0)
   :half-ppr (preset 0.5)
   :ppr      (preset 1.0)})

(def stat-keys
  "Every stat key the custom scoring editor and league import may touch."
  (vec (keys (:standard presets))))

(defn usable-weight
  "Coerce a weight to a finite number, or `0.0` if it is unusable.
  This prevents NaN/Infinity values from a blank custom-scoring input from
  poisoning the whole board."
  [x]
  (if (and (number? x)
           #?(:clj  (let [d (double x)]
                      (not (or (Double/isNaN d) (Double/isInfinite d))))
              :cljs (js/isFinite x)))
    (double x)
    0.0))

(def formats
  "The scoring formats that vendor columns are published against, in reception order."
  [:standard :half-ppr :ppr])

(defn format-label
  "Build a vendor-column label for a source and scoring format.
  Example: `(format-label :fantasypros/ecr :half-ppr)` ->
  `:fantasypros/ecr-half-ppr`. The same label scheme is used by ingestion and the browser."
  [source fmt]
  (keyword (namespace source) (str (name source) "-" (name fmt))))

(defn resolve-config
  "Coerce a scoring value into a `{stat weight}` map.
  Accepts a preset keyword, its string form, or a custom weight map. This keeps
  browser and server resolution in sync for the same scoring value."
  [s]
  (cond
    (map? s)                      s
    (or (string? s) (keyword? s)) (get presets (keyword s) (:ppr presets))
    :else                         (:ppr presets)))

(def ^:private half-ppr-cutoff 0.25)
(def ^:private ppr-cutoff 0.75)

(defn format-of
  "Choose the published vendor format closest to the scoring config.
  The nearest format is selected from the receiving weight, since custom leagues
  do not carry a named format of their own."
  [scoring]
  (let [rec (usable-weight (:rec scoring))]
    (cond (< rec half-ppr-cutoff) :standard
          (< rec ppr-cutoff)      :half-ppr
          :else                   :ppr)))

(def ^:private projected-stats
  "Stats that can materially affect a player's projected points.
  These are the keys either published on the season line or carried forward from
  the weekly line after the season fill; they are the only extra stats that can
  meaningfully move a board."
  #{;; the season line's own
    :pass_cmp :pass_fd :pass_int_td :rush_fd
    :rec_fd :rec_20_29 :rec_30_39 :rec_40p
    ;; the weekly line's, reaching the season horizon through the fill
    :pass_cmp_40p :rush_40p :fum :fgmiss_30_39 :st_td :def_kr_yd :def_pr_yd
    :pts_allow_14_20 :pts_allow_21_27 :pts_allow_28_34
    :yds_allow_200_299 :yds_allow_300_349 :yds_allow_350_399})

(def unprojected-stats
  "Stats that the app should ignore when evaluating whether a config scores anything.
  These are either absent from the player data or intentionally not used in the
  season projection horizon, so they cannot meaningfully affect points."
  (into #{} (remove projected-stats) stated-only))

(def fg-buckets
  "Made-field-goal buckets that supersede the flat `:fgm` weight.
  If the config weights any bucket, the flat `:fgm` weight is removed to avoid
  double-counting one kick."
  [:fgm_0_19 :fgm_20_29 :fgm_30_39 :fgm_40_49 :fgm_50p])

(def pts-allow-cap
  "The most points allowed that gets a count of its own; a worse game counts as
  this one. An open-ended top band runs up to here."
  80)

(defn pts-allow-count-key
  "The stat key counting games in which a defense allowed exactly `n` points,
  e.g. `:pts_allow_at_17`. A league that grades points allowed in bands of its
  own weights these, because the host's fixed `:pts_allow_*` buckets cannot say
  which band a game fell in."
  [n]
  (keyword (str "pts_allow_at_" (min pts-allow-cap (max 0 (long n))))))

(def pts-allow-bucket-points
  "The points allowed a host's fixed bucket stands for, where the projection
  states only the bucket. Sleeper's own buckets, at their middle."
  {:pts_allow_0 0 :pts_allow_1_6 3 :pts_allow_7_13 10 :pts_allow_14_20 17
   :pts_allow_21_27 24 :pts_allow_28_34 31 :pts_allow_35p 38})

(defn with-pts-allow-counts
  "Pure: a projected stat line -> the same line with its points-allowed buckets
  restated as per-score counts (see `pts-allow-count-key`). The projection
  states a bucket and not a score, so each bucket counts at its middle."
  [stats]
  (reduce-kv (fn [acc k pts]
               (let [n (get stats k)]
                 (if (and (number? n) (pos? n))
                   (update acc (pts-allow-count-key pts) (fnil + 0.0) (double n))
                   acc)))
             stats
             pts-allow-bucket-points))

(defn pts-allow-game
  "Pure: one played game's real points allowed -> its count stat, or `{}` for a
  line that has none (every player but a defense)."
  [pts]
  (if (number? pts)
    {(pts-allow-count-key (Math/round (double pts))) 1.0}
    {}))

(defn clean-bands
  "Pure: a points-allowed band list, or nil, made safe to score. A band is
  `{:lo :hi :points}` with `:hi` nil for an open top. Anything malformed is
  dropped, and the list is capped so a client cannot amplify per-player work."
  [bands]
  (let [ok (->> (when (sequential? bands) bands)
                (filter (fn [{:keys [lo hi points]}]
                          (and (number? lo) (number? points)
                               (or (nil? hi) (and (number? hi) (<= lo hi))))))
                (take 20)
                (mapv (fn [{:keys [lo hi points]}]
                        {:lo (long lo) :hi (some-> hi long)
                         :points (usable-weight points)})))]
    (when (seq ok) ok)))

(defn band-weights
  "Pure: a band list -> `{count-key points}`, one entry per score a band covers.
  The first band to cover a score wins."
  [bands]
  (reduce (fn [acc {:keys [lo hi points]}]
            (reduce (fn [acc n]
                      (let [k (pts-allow-count-key n)]
                        (if (contains? acc k) acc (assoc acc k points))))
                    acc
                    (range (max 0 lo) (inc (min pts-allow-cap (or hi pts-allow-cap))))))
          {}
          bands))

(defn scores-anything?
  "True when the config has at least one usable weight that can move points.
  An empty or all-zero config is treated as a non-league configuration rather
  than a real scoring setup."
  [scoring]
  (boolean (or (some #(not (zero? (usable-weight %)))
                     (vals (apply dissoc scoring :pts-allowed-bands
                                  unprojected-stats)))
               (some #(not (zero? (usable-weight (:points %))))
                     (clean-bands (:pts-allowed-bands scoring))))))

(defn scores-by-distance?
  "True when the config weights field goals by distance instead of using the flat `:fgm` key."
  [scoring]
  (boolean (some #(not (zero? (usable-weight (get scoring %)))) fg-buckets)))

(defn resolve-buckets
  "Remove the flat `:fgm` weight when the config scores field goals by distance,
  and restate `:pts-allowed-bands` as weights on the per-score counts. This is a
  config-level normalization shared by all players so a league does not
  double-count a made kick, and it is idempotent."
  [scoring]
  (let [bands (:pts-allowed-bands scoring)]
    (cond-> scoring
      (scores-by-distance? scoring) (dissoc :fgm)
      (contains? scoring :pts-allowed-bands)
      (-> (dissoc :pts-allowed-bands)
          (merge (band-weights (clean-bands bands)))))))

(defn bounded-config
  "Pure: a scoring config reduced to the keys the engine reads — the stat keys
  and a cleaned band list — so an oversized client map cannot amplify
  per-player scoring."
  [scoring]
  (let [bands (clean-bands (:pts-allowed-bands scoring))]
    (cond-> (select-keys scoring stat-keys)
      bands (assoc :pts-allowed-bands bands))))

(defn resolved-points
  "Score a player using an already-normalized config.
  This is the low-level weighted-sum helper; callers typically run it after
  `resolve-buckets` has removed the flat FGM overlap."
  [player scoring]
  (let [stats (:stats player)]
    (reduce-kv (fn [acc stat weight]
                 (let [w (usable-weight weight)]
                   (if (zero? w)
                     acc
                     (+ acc (* w (usable-weight (get stats stat 0)))))))
               0.0 scoring)))

(defn player-points
  "Score one player with the provided config, defaulting any missing stat to 0."
  [player scoring]
  (resolved-points player (resolve-buckets scoring)))

(defn with-points
  "Return board with a :points value on each player."
  [board scoring]
  (let [scoring (resolve-buckets scoring)]
    (mapv #(assoc % :points (resolved-points % scoring)) board)))
