(ns draft-day.benchmark.ros-recent
  "Does weighting a player's last games into his rest-of-season rate predict
  the rest of his season better than his whole season does?

  Research, not shipped — it settles `ros/RECENT-WEIGHT`. For each past season
  and each as-of week it replays what `nflverse-weekly/accumulate` would have
  known then, predicts points per game, and scores that against the points per
  game the player actually put up in the weeks after.

  Three families of prediction, all against the whole-season rate (`w = 0`):

  - **points** blends recent points per game straight in.
  - **gate** does the same, but only for a player whose opportunity (targets
    plus carries per game) over the window is at least `T` times his season's.
  - **opportunity** keeps his season points per opportunity and blends only the
    opportunity rate, so recency comes from volume and not from touchdowns.

  It leaves the preseason prior out on purpose: the prior is a vintage
  projection, the scarce input, and it shrinks both arms of a comparison the
  same way. What is measured is the recency question alone, so read a winning
  weight as an upper bound on what the full blend should take.

  Players with three games and six points a game or more, receiving positions
  only (the opportunity columns say nothing of a quarterback), weighted by the
  games they went on to play. The standard error is a bootstrap over *players*,
  because one player asked about at five as-of weeks is not five independent
  observations. A gate is reported with the number of asks it fired on.

      lein run -m draft-day.benchmark.ros-recent -- --seasons 2022,2023,2024,2025"
  (:require [clojure.string :as str]
            [draft-day.db :as db]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.nflverse-weekly :as weekly]
            [draft-day.scoring :as scoring]))

(def ^:private as-of-weeks [4 6 8 10 12])
(def ^:private last-week 17)
(def ^:private min-games 3)
(def ^:private min-ppg 6.0)
(def ^:private surging 1.4)

(defn per-game
  "A line's points per game under `cfg`, or nil with no games."
  [cfg {:keys [games stats]}]
  (when (pos? (or games 0))
    (/ (scoring/resolved-points {:stats stats} cfg) games)))

(defn opportunities
  "Targets plus carries per game over a line, or nil with no games."
  [{:keys [games usage]}]
  (when (pos? (or games 0))
    (/ (+ (double (get usage :targets 0.0)) (double (get usage :carries 0.0))) games)))

(defn asks
  "One map per receiving-position player asked about at `as-of`: his season and
  recent rates as of then, and the points per game he went on to score."
  [cfg positions rows as-of]
  (let [known  (weekly/accumulate (weekly/season-rows rows as-of))
        future (->> (weekly/season-rows rows last-week)
                    (filter (fn [[_ w _]] (> w as-of)))
                    (reduce (fn [acc [g w row]] (update acc g (fnil conj []) [w row])) {}))]
    (into []
          (keep (fn [[gsis line]]
                  (let [season (:nflverse/season-to-date line)
                        recent (:nflverse/recent line)
                        after  (get future gsis)
                        s-ppg  (per-game cfg season)
                        r-ppg  (per-game cfg recent)
                        s-opp  (opportunities season)
                        r-opp  (opportunities recent)]
                    (when (and s-ppg r-ppg s-opp r-opp (pos? s-opp) after
                               (db/receiving-position? (get positions gsis))
                               (>= (:games season) min-games)
                               (>= s-ppg min-ppg))
                      {:id gsis :as-of as-of :games (count after)
                       :season s-ppg :recent r-ppg
                       :ppo (/ s-ppg s-opp) :s-opp s-opp :r-opp r-opp
                       :trend (/ r-opp s-opp)
                       :actual (/ (reduce + (map #(scoring/resolved-points {:stats (:stats %)} cfg)
                                                 (weekly/game-log after)))
                                  (count after))}))))
          known)))

(defn points-arm [w] (fn [{:keys [season recent]}] (+ (* (- 1.0 w) season) (* w recent))))

(defn gate-arm [w t]
  (let [blend (points-arm w)]
    (fn [a] (if (>= (:trend a) t) (blend a) (:season a)))))

(defn opportunity-arm [w]
  (fn [{:keys [ppo s-opp r-opp]}] (* ppo (+ (* (- 1.0 w) s-opp) (* w r-opp)))))

(def arms
  (concat
   (for [w [0.25 0.5 1.0]] [(str "points " w) (points-arm w)])
   (for [t [1.25 1.5 2.0] w [0.25 0.5]] [(str "gate T=" t " w=" w) (gate-arm w t) t])
   (for [w [0.25 0.5 1.0]] [(str "opportunity " w) (opportunity-arm w)])))

(defn sse
  "Games-weighted squared error of predictor `f` over `asked`."
  [f asked]
  (reduce + (map (fn [{:keys [actual games] :as a}]
                   (let [e (- actual (f a))]
                     (* games e e)))
                 asked)))

(defn bootstrap-delta
  "Per-game-weighted change in squared error of `f` against the season rate,
  with the standard error from resampling players."
  [f asked n]
  (let [by-player (vec (vals (group-by :id asked)))
        total     (double (reduce + (map :games asked)))
        delta     (fn [rows] (- (sse f rows) (sse :season rows)))
        rng       (java.util.Random. 42)
        k         (count by-player)
        draw      (fn [] (delta (apply concat (repeatedly k #(nth by-player (.nextInt rng k))))))
        ds        (vec (repeatedly n draw))
        mean      (/ (reduce + ds) n)
        sd        (Math/sqrt (/ (reduce + (map #(Math/pow (- % mean) 2) ds)) (dec n)))]
    {:delta (/ (delta asked) total) :se (/ sd total)}))

(defn report [label asked]
  (let [g (double (reduce + (map :games asked)))]
    (println (format "\n%s: %d asks, %d players, season-only mse %.3f"
                     label (count asked) (count (distinct (map :id asked)))
                     (/ (sse :season asked) g)))
    (doseq [[name f t] arms]
      (let [fired (if t (count (filter #(>= (:trend %) t) asked)) (count asked))
            {:keys [delta se]} (bootstrap-delta f asked 500)]
        (println (format "  %-22s %+.3f ± %.3f  (%.1f se)  fired on %d"
                         name delta se (/ delta se) fired))))))

(defn -main [& args]
  (let [opts    (apply hash-map (map #(if (str/starts-with? % "--") (keyword (subs % 2)) %)
                                     (remove #{"--"} args)))
        seasons (map #(Long/parseLong %) (str/split (or (:seasons opts) "2022,2023,2024,2025") #","))
        cfg     (scoring/resolve-buckets (:ppr scoring/presets))
        asked   (into []
                      (mapcat (fn [season]
                                (if-let [rows (weekly/fetch-rows season)]
                                  (let [positions (into {} (keep (fn [r]
                                                                   (when-let [g (nflverse/gsis-id r)]
                                                                     [g (get r "position")])))
                                                        rows)]
                                    (mapcat #(asks cfg positions rows %) as-of-weeks))
                                  (do (println "no weekly file for" season) []))))
                      seasons)]
    (report "all receiving players" asked)
    ;; The promoted backup: a window whose volume is well above the season's.
    ;; Known at ask time, so selecting on it is not peeking.
    (report (str "volume surging (trend >= " surging ")")
            (filterv #(>= (:trend %) surging) asked))))
