(ns draft-day.benchmark.ros-backtest
  "How well does the rest-of-season blend predict the rest of the season, and
  how strongly should it trust the preseason line?

  Research, not shipped: it is the evidence behind `ros/prior-games`, and the
  first in-season benchmark (`benchmark.report` scores preseason boards only).
  For each past season and each as-of week it rebuilds what `ros/blend` would
  have known after that week, predicts every player's points per game for the
  weeks that remained, and scores that against what he scored.

  Inputs are Sleeper's, as the app reads them: the vintage preseason season line
  (`benchmark.sources.sleeper`, behind the same leakage gate as the draft
  benchmark) and the weekly stat lines (`sleeper-actual/fetch-week`, cached under
  `data/benchmark_cache`). The prior is `ros/blend`'s own, so what is scored is
  the code that ships.

  Arms. `prior-arms` are prior strengths: how many games of evidence the
  preseason line is worth, against 6 as shipped; 1000 is the preseason line alone.
  The role arm predicts a player who has taken over a vacated role from the games
  since the change instead of the whole season, using the same snap-count rule as
  `rankings.role`, `role/change-point` and `role/took-over?`. It cannot tell an
  injury from a benching, because there is no historical injury data here, and
  the question it asks does not need the cause: whether a role the snaps say has
  changed predicts better from its new games.

  What it cannot score is the outlook prior `ros` now starts from. Sleeper
  revises a past week's line after the games, so no vintage weekly line exists for
  past seasons, and `tools.projections` is banking this season's. The August
  prior scored here is the weaker of the two, so a strength measured against it
  is a floor for the outlook's. Its season line is also sparser than production's:
  the live pipeline fills what it omits from the week-one line
  (`sleeper/complete-season-line`), which for a past season is already revised, so
  a quarterback's or a defense's prior here is missing stats and their strengths
  read low.

  Players who played at least two games after the as-of week and have either a
  game before it or a preseason line, weighted by those games. The standard error
  is a bootstrap over player-seasons, since one player asked about at ten as-of
  weeks is not ten independent observations.

      lein run -m draft-day.benchmark.ros-backtest -- --seasons 2021,2022,2023,2024,2025
      lein run -m draft-day.benchmark.ros-backtest -- --teams 10

  `--teams` sets the league size the hit rate counts starters for (12 by
  default, over `db/default-roster`); the errors, and so the prior strengths,
  do not depend on it."
  (:require [clojure.string :as str]
            [draft-day.benchmark.fetch :as fetch]
            [draft-day.benchmark.sources.sleeper :as pre-src]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.sleeper-actual :as actual]
            [draft-day.db :as db]
            [draft-day.rankings.replacement :as replacement]
            [draft-day.rankings.role :as role]
            [draft-day.rankings.ros :as ros]
            [draft-day.scoring :as scoring]))

(def prior-arms
  "Prior strengths in games. 1000 is the preseason line alone."
  [1 2 4 6 9 12 18 30 60 1000])

(def shipped 6)

(def role-arms
  "The prior strengths the role arm is compared at, among `prior-arms`."
  [1 4 6 12])

(def as-of-weeks (range 3 13))

(def season-games 17)

(def last-week 18)

(def min-games-after 2)

(def positions ["QB" "RB" "WR" "TE" "K" "DST"])

(def starter-keys
  "The roster key holding each position's dedicated starters per team."
  {"QB" :qb "RB" :rb "WR" :wr "TE" :te "K" :k "DST" :dst})

(def default-teams 12)

(def configs
  {:ppr      (scoring/resolve-buckets (:ppr scoring/presets))
   :half-ppr (scoring/resolve-buckets (:half-ppr scoring/presets))
   :standard (scoring/resolve-buckets (:standard scoring/presets))})

(defn canon-position [p] (if (= "DEF" p) "DST" p))

(defn weekly-rows
  "Every played-game row of a regular season, weeks 1 through `last-week`, with
  the team and position Sleeper listed him under that week. Disk-cached; a week
  that fails to fetch throws rather than shortening the season. The rows are
  `actual/entry->row`'s output, so the cache token below goes with that
  function's shape: bump it when `entry->row` changes."
  [season]
  (fetch/cached
   (fetch/cache-path "sleeper" "weekly-rows" "v1" season)
   (fn []
     (let [weeks (range 1 (inc last-week))
           got   (parallel/all (zipmap weeks (map (fn [w] #(actual/fetch-week season w)) weeks)))]
       (vec (for [w weeks
                  e (get got w)
                  :let [row (actual/entry->row e)]
                  :when row]
              (assoc row
                     :team (:team e)
                     :position (canon-position (get-in e [:player :position])))))))))

(defn totals
  "`{:games :stats}` over a player's rows."
  [rows]
  {:games (count rows)
   :stats (reduce (fn [acc {:keys [stats]}] (merge-with + acc stats)) {} rows)})

(defn points
  "A stat line's points under `cfg`."
  [stats cfg]
  (scoring/resolved-points {:stats stats} cfg))

(defn predict
  "`{cfg-key points-per-game}` that `ros/blend` predicts from a preseason line and
  what a player has done, at prior strength `k`."
  [{:keys [pre real projected k]}]
  (let [line (ros/blend {:pre pre :realized (:stats real) :played (:games real)
                         :games-remaining 1.0 :season-games season-games
                         :prior-games k :projected projected})]
    (into {} (map (fn [[c cfg]] [c (if line (points line cfg) 0.0)])) configs)))

(defn game-log-player
  "A player as `rankings.role` reads one: team, position and a game log."
  [rows]
  (let [last-row (last (sort-by :week rows))]
    {:player-id (:id last-row) :team (:team last-row) :position (:position last-row)
     :realized/game-log (actual/game-log rows)}))

(defn takers
  "`{player-id change-week}` for every player who had taken over a role by
  `as-of`, off snap counts alone: a teammate at his position held half his team's
  snaps, has been under a quarter of them for at least two weeks, and this player
  averaged a quarter or more since, and more than before."
  [rows-by-id as-of]
  (let [players (mapv game-log-player (vals rows-by-id))
        played  (role/team-weeks players)
        vacant  (reduce (fn [acc p]
                          (let [w (and (contains? role/role-positions (:position p))
                                       (role/change-point p (set (get played (:team p))) as-of))]
                            (if (and w (<= w (dec as-of)))
                              (update acc [(:team p) (:position p)]
                                      (fn [old] (if (and old (<= (:since old) w)) old {:since w :by (:player-id p)})))
                              acc)))
                        {}
                        players)]
    (into {}
          (keep (fn [p]
                  (let [{:keys [since by]} (get vacant [(:team p) (:position p)])]
                    (when (and since (not= by (:player-id p))
                               (role/took-over? p (set (get played (:team p))) since as-of))
                      [(:player-id p) since]))))
          players)))

(defn asks-at
  "One ask per eligible player at `as-of`: what the blend predicted at every prior
  strength, and what he went on to score per game, under every configured
  scoring."
  [{:keys [season rows-by-id pre-by-id projected]} as-of]
  (let [taker (takers (update-vals rows-by-id (fn [rs] (filterv #(<= (:week %) as-of) rs))) as-of)]
    (into []
          (keep
           (fn [[id rows]]
             (let [before (filterv #(<= (:week %) as-of) rows)
                   after  (filterv #(> (:week %) as-of) rows)
                   pos    (:position (last rows))
                   pre    (:stats (get pre-by-id id))
                   real   (totals before)]
               (when (and (>= (count after) min-games-after)
                          (contains? starter-keys pos)
                          (or (pos? (:games real)) (seq pre)))
                 (let [since    (get taker id)
                       since-r  (when since (totals (filterv #(>= (:week %) since) before)))
                       ppg      (let [t (totals after)]
                                  (into {} (map (fn [[c cfg]] [c (/ (points (:stats t) cfg) (:games t))])) configs))
                       arm      (fn [real* k] (predict {:pre pre :real real* :projected projected :k k}))]
                   (cond-> {:season season :as-of as-of :id id :position pos
                            :games (count after) :actual ppg
                            :pred (into {} (map (fn [k] [k (arm real k)])) prior-arms)}
                     since (assoc :since since
                                  :role-pred (into {} (map (fn [k] [k (arm since-r k)])) role-arms))))))))
          rows-by-id)))

(defn season-asks
  "Every ask of one season, over `as-of-weeks`."
  [season]
  (let [rows       (weekly-rows season)
        pre-by-id  (into {} (map (juxt :player-id identity)) (pre-src/players season))
        ctx        {:season season
                    :rows-by-id (group-by :id rows)
                    :pre-by-id pre-by-id
                    :projected (into #{} (mapcat (comp keys :stats)) (vals pre-by-id))}]
    (into [] (mapcat #(asks-at ctx %)) as-of-weeks)))

(defn loss
  "Games-weighted squared error of one prediction."
  [ask pred cfg]
  (let [e (- (get-in ask [:actual cfg]) (get pred cfg))]
    (* (:games ask) e e)))

(defn pred-at [ask k] (get-in ask [:pred k]))

(defn bootstrap
  "`{:delta :se}` of the games-weighted change in squared error that `f` gives
  each ask, per game, resampling player-seasons. `total-games` is the denominator
  a caller wants the change spread over."
  [asks f total-games n]
  (let [clusters (vec (vals (group-by (juxt :season :id) asks)))
        sums     (mapv (fn [as] (reduce + 0.0 (map f as))) clusters)
        k        (count clusters)
        rng      (java.util.Random. 42)
        draw     (fn [] (reduce + 0.0 (repeatedly k #(nth sums (.nextInt rng k)))))
        ds       (vec (repeatedly n draw))
        mean     (/ (reduce + ds) n)
        sd       (Math/sqrt (/ (reduce + (map #(Math/pow (- % mean) 2) ds)) (dec n)))]
    {:delta (/ (reduce + sums) total-games) :se (/ sd total-games)}))

(defn mse
  "Games-weighted mean squared error of prior strength `k` over `asks`."
  [asks cfg k]
  (/ (reduce + 0.0 (map #(loss % (pred-at % k) cfg) asks))
     (reduce + 0.0 (map :games asks))))

(defn starters
  "`{position n}` a league of `num-teams` starts over one (season, as-of week)
  group: the dedicated seats times the teams, plus the flex seats the position
  wins when the best of what is left fills them on what players actually scored
  (`replacement/flex-claims`), which is how the draft board allocates them."
  [group cfg num-teams roster]
  (let [board (map (fn [a] {:position (:position a) :score (get-in a [:actual cfg])}) group)
        flex  (replacement/flex-claims board num-teams roster :score)]
    (into {}
          (map (fn [[pos k]]
                 [pos (+ (* num-teams (long (or (get roster k) 0))) (get flex pos 0))]))
          starter-keys)))

(defn group-starters
  "`{[season as-of] {position n}}` over all of `asks`, so a position's top N
  follows the league size and lineup rather than a fixed count."
  [asks cfg num-teams roster]
  (update-vals (group-by (juxt :season :as-of) asks) #(starters % cfg num-teams roster)))

(defn hit-rate
  "Of the top `n` at a position by predicted points a game, how many finished in
  the top `n` by actual, averaged over every (season, as-of week) group. `n` is
  the position's entry in `group-starters`' map for that group."
  [asks cfg k starters-by-group]
  (let [groups (vals (group-by (juxt :season :as-of) asks))
        rates  (keep (fn [g]
                       (let [n (get-in starters-by-group [[(:season (first g)) (:as-of (first g))]
                                                          (:position (first g))])]
                         (when (and n (pos? n) (>= (count g) (* 2 n)))
                           (let [top (fn [f] (set (map :id (take n (sort-by f > g)))))
                                 p   (top #(get-in (pred-at % k) [cfg]))
                                 a   (top #(get-in % [:actual cfg]))]
                             (/ (count (filter p a)) (double n))))))
                     groups)]
    (when (seq rates) (/ (reduce + rates) (count rates)))))

(defn prior-report
  "One position's errors by prior strength under `cfg`, the best strength, and
  its paired change against the shipped one. Hit rates take `starters-by-group`."
  [pos asks cfg starters-by-group]
  (let [errs   (into {} (map (fn [k] [k (mse asks cfg k)])) prior-arms)
        best   (key (apply min-key val errs))
        total  (reduce + 0.0 (map :games asks))
        delta  (when-not (= best shipped)
                 (bootstrap asks #(- (loss % (pred-at % best) cfg) (loss % (pred-at % shipped) cfg)) total 500))]
    {:position pos :n (count asks) :players (count (distinct (map (juxt :season :id) asks)))
     :errors errs :best best :delta delta
     :hits (when (= :ppr cfg) (into {} (map (fn [k] [k (hit-rate asks cfg k starters-by-group)])) prior-arms))}))

(defn role-report
  "The role arm against the whole-season rate at each prior strength, on the
  players it fires for, and spread over every ask at his position."
  [pos asks cfg]
  (let [fired (filterv :since asks)
        total (reduce + 0.0 (map :games asks))
        fgam  (reduce + 0.0 (map :games fired))]
    (when (seq fired)
      {:position pos :fired (count fired) :players (count (distinct (map (juxt :season :id) fired)))
       :arms (into {}
                   (map (fn [k]
                          (let [d (fn [a] (- (loss a (get-in a [:role-pred k]) cfg) (loss a (pred-at a k) cfg)))]
                            [k {:season-mse (/ (reduce + 0.0 (map #(loss % (pred-at % k) cfg) fired)) fgam)
                                :role-mse   (/ (reduce + 0.0 (map #(loss % (get-in % [:role-pred k]) cfg) fired)) fgam)
                                :on-fired   (bootstrap fired d fgam 500)
                                :overall    (bootstrap fired d total 500)}])))
                   role-arms)})))

(defn fmt [x] (if (number? x) (format "%.2f" (double x)) "-"))

(defn print-priors [reports cfg]
  (println (format "\nPrior strength, %s: games-weighted MSE by games of evidence the preseason line is worth" (name cfg)))
  (println (format "  %-4s %6s %5s  %s  | best  vs shipped (%d)" "pos" "asks" "plyrs"
                   (str/join " " (map #(format "%6s" (if (= 1000 %) "pre" %)) prior-arms)) shipped))
  (doseq [{:keys [position n players errors best delta]} reports]
    (println (format "  %-4s %6d %5d  %s  | %-4s  %s"
                     position n players
                     (str/join " " (map #(format "%6s" (fmt (get errors %))) prior-arms))
                     (if (= 1000 best) "pre" best)
                     (if delta
                       (format "%+.3f ± %.3f (%.1f se)" (:delta delta) (:se delta) (/ (:delta delta) (:se delta)))
                       "shipped is best")))))

(defn print-hits [reports num-teams]
  (println (format "\nHit rate, ppr, %d teams: of the top N predicted at a position, how many finished top N" num-teams))
  (println (format "  %-4s %s" "pos" (str/join " " (map #(format "%6s" (if (= 1000 %) "pre" %)) prior-arms))))
  (doseq [{:keys [position hits]} reports]
    (println (format "  %-4s %s" position (str/join " " (map #(format "%6s" (fmt (some-> (get hits %) (* 100)))) prior-arms))))))

(defn print-roles [reports]
  (println "\nRole arm, ppr: players who took over a vacated role, predicted from the games since")
  (println "  change in squared error per game (negative: the role arm predicts better); on fired = over the")
  (println "  players it fires for, overall = spread over every ask at the position")
  (doseq [{:keys [position fired players arms]} reports]
    (println (format "  %s: fired on %d asks, %d player-seasons" position fired players))
    (doseq [k role-arms]
      (let [{:keys [season-mse role-mse on-fired overall]} (get arms k)]
        (println (format "    prior %-3d  season-rate mse %s  role mse %s  on fired %+.3f ± %.3f  overall %+.4f ± %.4f"
                         k (fmt season-mse) (fmt role-mse) (:delta on-fired) (:se on-fired)
                         (:delta overall) (:se overall)))))))

(defn -main [& args]
  (let [opts    (apply hash-map (map #(if (str/starts-with? % "--") (keyword (subs % 2)) %)
                                     (remove #{"--"} args)))
        seasons (map #(Long/parseLong %) (str/split (or (:seasons opts) "2021,2022,2023,2024,2025") #","))
        teams   (Long/parseLong (or (:teams opts) (str default-teams)))
        asks    (into [] (mapcat season-asks) seasons)
        by-pos  (group-by :position asks)
        order   positions
        ;; The hit rate is the one thing here that depends on the league: how
        ;; many of a position start. The errors are per player and do not.
        n-by-group (group-starters asks :ppr teams db/default-roster)]
    (println (format "%d asks, %d player-seasons, seasons %s" (count asks)
                     (count (distinct (map (juxt :season :id) asks))) (str/join "," seasons)))
    (doseq [cfg (keys configs)]
      (let [reports (mapv #(prior-report % (get by-pos %) cfg n-by-group) (filter by-pos order))]
        (print-priors reports cfg)
        (when (= :ppr cfg) (print-hits reports teams))))
    (print-roles (keep #(when (get by-pos %) (role-report % (get by-pos %) :ppr)) order))))
