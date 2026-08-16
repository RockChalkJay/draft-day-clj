(ns draft-day.benchmark.vintage
  "Assemble the board as it could have been known in August of season S, and
  refuse to run on a season where that claim does not hold.

  WHY A GATE AT ALL. A benchmark is only worth the paper it is printed on if the
  inputs were frozen before the season. If a source quietly updates its 'past'
  projections with in-season information, the model appears to predict the future
  and the whole exercise is theatre. Every source added here must clear this gate
  before its numbers are believed — including paid ones, which are no safer by
  virtue of costing money.

  WHY THE GATE IS BEHAVIOURAL. The obvious check — a `last_modified` timestamp —
  is useless for Sleeper. Every archived season is bulk re-stamped on the Monday
  after week 18, and all ~680 records for a season are written inside a 10-20
  second window (2024: span 10.5s across 684 distinct stamps). The stamp says
  January; the values are from August. So the gate tests what the numbers *do*:

  1. FLAT GAMES. A preseason projection gives everyone a full season (17 or 18).
     A snapshot contaminated with in-season data reflects games actually played
     and spreads out. This separates cleanly — Sleeper 2021-2025 sit at 95%+ on
     the modal `gp`, while 2019/2020 fall to ~23%.
  2. INJURED PLAYERS STILL PROJECTED. Advisory rather than pass/fail: the rank
     correlation between projection and realized games should be weak. A
     hindsight-contaminated source cannot project 354 points for a player who
     managed 117 in seven games.

  LEAKAGE ALLOWLIST. Vintage rows are built key-by-key from vintage sources
  rather than by stripping a live player map, because stripping fails open — a
  new field added upstream silently joins the board. Notably Sleeper's projection
  entries embed a *player object* whose team, injury status and experience are of
  unknown vintage (the record was last written after the season), so none of it
  is carried."
  (:require [draft-day.benchmark.sources.fantasypros-archive :as fp-archive]
            [draft-day.benchmark.sources.fantasypros-ecr :as fp-ecr]
            [draft-day.benchmark.sources.fftoday :as fftoday]
            [draft-day.replay.metrics :as replay-metrics]
            [draft-day.scoring :as scoring]
            [draft-day.benchmark.sources.ffcalculator :as ffc]
            [draft-day.benchmark.sources.ids :as ids]
            [draft-day.ingestion.player-ids :as player-ids]
            [draft-day.benchmark.sources.nflverse :as nflverse]
            [draft-day.benchmark.sources.sleeper :as sleeper]
            [draft-day.ingestion.match :as match]))

(def gp-flatness-threshold
  "Share of scored entries that must share the modal projected `gp`. Sleeper's
  clean seasons sit at 0.95+, its contaminated ones near 0.23; 0.85 sits in the
  empty space between and is not a tuned number."
  0.85)

(defn gp-flatness
  "Share of scored entries carrying the most common projected games value."
  [{:keys [gp-freq n]}]
  (if (or (nil? n) (zero? n) (empty? gp-freq))
    0.0
    (/ (double (apply max (vals gp-freq))) (double n))))

(defn preseason-snapshot?
  "Does this season's projection vintage look like a genuine preseason freeze?"
  [vintage]
  (>= (gp-flatness vintage) gp-flatness-threshold))

(defn gate
  "Judge a season's projection source. Returns
  {:season :pass? :gp-flatness :n :reason}, and on a failed fetch
  {:pass? false :fetch-error msg}.

  A fetch failure is reported as its own outcome, never folded into 'no
  projections published'. Sleeper answers 200-with-empty-stats for seasons it
  genuinely lacks, so the two are distinguishable — and conflating them would let
  an outage masquerade as a fact about history."
  [season]
  (try
    (let [d (sleeper/season-data season)
          v (:vintage d)
          f (gp-flatness v)]
      {:season      season
       :n           (:n v 0)
       :raw-entries (:raw-entries d 0)
       :with-stats  (:with-stats d 0)
       :gp-flatness f
       :modified    (:modified v)
       :pass?       (preseason-snapshot? v)
       :reason      (cond
                      (zero? (:n v 0))             "no projections published by Sleeper"
                      (>= f gp-flatness-threshold) "flat projected games — preseason freeze"
                      :else                        "projected games vary — in-season or backfilled")})
    (catch Exception e
      {:season season :pass? false :n 0
       :fetch-error (ex-message e)
       :reason (str "FETCH FAILED — " (ex-message e))})))

;; ---- assembly ----

(defn prior-usage
  "Prior-season usage keyed by GSIS id, from nflverse season S-1. Absent players
  (rookies, or anyone who did not play) simply have no entry; models default
  them rather than this namespace inventing a value."
  [season]
  (nflverse/usage (dec season)))

(defn with-biography
  "Attach draft capital and age, both settled before the season starts.

  Rookies are the point: a first-year player has no prior-season usage, so the
  board currently knows nothing about him beyond a projection. Draft position is
  the strongest thing that WAS known in August, and it is unambiguously
  leak-free — the draft is in April. Age likewise: a birthday does not move.

  `:rookie?` is derived rather than stored, so it stays correct for whichever
  season the row belongs to."
  [row season bio]
  (if-not bio
    row
    (cond-> row
      (:draft-overall bio) (assoc :draft/overall (:draft-overall bio))
      (:draft-round bio)   (assoc :draft/round (:draft-round bio))
      (:draft-year bio)    (assoc :draft/year (:draft-year bio)
                                  :rookie? (= (:draft-year bio) season))
      (:birth-year bio)    (assoc :age (- season (:birth-year bio))))))

(defn attach-biography
  "Add draft capital and age to an assembled board. Applied by every assembly
  path so the models see the same row shape regardless of projection source."
  [players season]
  (let [bio (ids/biography)]
    (mapv #(with-biography % season (get bio (:gsis-id %))) players)))

(defn vintage-row
  "Build one leak-free board row. Every key is named explicitly — see the
  allowlist note in the namespace docstring."
  [{:keys [player gsis-id adp usage outcome]}]
  (cond-> {:player-id   (:player-id player)
           :player-name (:player-name player)
           :position    (:position player)
           :gsis-id     gsis-id
           :stats       (:stats player)
           :adp         adp}
    usage   (merge {:prior/wopr            (:wopr usage)
                    :prior/target-share    (:target-share usage)
                    :prior/air-yards-share (:air-yards-share usage)
                    :prior/targets-per-game (:targets-per-game usage)
                    :prior/carries-per-game (:carries-per-game usage)
                    :prior/ppg             (:ppg usage)
                    :prior/games           (:games usage)})
    outcome (merge {:actual/stats (:stats outcome)
                    :actual/games (:games outcome)})))

(def scoring-positions
  "The benchmark pool. K/DST are excluded for the same reason the valuation engine
  prices them at $0, and nflverse has no per-player team-defense rows anyway."
  #{"QB" "RB" "WR" "TE"})

(defn assemble
  "The season-S board as of August of season S, with realized outcomes attached.

  opts:
    :pool-size  keep players inside this ADP (default 200) — the draftable
                universe. Players with no ADP are dropped: without a draft-time
                price there is no evidence they were draftable, and including
                them would let a model take credit for 'ranking' waiver fodder.
    :adp-source :ffc (default, reaches 2010) or :sleeper (2021+).

  Returns {:season :players [...] :gate {...}}."
  ([season] (assemble season {}))
  ([season {:keys [pool-size adp-source] :or {pool-size 200 adp-source :ffc}}]
   (let [players    (sleeper/players season)
         xwalk      (ids/crosswalk)
         name-xwalk (ids/name-resolver season)
         resolve-id (player-ids/resolver xwalk)
         outcomes   (nflverse/outcomes season)
         usage      (prior-usage season)
         ffc-adp    (when (= adp-source :ffc) (ffc/adp-by-gsis season name-xwalk))
         rows       (keep
                     (fn [p]
                       (let [gsis (resolve-id (:player-id p) nil)
                             adp  (if (= adp-source :ffc)
                                    (get ffc-adp gsis)
                                    (sleeper/adp-of p))]
                         (when (and (scoring-positions (:position p))
                                    adp (< adp pool-size))
                           (vintage-row {:player  p
                                         :gsis-id gsis
                                         :adp     adp
                                         :usage   (get usage gsis)
                                         :outcome (get outcomes gsis)}))))
                     players)]
     ;; An empty board is never a real answer — it means the ADP source resolved
     ;; to nothing, and the `adp` guard above turned that into a season with no
     ;; players instead of a stated reason. That is exactly how a key move under
     ;; `:sleeper/adp` went unnoticed. It fails the gate rather than throwing:
     ;; `gate` judges projections only, so a season Sleeper has no ADP for at all
     ;; (pre-2021) passes it, and throwing would kill a whole sweep over seasons
     ;; the caller already knows how to skip.
     {:season  season
      :gate    (if (empty? rows)
                 {:season season :pass? false
                  :reason (str "no player cleared the ADP gate — " (name adp-source)
                               " produced no ADP for this season")
                  :adp-source adp-source :pool-size pool-size
                  :universe (count players)}
                 (gate season))
      :players (attach-biography rows season)})))

(defn assemble-from-fp-archive
  "The season-S board using ARCHIVED FANTASYPROS projections instead of Sleeper's.

  Same shape as `assemble`, so models cannot tell the difference — which is the
  point: running the identical model over two independent projection sources is
  how you find out whether 'the projection' is a real signal or an artifact of
  one vendor (Sleeper's are RotoWire's, on every entry).

  Universe and pool still come from FFC's vintage ADP, because FantasyPros
  publishes a projection list, not a draftable universe, and its long tail would
  otherwise pad the pool with players nobody would draft."
  ([season] (assemble-from-fp-archive season {}))
  ([season {:keys [pool-size] :or {pool-size 200}}]
   (let [name-xwalk (ids/name-resolver season)
         resolve-fp (player-ids/fp-resolver (ids/fp-crosswalk) name-xwalk)
         outcomes   (nflverse/outcomes season)
         usage      (prior-usage season)
         adp        (ffc/adp-by-gsis season name-xwalk)
         {:keys [players captures]} (fp-archive/season-data season)
         rows (keep (fn [{:keys [fp-id player-name position stats]}]
                      (let [gsis (resolve-fp fp-id (match/key-for player-name position))
                            a    (get adp gsis)]
                        (when (and gsis a (< a pool-size) (scoring-positions position))
                          (vintage-row {:player  {:player-id   gsis
                                                  :player-name player-name
                                                  :position    position
                                                  :stats       stats}
                                        :gsis-id gsis
                                        :adp     a
                                        :usage   (get usage gsis)
                                        :outcome (get outcomes gsis)}))))
                    players)
         ;; A player can appear on more than one position page across seasons;
         ;; keep one row per player so the pool is not double counted.
         deduped (vals (into {} (map (juxt :gsis-id identity)) rows))]
     {:season  season
      :gate    {:season season
                :pass?  (= 4 (count captures))
                :captures captures
                :reason (if (= 4 (count captures))
                          (str "archived preseason captures: " (pr-str captures))
                          (str "incomplete preseason capture set: " (pr-str captures)))}
      :players (attach-biography deduped season)})))

(defn fftoday-gate
  "Behavioural vintage check for FFToday, which publishes no capture date.

  A preseason board cannot know who gets hurt, so projected points should be
  close to uncorrelated with realized games played. A backfilled board would show
  a strong positive relationship. Reported as a diagnostic with a permissive
  threshold — the decisive evidence is qualitative and documented in the source
  namespace (the 2018 board still ranks Le'Veon Bell third; he held out all
  season)."
  [rows]
  (let [pairs (keep (fn [r] (when (:actual/games r)
                              [(scoring/player-points r (:ppr scoring/presets))
                               (double (:actual/games r))]))
                    rows)
        rho   (when (> (count pairs) 10)
                (replay-metrics/spearman (mapv first pairs) (mapv second pairs)))]
    {:proj-vs-games-rho rho
     :pass? (or (nil? rho) (< rho 0.5))
     :reason (cond
               (nil? rho)     "too few resolved players to judge"
               (< rho 0.5)    (format "projection is ~uncorrelated with realized games (rho %.2f)" rho)
               :else          (format "projection tracks realized games (rho %.2f) — looks backfilled" rho))}))

(defn assemble-from-fftoday
  "The season-S board from FFToday's preseason projections — the deepest
  projection source, 2008-2025.

  Pool comes from FFC vintage ADP where it exists (2010+), keeping this directly
  comparable with the Sleeper path. For 2008-2009, where no ADP exists, the pool
  falls back to FFToday's own top-N per position by projected points, which is a
  legitimate preseason ordering but NOT the same universe — the gate says so."
  ([season] (assemble-from-fftoday season {}))
  ([season {:keys [pool-size] :or {pool-size 200}}]
   (let [name-xwalk (ids/name-resolver season)
         outcomes   (nflverse/outcomes season)
         usage      (prior-usage season)
         adp        (ffc/adp-by-gsis season name-xwalk)
         have-adp?  (seq adp)
         rows (keep (fn [{:keys [player-name position stats]}]
                      (when-let [gsis (name-xwalk (match/key-for player-name position))]
                        (when (scoring-positions position)
                          (let [a (get adp gsis)]
                            (when (or (not have-adp?) (and a (< a pool-size)))
                              (vintage-row {:player  {:player-id   gsis
                                                      :player-name player-name
                                                      :position    position
                                                      :stats       stats}
                                            :gsis-id gsis
                                            :adp     a
                                            :usage   (get usage gsis)
                                            :outcome (get outcomes gsis)}))))))
                    (fftoday/players season))
         deduped (vec (vals (into {} (map (juxt :gsis-id identity)) rows)))
         gate    (fftoday-gate deduped)]
     {:season  season
      :gate    (assoc gate :season season
                      :pool-source (if have-adp? "ffc-adp" "fftoday top-N (no vintage ADP)"))
      :players (attach-biography deduped season)})))

(defn assemble-from-ecr
  "The season-S board from archived FantasyPros expert-consensus rankings.

  Unlike every other path this needs no ADP list: the cheatsheet is itself an
  ordered draftable universe, which is what lets it reach 2011 — six seasons
  before Fantasy Football Calculator's ADP starts and ten before Sleeper's
  projections.

  Rows carry `:ecr` (rank, lower is better) and no `:stats`, so only models
  declaring `#{:ecr}` may use this path; `benchmark.core` enforces that."
  ([season] (assemble-from-ecr season {}))
  ([season {:keys [pool-size] :or {pool-size 200}}]
   (let [name-xwalk (ids/name-resolver season)
         outcomes   (nflverse/outcomes season)
         usage      (prior-usage season)
         {:keys [players timestamp]} (fp-ecr/season-data season)
         rows (keep (fn [{:keys [player-name position ecr]}]
                      (when-let [gsis (name-xwalk (match/key-for player-name position))]
                        (when (and (scoring-positions position) (<= ecr pool-size))
                          (assoc (vintage-row {:player  {:player-id   gsis
                                                         :player-name player-name
                                                         :position    position
                                                         :stats       {}}
                                               :gsis-id gsis
                                               :adp     ecr   ; ECR doubles as the draft-order prior
                                               :usage   (get usage gsis)
                                               :outcome (get outcomes gsis)})
                                 :ecr ecr))))
                    players)
         deduped (vals (into {} (map (juxt :gsis-id identity)) rows))]
     {:season  season
      :gate    {:season season
                :pass?  (boolean (seq deduped))
                :capture (some-> timestamp (subs 0 8))
                :reason (if (seq deduped)
                          (str "archived ECR cheatsheet captured " (some-> timestamp (subs 0 8)))
                          "no pre-kickoff ECR capture parsed (2019+ pages are JavaScript-rendered)")}
      :players (attach-biography deduped season)})))

(defn assemble-from-adp
  "The season-S board built WITHOUT Sleeper projections — universe, ordering and
  pool all come from Fantasy Football Calculator's vintage ADP, joined to
  nflverse outcomes and prior-season usage.

  This is what lets a consensus- or usage-based model be tested on 2010-2025
  instead of 2021-2025. Sleeper publishes nothing before 2018 (it answers 200
  with an empty stats map on all 3112 entries), so a projection model genuinely
  cannot go further back — but a model that never reads a projection should not
  inherit that limit.

  Rows carry no :stats, so `scoring/player-points` would score them 0. Only
  models declaring `#{:adp}` / `#{:adp :prior-usage}` may use this path;
  `benchmark.core` enforces that."
  ([season] (assemble-from-adp season {}))
  ([season {:keys [pool-size] :or {pool-size 200}}]
   (let [name-xwalk (ids/name-resolver season)
         outcomes   (nflverse/outcomes season)
         usage      (prior-usage season)
         rows (keep (fn [{:keys [match-key name position adp]}]
                      (when-let [gsis (name-xwalk match-key)]
                        (when (and (scoring-positions position) adp (< adp pool-size))
                          (vintage-row {:player  {:player-id   gsis
                                                  :player-name name
                                                  :position    position
                                                  :stats       {}}
                                        :gsis-id gsis
                                        :adp     adp
                                        :usage   (get usage gsis)
                                        :outcome (get outcomes gsis)}))))
                    (:players (ffc/season-data season)))]
     {:season  season
      :gate    {:season season :pass? (boolean (seq rows))
                :reason (if (seq rows)
                          "adp-only path — no projection gate needed"
                          "no vintage ADP published")}
      :players (attach-biography rows season)})))
