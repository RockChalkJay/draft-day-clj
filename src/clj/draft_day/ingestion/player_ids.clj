(ns draft-day.ingestion.player-ids
  "Cross-source player identity: Sleeper player_id -> GSIS id, plus the other
  public ids that hang off the same crosswalk.

  GSIS is the NFL's own identifier (Game Statistics and Information System,
  e.g. \"00-0034857\"). It is what nflverse and most public football data anchor
  on, so carrying it turns a name-key guess into an exact join.

  Two things make a naive Sleeper->GSIS join fail badly, and both are
  load-bearing:

  1. Sleeper's own `gsis_id` field is present on only about a third of
     draftable players.
  2. When present it is frequently prefixed with a space (\" 00-0035229\"), so
     an untrimmed equality join silently drops those too.

  Untrimmed Sleeper-only joins land around 28% coverage. Trimming plus a
  DynastyProcess fallback reaches ~99%.

  DynastyProcess's `db_playerids.csv` is that fallback: a free, keyless
  crosswalk of sleeper/gsis/fantasypros/espn/pfr ids. It is a *current* file
  (no per-season vintage), which is fine because player identity does not
  change with the season — only their stats do.

  Everything here is pure: it transforms already-fetched rows. Where those rows
  come from differs by caller — the app reads a pinned snapshot committed under
  resources/ so derivation stays offline and reproducible, while the benchmark
  harness fetches the current file live. Same builders, two vintages."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.ingestion.match :as match]))

(def playerids-url
  "https://github.com/dynastyprocess/data/raw/master/files/db_playerids.csv")

(def snapshot-resource
  "The pinned crosswalk the app derives ids from. Committed rather than fetched
  so derivation is pure, offline and identical between runs — and so the data
  survives DynastyProcess itself going away. Refreshed deliberately by
  `draft-day.tools.refresh-player-ids`, never as a side effect of ingestion."
  "player_ids.edn")

(def snapshot-schema-version 1)

(defn clean-gsis
  "Trim a GSIS id, returning nil for blank. Sleeper emits leading whitespace."
  [s]
  (let [t (some-> s str str/trim)]
    (when-not (str/blank? t) t)))

(defn crosswalk-from-rows
  "Pure: DynastyProcess rows -> {sleeper-id gsis-id}, skipping rows missing either."
  [rows]
  (into {}
        (keep (fn [r]
                (let [sid  (some-> (get r "sleeper_id") str str/trim)
                      gsis (clean-gsis (get r "gsis_id"))]
                  (when (and gsis (not (str/blank? sid))) [sid gsis]))))
        rows))

(def nickname-aliases
  "Formal name -> the common names ADP and ranking sources actually print.

  Fantasy Football Calculator and FantasyPros publish the name a drafter says
  out loud; DynastyProcess carries the one on the birth certificate. These are
  the mismatches that recur every season — measured at 2-6 skill players per
  season, all of them genuinely draftable."
  {"Marquise Brown"    ["Hollywood Brown"]
   "Gabriel Davis"     ["Gabe Davis"]
   "Kenneth Gainwell"  ["Kenny Gainwell"]
   "Chigoziem Okonkwo" ["Chig Okonkwo"]
   "Jeffery Wilson"    ["Jeff Wilson" "Jeff Wilson Jr."]
   "Joshua Palmer"     ["Josh Palmer"]
   "Michael Pittman"   ["Michael Pittman Jr."]})

(def fantasy-positions
  "Positions a match key may legitimately carry. DynastyProcess sometimes files a
  player under a junk position (Rondale Moore is \"XX\"), which breaks a
  position-keyed join on the position rather than the name."
  #{"QB" "RB" "WR" "TE" "K" "DST" "DEF" "PK"})

(defn name-candidates-from-rows
  "Pure: DynastyProcess rows -> {match-key [candidate ...]}, where a candidate is
  {:gsis-id :draft-year :exact?}.

  Names collide, and collapsing a key to one id silently picks the wrong player.
  Two distinct failures, both observed:

  1. THREE Mike Williamses are WRs (2005, 2010, 2017 drafts). Any era-blind rule
     picks one of them for every season.
  2. A junk-position row fanned out across the skill positions — needed because
     DynastyProcess files Rondale Moore as \"XX\" — let the 2009 CORNERBACK
     D.J. Moore and the 2012 SAFETY Michael Thomas outrank the receivers who
     share their names.

  Both produced players with no nflverse outcome row, scored as zero realized
  points, penalising whichever model ranked them. So candidates are kept and
  disambiguated at lookup time by `pick-candidate`, which knows the season and
  prefers a genuine position match."
  [rows]
  (let [year (fn [r] (let [v (some-> (get r "draft_year") str str/trim)]
                       (when (and v (not= "" v) (not= "NA" v))
                         (try (long (Double/parseDouble v)) (catch Exception _ nil)))))]
    (->> rows
         (mapcat (fn [r]
                   (let [gsis (clean-gsis (get r "gsis_id"))
                         nm   (get r "name")
                         pos  (get r "position")]
                     (when (and gsis nm pos)
                       (let [names  (cons nm (get nickname-aliases nm))
                             exact? (boolean (fantasy-positions pos))
                             ;; A junk position still registers under the skill
                             ;; positions so the player is findable, but marked
                             ;; inexact so it can never displace a real match.
                             poss   (if exact? [pos] ["QB" "RB" "WR" "TE"])]
                         (for [n names, p poss]
                           [(match/key-for n p)
                            {:gsis-id gsis :draft-year (year r) :exact? exact?}]))))))
         (reduce (fn [m [k c]] (update m k (fnil conj []) c)) {}))))

(defn pick-candidate
  "Choose among same-name candidates for a given season.

  Exact position matches beat fanned-out ones outright. Among those, the player
  whose draft year is the latest one NOT after the season is the one who was
  actually in the league — which is why this must be season-aware: resolving
  'Mike Williams' to the 2017 receiver is right for 2023 and wrong for 2010."
  [candidates season]
  (when (seq candidates)
    (let [in-era?  (fn [c] (and (:draft-year c) (<= (:draft-year c) season)))
          latest   (fn [cs] (last (sort-by :draft-year cs)))
          exact    (filter :exact? candidates)
          ;; Being in the league that season outranks a position match. Preferring
          ;; position first would resolve a 2010 board to a receiver drafted in
          ;; 2018 purely because his listed position was cleaner — reintroducing
          ;; the same zero-outcome bug in a different disguise.
          exact-era   (filter in-era? exact)
          any-era     (filter in-era? candidates)]
      (:gsis-id
       (or (latest exact-era)
           (latest any-era)
           (first (remove :draft-year exact))
           (first (remove :draft-year candidates))
           (first (sort-by :draft-year exact))
           (first (sort-by :draft-year candidates)))))))

(defn name-crosswalk-from-rows
  "Pure: DynastyProcess rows -> {match-key gsis-id}, for sources that publish only
  a name (Fantasy Football Calculator). Reuses `ingestion.match/key-for` so this
  join normalizes identically to the FantasyPros join the app already does.

  ERA-BLIND, and therefore wrong whenever a name collides — it just takes the
  first candidate. An earlier version claimed the file is ordered oldest-first so
  a later-row-wins rule picks the recent player; that is false (the 2017 Mike
  Williams appears at row 3488, the 2005 one at row 7917) and it silently
  resolved three productive 2023 receivers to retired or defensive namesakes.
  Prefer `name-resolver`, which is season-aware."
  [rows]
  (into {}
        (map (fn [[k cands]] [k (:gsis-id (first cands))]))
        (name-candidates-from-rows rows)))

(defn fp-crosswalk-from-rows
  "Pure: DynastyProcess rows -> {fantasypros-id gsis-id}.

  FantasyPros' archived pages carry their own player id in the row class
  (`mpb-player-11594` — Travis Kelce, matching DynastyProcess `fantasypros_id`
  11594), which is an exact join. But the column is only populated for about 70%
  of skill players, so callers must fall back to the name crosswalk; see
  `fp-resolver`."
  [rows]
  (into {}
        (keep (fn [r]
                (let [fp   (some-> (get r "fantasypros_id") str str/trim)
                      gsis (clean-gsis (get r "gsis_id"))]
                  (when (and gsis fp (not (str/blank? fp)) (not= "NA" fp))
                    [fp gsis]))))
        rows))

(defn biography-from-rows
  "Pure: DynastyProcess rows -> {gsis-id {:draft-year :draft-round :draft-pick
  :draft-overall :birth-year}}.

  Both facts are leak-free for any season: a player's draft position is settled
  in April and his birthday never moves. That matters most for ROOKIES, who by
  definition have no prior-season usage — today they enter the board carrying no
  information at all beyond a projection, and draft capital is the strongest
  thing anyone knew about them in August."
  [rows]
  (into {}
        (keep (fn [r]
                (let [gsis (clean-gsis (get r "gsis_id"))
                      num  (fn [k] (let [v (some-> (get r k) str str/trim)]
                                     (when (and v (not= "" v) (not= "NA" v))
                                       (try (long (Double/parseDouble v))
                                            (catch Exception _ nil)))))
                      birth (some-> (get r "birthdate") str str/trim)
                      byear (when (re-find #"^\d{4}" (str birth))
                              (Long/parseLong (subs birth 0 4)))]
                  (when gsis
                    (let [m (cond-> {}
                              (num "draft_year")  (assoc :draft-year (num "draft_year"))
                              (num "draft_round") (assoc :draft-round (num "draft_round"))
                              (num "draft_pick")  (assoc :draft-pick (num "draft_pick"))
                              (num "draft_ovr")   (assoc :draft-overall (num "draft_ovr"))
                              byear               (assoc :birth-year byear))]
                      (when (seq m) [gsis m]))))))
        rows))

(defn name-resolver
  "(fn [match-key] -> gsis-id-or-nil) for a specific season, over an already-built
  candidate index. Season-aware, so a shared name resolves to whoever was
  actually playing that year."
  [candidates season]
  (fn [k] (pick-candidate (get candidates k) season)))

(defn fp-resolver
  "Build (fn [fp-id match-key] -> gsis-id-or-nil): exact FantasyPros id first,
  then the name fallback for the ~30% with no id on file. `resolve-name` is a
  function (see `name-resolver`), not a map, so the fallback stays season-aware."
  [fp-xwalk resolve-name]
  (fn [fp-id match-key]
    (or (when fp-id (get fp-xwalk (str/trim (str fp-id))))
        (resolve-name match-key))))

(defn resolver
  "Build (fn [sleeper-id sleeper-gsis-field] -> gsis-id-or-nil) from a crosswalk.
  Prefers the player's own (trimmed) gsis field, falls back to the crosswalk."
  [xwalk]
  (fn [sleeper-id gsis-field]
    (or (clean-gsis gsis-field)
        (get xwalk (some-> sleeper-id str str/trim)))))

;; ---- the pinned snapshot ----

(defn blank->nil
  "Trim, treating DynastyProcess's literal \"NA\" as absent."
  [s]
  (let [t (some-> s str str/trim)]
    (when-not (or (str/blank? t) (= "NA" t)) t)))

(defn parse-long-field
  "DynastyProcess writes integers as floats (\"2018.0\"); nil when absent."
  [s]
  (when-let [t (blank->nil s)]
    (try (long (Double/parseDouble t)) (catch Exception _ nil))))

(defn snapshot-row
  "Pure: one DynastyProcess CSV row -> the projected crosswalk row, or nil when
  it carries no Sleeper id. Sleeper is the join key — the app's universe is the
  Sleeper universe, so a row we cannot reach from a Sleeper id is dead weight."
  [r]
  (when-let [sleeper (blank->nil (get r "sleeper_id"))]
    (let [birth (blank->nil (get r "birthdate"))]
      (into {}
            (remove (comp nil? val))
            {:sleeper       sleeper
             :gsis          (blank->nil (get r "gsis_id"))
             :fantasypros   (blank->nil (get r "fantasypros_id"))
             :espn          (blank->nil (get r "espn_id"))
             :pfr           (blank->nil (get r "pfr_id"))
             :name          (blank->nil (get r "name"))
             :position      (blank->nil (get r "position"))
             :draft-year    (parse-long-field (get r "draft_year"))
             :draft-round   (parse-long-field (get r "draft_round"))
             :draft-pick    (parse-long-field (get r "draft_pick"))
             :draft-overall (parse-long-field (get r "draft_ovr"))
             :birth-year    (when (re-find #"^\d{4}" (str birth))
                              (parse-long-field (subs birth 0 4)))}))))

(defn rows->snapshot-rows
  "Pure: DynastyProcess rows -> projected rows, deduped on `:sleeper` (first
  wins) so the snapshot can never contain two mappings for one key."
  [rows]
  (->> rows
       (keep snapshot-row)
       (reduce (fn [{:keys [seen out] :as acc} r]
                 (if (seen (:sleeper r))
                   acc
                   {:seen (conj seen (:sleeper r)) :out (conj out r)}))
               {:seen #{} :out []})
       :out))

(defn snapshot-crosswalk
  "Pure: snapshot rows -> {sleeper-id gsis-id}, skipping rows with no GSIS id
  (rookies not yet in the file, and every team defense, which the NFL never
  issues a player id for)."
  [rows]
  (into {} (keep (fn [{:keys [sleeper gsis]}] (when gsis [sleeper gsis]))) rows))

(defn read-snapshot
  "Read the pinned snapshot from the classpath; nil when absent."
  [resource]
  (when-let [r (io/resource resource)]
    (edn/read-string (slurp r))))

(def load-snapshot
  "Memoized: the pinned snapshot envelope, or an empty one when not committed
  yet. Callers must degrade to the Sleeper id rather than fail — an absent
  snapshot means unresolved ids, never a broken board."
  (memoize (fn [] (or (read-snapshot snapshot-resource)
                      {:schema-version snapshot-schema-version :rows []}))))

(defn coverage
  "Diagnostic: how many of `entries` resolve, and via which path. `entries` is a
  seq of [sleeper-id gsis-field]."
  [xwalk entries]
  (let [resolve-id (resolver xwalk)]
    (reduce (fn [acc [sid gsis]]
              (let [own (clean-gsis gsis)]
                (cond-> (update acc :n inc)
                  own                        (update :via-sleeper inc)
                  (and (not own)
                       (resolve-id sid gsis)) (update :via-crosswalk inc)
                  (resolve-id sid gsis)       (update :resolved inc))))
            {:n 0 :resolved 0 :via-sleeper 0 :via-crosswalk 0}
            entries)))
