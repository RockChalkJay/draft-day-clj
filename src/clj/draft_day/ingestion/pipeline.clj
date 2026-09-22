(ns draft-day.ingestion.pipeline
  "Resolve the player universe through cache, live ingestion, and fallbacks.

  Each branch returns an envelope containing the players, source, season,
  fetch time, schema version, and validation report, so cached and fallback
  boards retain their provenance."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [cognitect.transit :as transit]
            [draft-day.ingestion.espn :as espn]
            [draft-day.ingestion.espn-schedule :as espn-schedule]
            [draft-day.ingestion.fantasypros :as fantasypros]
            [draft-day.ingestion.match :as match]
            [draft-day.ingestion.merge :as merge]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.nflverse-weekly :as nflverse-weekly]
            [draft-day.ingestion.sleeper-actual :as sleeper-actual]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.player-ids :as player-ids]
            [draft-day.ingestion.season :as season]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.validate :as validate]
            [draft-day.scoring :as scoring])
  (:import [java.time Instant]))

(def schema-version
  "Version of the persisted universe envelope and player-row shape.

  Bump it whenever cached data would otherwise deserialize successfully with
  missing or changed fields."
  13)

(def default-cache-path (str "data/players_cache.v" schema-version ".transit"))
(def ^:private sample-resource "sample_players.edn")

(defn now-iso
  "Current instant as ISO-8601. A var so tests can pin it."
  []
  (str (Instant/now)))

(defmacro best-effort
  "Evaluate body, returning its value; on any exception log it to *err* and
  return nil. For optional enrichment steps where a failed fetch should degrade
  gracefully (leave the column absent) rather than abort ingestion."
  [& body]
  `(try ~@body
     (catch Exception e#
       (log/warn e# "best-effort failed:" (ex-message e#) (ex-data e#))
       nil)))

(defn- cache-ttl-hours []
  (Double/parseDouble (or (System/getenv "DRAFTDAY_CACHE_TTL_HOURS") "24")))

(defn offline? []
  (= "1" (System/getenv "DRAFTDAY_OFFLINE")))

(defn write-transit! [path data]
  (io/make-parents path)
  (with-open [out (io/output-stream path)]
    (transit/write (transit/writer out :json) data)))

(defn read-transit [path]
  (when (.exists (io/file path))
    (with-open [in (io/input-stream path)]
      (transit/read (transit/reader in :json)))))

(defn delete-cache!
  "Remove the on-disk cache file, if present. A no-op when already absent."
  [path]
  (let [f (io/file path)]
    (when (.exists f) (.delete f))))

(defn cache-fresh? [path ttl-hours]
  (let [f (io/file path)]
    (and (.exists f)
         (< (- (System/currentTimeMillis) (.lastModified f))
            (long (* ttl-hours 3600 1000))))))

(defn load-sample
  "The committed offline fallback universe (EDN on the classpath)."
  []
  (when-let [r (io/resource sample-resource)]
    (edn/read-string (slurp r))))

(defn checked
  "Validate a universe from a branch with nowhere better to fall back to (cache,
  bundled sample): drop unusable rows and log them, but never throw. Returns
  `{:players :validation}`."
  [label rows]
  (let [{:keys [players report]} (validate/validate-universe (or rows []))]
    (validate/log-report! label report)
    {:players players :validation report}))

(defn cached->universe
  "Coerce whatever is on disk into an envelope. Pre-versioning caches were a bare
  player vector; call those schema 0 so a hand-copied file degrades to a refetch
  instead of throwing on a map lookup."
  [x]
  (cond
    (map? x)        x
    (sequential? x) {:schema-version 0 :players (vec x)}
    :else           nil))

(def hit-rate-floor
  "Below this share of a position's *published* rows landing on a universe
  player, the join is broken rather than the source being thin."
  0.80)

(defn log-enrichment!
  "Log each enrichment's coverage and warn when an expected join mostly misses.
  Sources marked `:expected-partial?` are exempt because their row sets are
  intentionally smaller than the current universe."
  [label {:keys [ok? rows matched hit-rate coverage by-position expected-partial?]}]
  (if-not ok?
    (log/warn (format "%s: unavailable, columns omitted" label))
    (do
      (log/info (format "%s: %d rows -> %d matched (%.0f%% of rows, %.0f%% of board)"
                        label rows matched (* 100.0 hit-rate) (* 100.0 coverage)))
      (doseq [[pos {:keys [n rows matched]}] (sort by-position)
              :when (and (not expected-partial?)
                         (pos? rows)
                         (< (/ (double matched) rows) hit-rate-floor))]
        (log/warn
         (format "%s: %s published %d row(s), only %d landed (board has %d)"
                 label pos rows matched n))))))

(defn apply-enrichment
  "Left-join one enrichment source and record its report under `label`.
  A nil source is recorded as unavailable; an empty successful source remains a
  join report so an unexpected miss is distinguishable from an outage."
  ([acc label by-key] (apply-enrichment acc label by-key {}))
  ([acc label by-key opts]
   (if (nil? by-key)
     (do (log-enrichment! label {:ok? false})
         (assoc-in acc [:sources label] {:ok? false}))
     (let [{:keys [players report]} (merge/left-join-report (:players acc) by-key opts)
           report (assoc report :ok? true
                         :expected-partial? (boolean (:expected-partial? opts)))]
       (log-enrichment! label report)
       (-> acc
           (assoc :players players)
           (assoc-in [:sources label] report))))))

(def ^{:doc "Shared vendor-column label builder used by ingestion and the browser."}
  format-label scoring/format-label)

(defn pos-tier-label
  "Build the `:sources` label for a position's expert-tier scrape."
  ([pos] (keyword "fantasypros" (str "pos-tier-" (str/lower-case pos))))
  ([pos fmt] (format-label (pos-tier-label pos) fmt)))

(def pos-tier-tasks
  "Tasks for each position-tier scrape, scoped only where formats differ."
  (vec (mapcat (fn [[pos varies?]]
                 (if varies?
                   (map (fn [fmt] [(pos-tier-label pos fmt) pos fmt]) scoring/formats)
                   [[(pos-tier-label pos) pos (first scoring/formats)]]))
               (sort fantasypros/pos-formats))))

(def enrichment-source-labels
  "All source labels reported by `enrich-universe`."
  (into (into [:sleeper/byes :fantasypros/sleepers :espn
               :nflverse/player-stats :nflverse/weekly :sleeper/realized]
              (mapcat (fn [fmt] [(format-label :fantasypros/ecr fmt)
                                 (format-label :fantasypros/aav fmt)]))
              scoring/formats)
        (map first)
        pos-tier-tasks))

(defn scoped
  "Re-key a by-key enrichment map so its columns land under
  `[:vendor/by-format fmt]` rather than at the top level."
  [fmt by-key]
  (some-> by-key (update-vals (fn [cols] {:vendor/by-format {fmt cols}}))))

(defn enrichment-tasks
  "Build best-effort enrichment tasks keyed by their source labels.

  Fetches are independent and run concurrently; joins remain ordered so the
  source report is deterministic."
  [season]
  (into (into {:sleeper/byes         #(best-effort (sleeper/fetch-byes season))
               :fantasypros/sleepers #(best-effort (fantasypros/fetch-sleepers))
               :espn                 #(best-effort (espn/fetch season))
               :nflverse/player-stats #(best-effort (nflverse/fetch (dec season)))
               :nflverse/weekly       #(best-effort (nflverse-weekly/fetch season))}
              (mapcat (fn [fmt]
                        [[(format-label :fantasypros/ecr fmt)
                          #(best-effort (fantasypros/fetch-ecr fmt))]
                         [(format-label :fantasypros/aav fmt)
                          #(best-effort (fantasypros/fetch-aav fmt))]]))
              scoring/formats)
        (map (fn [[label pos fmt]]
               [label #(best-effort (fantasypros/fetch-pos-ecr pos fmt))]))
        pos-tier-tasks))

(defn enrich-universe
  "Enrich an already-validated universe and return `{:players :sources}`.

  Format-specific FantasyPros values remain side by side because the shared
  universe serves leagues with different scoring formats; ESPN stays unscoped."
  [season universe]
  (let [fetched  (parallel/all (enrichment-tasks season))
        byes     (:sleeper/byes fetched)
        sleepers (:fantasypros/sleepers fetched)
        espn     (:espn fetched)
        prior    (:nflverse/player-stats fetched)
        weekly   (:nflverse/weekly fetched)
        ;; Not a peer task: it needs `through-week`, which is
        ;; `:nflverse/weekly`'s own answer, and two sources for the week is how
        ;; a November league comes to read an August board.
        realized (best-effort (sleeper-actual/fetch season (:through-week weekly)))]
    (log/info (format ":sleeper/byes: %d team bye weeks" (count byes)))
    (as-> {:players (cond-> universe (seq byes) (sleeper/assoc-byes byes))
           :sources {:sleeper/byes (if (seq byes)
                                     {:ok? true :rows (count byes)}
                                     {:ok? false})}} acc
      (reduce (fn [acc fmt]
                (let [ecr (get fetched (format-label :fantasypros/ecr fmt))
                      aav (get fetched (format-label :fantasypros/aav fmt))]
                  (-> acc
                      (apply-enrichment (format-label :fantasypros/ecr fmt)
                                        (scoped fmt (some-> ecr match/by-key)))
                      (apply-enrichment (format-label :fantasypros/aav fmt)
                                        (scoped fmt (some-> aav match/by-key))))))
              acc scoring/formats)
      (reduce (fn [acc [label pos fmt]]
                (let [by-key (some-> (get fetched label) match/by-key)]
                  (apply-enrichment acc label
                                    (if (get fantasypros/pos-formats pos)
                                      (scoped fmt by-key)
                                      by-key))))
              acc pos-tier-tasks)
      (apply-enrichment acc :fantasypros/sleepers (some-> sleepers match/by-key))
      (apply-enrichment acc :espn espn)
      (apply-enrichment acc :nflverse/player-stats (:by-key prior)
                        {:key-fn            #(get-in % [:ids :gsis])
                         :key-position      (:positions prior)
                         :expected-partial? true})
      (apply-enrichment acc :nflverse/weekly (:by-key weekly)
                        {:key-fn            #(get-in % [:ids :gsis])
                         :key-position      (:positions weekly)
                         :expected-partial? true})
      ;; Joined on the player's *Sleeper* id, not `:player-id`, which is the GSIS
      ;; id wherever one resolves. Same trap and fallback as `assoc-weekly`,
      ;; whose comment has the numbers; the fallback carries team defenses.
      (apply-enrichment acc :sleeper/realized (:by-key realized)
                        {:key-fn            #(or (get-in % [:ids :sleeper])
                                                 (:player-id %))
                         :expected-partial? true})
      (assoc acc :through-week (or (:through-week weekly) 0)))))

(defn fetch-enriched-universe
  "Fetch, anchor, validate, and enrich the live universe.
  Validation follows id anchoring so collisions are rejected before enrichment;
  systemic validation failures throw for the cache fallback to handle."
  [season]
  (let [anchored (player-ids/attach-ids (sleeper/fetch-universe season)
                                        (player-ids/pinned-index))
        {:keys [players report]} (validate/validate-universe anchored)]
    (validate/log-report! "sleeper universe" report)
    (when (validate/systemic-failure? report)
      (throw (ex-info "player universe failed validation" report)))
    (assoc (enrich-universe season players) :validation report)))

(defn sample-universe
  "Return the bundled fallback as an envelope, preserving captured provenance."
  []
  (let [env (or (cached->universe (load-sample)) {:players []})]
    (merge {:schema-version (:schema-version env)
            :season         (:season env)
            :fetched-at     (:captured-at env)
            :through-week   (or (:through-week env) 0)
            :sources        (:sources env)
            :source         "sample"}
           (checked "sample" (player-ids/attach-ids
                              (:players env) (player-ids/pinned-index))))))

(defn cached-universe
  "Read and validate a schema-matched disk cache, or return nil."
  [path]
  (when-let [env (cached->universe (read-transit path))]
    (when (= schema-version (:schema-version env))
      (let [{:keys [players] :as v} (checked "cache" (:players env))]
        (when (seq players)
          (merge env v {:source "cache"}))))))

(defn live-universe
  "Fetch and cache the live universe, falling back to stale cache, sample, or empty."
  [season cache-path]
  (try
    (let [season' (season/resolve-season season)
          {:keys [players validation sources through-week]} (fetch-enriched-universe season')
          env {:schema-version schema-version
               :season         season'
               :fetched-at     (now-iso)
               :through-week   (or through-week 0)
               :validation     validation
               :sources        sources
               :players        players}]
      (write-transit! cache-path env)
      (assoc env :source "live"))
    (catch Exception _
      (or (cached-universe cache-path)
          (let [s (sample-universe)] (when (seq (:players s)) s))
          {:schema-version schema-version :players [] :source "empty"}))))

(defn load-universe
  "Load the universe envelope, optionally bypassing the fresh cache."
  ([] (load-universe {}))
  ([{:keys [refresh season cache-path] :or {cache-path default-cache-path}}]
   (cond
     (offline?)
     (sample-universe)

     (and (not refresh) (cache-fresh? cache-path (cache-ttl-hours)))
     (or (cached-universe cache-path) (live-universe season cache-path))

     :else
     (live-universe season cache-path))))

(def weekly-schema-version
  "Version of the weekly projection cache envelope and line shape."
  4)

(def default-weekly-cache-path
  (str "data/weekly_projections.v" weekly-schema-version ".transit"))

(def matchup-weekly-cache-path
  "Separate cache path for matchup projections, which use a different week."
  (str "data/weekly_projections_matchup.v" weekly-schema-version ".transit"))

(defn- weekly-ttl-hours []
  (Double/parseDouble (or (System/getenv "DRAFTDAY_WEEKLY_TTL_HOURS") "1")))

(defn weekly-answers?
  "Whether a cached envelope matches the schema, season, and requested week.
  An empty `:lines` map is still a valid answer."
  [env season week]
  (boolean (and (map? env)
                (= weekly-schema-version (:schema-version env))
                (= season (:season env))
                (= week (:week env)))))

(defn- read-weekly [path]
  (try (read-transit path)
       (catch Exception e
         (log/warn e "weekly cache unreadable; refetching")
         nil)))

(defn live-weekly
  "Fetch and cache one week's projection lines with its kickoff times."
  [season week path]
  (let [env {:schema-version weekly-schema-version
             :season         season
             :week           week
             :fetched-at     (now-iso)
             :lines          (sleeper/fetch-weekly season week)
             :kickoffs       (or (espn-schedule/fetch season week) {})}]
    (write-transit! path env)
    env))

(defonce ^:private weekly-memo
  ;; Memoized by path; file freshness and season/week matching remain authoritative.
  (atom nil))

(defn load-weekly
  "Load one week's cached projection envelope, or nil when unavailable."
  ([season week] (load-weekly season week {}))
  ([season week {:keys [refresh path] :or {path default-weekly-cache-path}}]
   (when-not (offline?)
     (let [fresh? (and (not refresh) (cache-fresh? path (weekly-ttl-hours)))
           memo   (let [{:keys [memo-path env]} @weekly-memo]
                    (when (and fresh? (= memo-path path)
                               (weekly-answers? env season week))
                      env))
           env    (or memo
                      (let [cached (read-weekly path)]
                        (if (and fresh? (weekly-answers? cached season week))
                          cached
                          (try
                            (live-weekly season week path)
                            (catch Exception e
                              (log/warn e "weekly projections fetch failed:" (ex-message e))
                              (when (weekly-answers? cached season week) cached))))))]
       (reset! weekly-memo {:memo-path path :env env})
       (when (seq (:lines env)) env)))))

(defn assoc-kickoffs
  "Join kickoff metadata onto players by team; unknown teams remain unannotated."
  [players kickoffs]
  (if (empty? kickoffs)
    players
    (mapv (fn [p]
            (if-let [{:keys [kickoff status detail venue opponent home? neutral?]}
                     (get kickoffs (:team p))]
              (assoc p
                     :kickoff/at       kickoff
                     :kickoff/status   status
                     :kickoff/started? (not (espn-schedule/not-started? status))
                     :kickoff/detail   detail
                     :kickoff/venue    venue
                     :kickoff/opponent opponent
                     :kickoff/home?    home?
                     :kickoff/neutral? neutral?)
              p))
          players)))

(defn assoc-weekly
  "Join weekly lines onto players, preserving absence for unprojected players."
  [players lines]
  (mapv (fn [p]
          (let [k (or (get-in p [:ids :sleeper]) (:player-id p))]
            (if-let [{:keys [stats opponent home? updated-at]} (get lines k)]
              (assoc p
                     :week/stats stats
                     :week/opponent opponent
                     :week/home? home?
                     :week/updated-at updated-at)
              p)))
        players))
