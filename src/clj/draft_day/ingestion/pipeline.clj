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
            [draft-day.ingestion.sleeper-defense :as sleeper-defense]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.player-ids :as player-ids]
            [draft-day.ingestion.season :as season]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.validate :as validate]
            [draft-day.scoring :as scoring])
  (:import [java.time Instant]))

(def schema-version
  "Version of the persisted universe envelope and player-row shape."
  16)

(def default-cache-path (str "data/players_cache.v" schema-version ".transit"))
(def ^:private sample-resource "sample_players.edn")

(defn now-iso
  "Return the current instant as an ISO-8601 string."
  []
  (str (Instant/now)))

(defmacro best-effort
  "Evaluate optional ingestion work, logging exceptions and returning nil on failure."
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

(def failure-backoff-ms
  "How long a failed `load-ttl-cache` fetch keeps the next one from trying."
  (* 5 60 1000))

(defonce ^:private cache-failed-at (atom {}))

(defn reset-cache-failures!
  "Forget every failed fetch, so the next `load-ttl-cache` tries again (tests)."
  []
  (reset! cache-failed-at {}))

(defn- read-ttl-cache [path schema-version label]
  (try
    (let [env (read-transit path)]
      (when (= schema-version (:schema-version env)) env))
    (catch Exception e
      (log/warn e label "cache unreadable; refetching")
      nil)))

(defn load-ttl-cache
  "An envelope kept at `path` for `ttl-hours`: the cached copy while fresh, else
  whatever `fetch` returns, else the stale copy; nil with nothing to serve.

  `fetch` takes no arguments, returns the envelope to cache and throws when the
  answer is not one worth keeping, so it decides what an empty answer means. A
  failure serves the stale copy and is not retried for `failure-backoff-ms`,
  since every caller of one vendor shares its rate permits. A write that fails
  costs the cache one answer, never the board the one it just fetched. A copy
  under another `schema-version` is a miss.

  The gate for offline and replayed weeks stays with the caller, since
  `nflverse-weekly` requires this namespace."
  [{:keys [path ttl-hours schema-version fetch label]}]
  (let [cached (read-ttl-cache path schema-version label)
        backing-off? (when-let [t (get @cache-failed-at path)]
                       (< (- (System/currentTimeMillis) t) failure-backoff-ms))]
    (if (or (and cached (cache-fresh? path ttl-hours)) backing-off?)
      cached
      (try (let [env (assoc (fetch) :schema-version schema-version)]
             (best-effort (write-transit! path env))
             (swap! cache-failed-at dissoc path)
             env)
           (catch Exception e
             (log/warn e label "fetch failed:" (ex-message e))
             (swap! cache-failed-at assoc path (System/currentTimeMillis))
             cached)))))

(defn load-sample
  "The committed offline fallback universe (EDN on the classpath)."
  []
  (when-let [r (io/resource sample-resource)]
    (edn/read-string (slurp r))))

(defn checked
  "Validate fallback rows without throwing, returning filtered players and a report."
  [label rows]
  (let [{:keys [players report]} (validate/validate-universe (or rows []))]
    (validate/log-report! label report)
    {:players players :validation report}))

(defn cached->universe
  "Coerce a cached envelope or legacy player vector into an envelope."
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
  "Log enrichment coverage and warn when a non-partial join mostly misses."
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
  "Left-join one enrichment source and record whether it was unavailable or empty."
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

(def realized-source-labels
  "Source labels reported by `assoc-realized`."
  [:nflverse/weekly :sleeper/realized])

(def enrichment-source-labels
  "All source labels a fully enriched universe reports: `enrich-universe`'s,
  then `assoc-realized`'s."
  (-> [:sleeper/byes :fantasypros/sleepers :espn :nflverse/player-stats
       :sleeper/defense-history]
      (into (mapcat (fn [fmt] [(format-label :fantasypros/ecr fmt)
                               (format-label :fantasypros/aav fmt)]))
            scoring/formats)
      (into (map first) pos-tier-tasks)
      (into realized-source-labels)))

(defn scoped
  "Re-key a by-key enrichment map so its columns land under
  `[:vendor/by-format fmt]` rather than at the top level."
  [fmt by-key]
  (some-> by-key (update-vals (fn [cols] {:vendor/by-format {fmt cols}}))))

(defn enrichment-tasks
  "Build independently fetched enrichment tasks keyed by source label."
  [season]
  (into (into {:sleeper/byes         #(best-effort (sleeper/fetch-byes season))
               :fantasypros/sleepers #(best-effort (fantasypros/fetch-sleepers))
               :espn                 #(best-effort (espn/fetch season))
               :nflverse/player-stats #(best-effort (nflverse/fetch (dec season)))
               :sleeper/defense-history #(best-effort (sleeper-defense/fetch (dec season)))}
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
  "Enrich a validated universe, retaining format-specific vendor columns side by
  side. The in-season realized columns are not here: see `load-realized`."
  [season universe]
  (let [fetched  (parallel/all (enrichment-tasks season))
        byes     (:sleeper/byes fetched)
        sleepers (:fantasypros/sleepers fetched)
        espn     (:espn fetched)
        prior    (:nflverse/player-stats fetched)
        defense  (:sleeper/defense-history fetched)]
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
      ;; Team defenses, which nflverse has no rows for: keyed by the Sleeper
      ;; player id, which for a defense is its team abbreviation.
      (apply-enrichment acc :sleeper/defense-history (:by-key defense)
                        {:key-fn            #(or (get-in % [:ids :sleeper])
                                                 (:player-id %))
                         :key-position      (:positions defense)
                         :expected-partial? true}))))

(def realized-schema-version
  "Version of the realized cache envelope."
  3)

(def default-realized-cache-path
  (str "data/realized.v" realized-schema-version ".transit"))

(defn- realized-ttl-hours []
  (Double/parseDouble (or (System/getenv "DRAFTDAY_REALIZED_TTL_HOURS") "1")))

(defn fetch-realized
  "Network: this season's realized columns and how far the season has got. A
  source that failed is nil in the envelope rather than failing the whole."
  [season]
  (let [weekly (best-effort (nflverse-weekly/fetch season))]
    {:schema-version realized-schema-version
     :season         season
     :as-of          (nflverse-weekly/as-of-week)
     :fetched-at     (now-iso)
     :weekly         weekly
     :sleeper        (best-effort (sleeper-actual/fetch season (:through-week weekly)))}))

(defn realized-answers?
  "Whether a realized envelope was fetched for this season and week ceiling."
  [env season]
  (and (= realized-schema-version (:schema-version env))
       (= season (:season env))
       (= (nflverse-weekly/as-of-week) (:as-of env))))

(defn lost-a-source?
  "Whether `fetched` is missing a source `cached` had, so a transient outage
  does not replace a good copy with an empty one."
  [fetched cached]
  (boolean (some #(and (get cached %)
                       (nil? (get fetched %))) [:weekly :sleeper])))

(defn- read-realized [path]
  (try (read-transit path)
       (catch Exception e
         (log/warn e "realized cache unreadable; refetching")
         nil)))

(defonce ^:private realized-memo (atom nil))

(defn load-realized
  "This season's realized envelope off a short-lived cache, or nil offline.
  Apart from the universe because a game-day cadence cannot ride a 24-hour
  cache; nflverse's file and Sleeper's weeks stay one fetch, since nflverse's
  `:through-week` bounds which Sleeper weeks are asked for."
  ([season] (load-realized season {}))
  ([season {:keys [refresh path] :or {path default-realized-cache-path}}]
   (when-not (offline?)
     (let [fresh? (and (not refresh) (cache-fresh? path (realized-ttl-hours)))
           memo   (let [{:keys [memo-path env]} @realized-memo]
                    (when (and fresh? (= memo-path path) (realized-answers? env season))
                      env))
           env    (or memo
                      (let [cached  (read-realized path)
                            cached? (realized-answers? cached season)]
                        (if (and fresh? cached?)
                          cached
                          (let [fetched (fetch-realized season)]
                            (if (and cached? (lost-a-source? fetched cached))
                              ;; Kept copies restart their clock, or every request retries.
                              (do (.setLastModified (io/file path) (System/currentTimeMillis))
                                  cached)
                              (do (write-transit! path fetched) fetched))))))]
       (reset! realized-memo {:memo-path path :env env})
       env))))

(defn reset-realized!
  "Drop the realized cache, in memory and on disk."
  []
  (reset! realized-memo nil)
  (delete-cache! default-realized-cache-path))

(defn assoc-realized
  "Join a realized envelope onto a universe envelope and set `:through-week`."
  [env {:keys [weekly sleeper]}]
  (-> env
      (apply-enrichment :nflverse/weekly (:by-key weekly)
                        {:key-fn            #(get-in % [:ids :gsis])
                         :key-position      (:positions weekly)
                         :expected-partial? true})
      (apply-enrichment :sleeper/realized (:by-key sleeper)
                        {:key-fn            #(or (get-in % [:ids :sleeper])
                                                 (:player-id %))
                         :expected-partial? true})
      (assoc :through-week (or (:through-week weekly) 0))))

(defonce ^:private joined-memo (atom nil))

(defn with-realized
  "The universe with the current realized columns joined on. Only a live or
  cached universe is joined; a sample carries its own, captured with it. The
  join is memoized on both inputs, since every board request asks for it."
  [{:keys [source season] :as env}]
  (if-not (#{"live" "cache"} source)
    env
    (let [realized (load-realized (season/resolve-season season))
          {:keys [base joined] r :realized} @joined-memo]
      (if (and (identical? base env) (identical? r realized))
        joined
        (let [joined (assoc-realized env realized)]
          (reset! joined-memo {:base env :realized realized :joined joined})
          joined)))))

(defn fetch-base-universe
  "Fetch, anchor, validate, and enrich the live universe, without the realized
  columns."
  [season]
  (let [anchored (player-ids/attach-ids (sleeper/fetch-universe season)
                                        (player-ids/pinned-index))
        {:keys [players report]} (validate/validate-universe anchored)]
    (validate/log-report! "sleeper universe" report)
    (when (validate/systemic-failure? report)
      (throw (ex-info "player universe failed validation" report)))
    (assoc (enrich-universe season players) :validation report)))

(defn fetch-enriched-universe
  "Fetch the live universe with every column, realized ones included."
  [season]
  (assoc-realized (fetch-base-universe season) (fetch-realized season)))

(defn sample-universe
  "Return the bundled fallback as an envelope with its captured provenance."
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
  "Read a schema-matched disk cache, or return nil."
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
          {:keys [players validation sources]} (fetch-base-universe season')
          env {:schema-version schema-version
               :season         season'
               :fetched-at     (now-iso)
               :validation     validation
               :sources        sources
               :players        players}]
      (write-transit! cache-path env)
      (assoc env :source "live"))
    (catch Exception _
      (or (cached-universe cache-path)
          (let [s (sample-universe)] (when (seq (:players s)) s))
          {:schema-version schema-version :players [] :source "empty"}))))

(def retry-ms
  "How long a universe that fell back past a fresh cache is held before live
  ingestion is tried again."
  (* 10 60 1000))

(defn expires-at
  "When a universe just loaded against `cache-path` should be reloaded: when the
  file goes stale, or after `retry-ms` when it already is. Measured off the
  file rather than the load, so a file read an hour before its expiry is not
  then held for a whole TTL more."
  [cache-path]
  (let [f   (io/file cache-path)
        ttl (long (* (cache-ttl-hours) 3600 1000))]
    (if (cache-fresh? cache-path (cache-ttl-hours))
      (+ (.lastModified f) ttl)
      (+ (System/currentTimeMillis) retry-ms))))

(defn load-universe
  "Load the universe envelope, optionally bypassing the fresh cache, stamped
  with its `:expires-at`. Without the realized columns: see `with-realized`."
  ([] (load-universe {}))
  ([{:keys [refresh season cache-path] :or {cache-path default-cache-path}}]
   (-> (cond
         (offline?)
         (sample-universe)

         (and (not refresh) (cache-fresh? cache-path (cache-ttl-hours)))
         (or (cached-universe cache-path) (live-universe season cache-path))

         :else
         (live-universe season cache-path))
       (assoc :expires-at (expires-at cache-path)))))

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
  "Whether a cached envelope matches the schema, season, and requested week."
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
  "Fetch and cache one week's projection lines and kickoff times."
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
  "Join kickoff metadata onto players by team, leaving unknown teams unchanged."
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

(defn assoc-trending
  "Join Sleeper's trending adds onto players as `:trending/adds`, by Sleeper id
  as `assoc-weekly` joins, leaving a player off the list unannotated."
  [players adds]
  (if (empty? adds)
    players
    (mapv (fn [p]
            (if-let [n (get adds (or (get-in p [:ids :sleeper]) (:player-id p)))]
              (assoc p :trending/adds n)
              p))
          players)))

(defn assoc-weekly
  "Join weekly lines onto players, leaving unprojected players unannotated."
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
