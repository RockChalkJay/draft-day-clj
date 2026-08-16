(ns draft-day.benchmark.sources.sleeper
  "Sleeper: vintage preseason projections + ADP for a past season.

  Reuses the production ingestion path (`ingestion.sleeper/normalize-entry`) on
  purpose — the harness should score the universe the app actually builds, not a
  parallel reimplementation that can drift away from it.

  Alongside the normalized players it keeps the vintage evidence the leakage gate
  needs, which `normalize-entry` drops: each entry's projected `gp` and its
  `last_modified` stamp. See `benchmark.vintage` for how those are judged — the
  short version is that `last_modified` is worthless as a gate here (Sleeper
  bulk-restamps every archived season on the Monday after week 18) and the real
  test is behavioural."
  (:require [draft-day.benchmark.fetch :as fetch]
            [draft-day.ingestion.sleeper :as sleeper]))

(defn entry-vintage
  "Pure: raw projection entries -> the evidence the vintage gate reasons about.

  :gp-freq   projected games-played frequencies. A preseason snapshot projects a
             full season for everyone (flat 17/18); a snapshot contaminated with
             in-season data reflects games actually played and spreads out.
  :modified  summary of the `last_modified` stamps. The span matters more than
             the date: 2024's ~684 records were all written inside a ~10 second
             window in January 2025, which is a batch archival re-stamp rather
             than per-player in-season edits. A genuinely maintained-in-season
             projection would show stamps spread over months."
  [entries]
  (let [scored (filter #(get-in % [:stats :pts_ppr]) entries)
        stamps (sort (keep :last_modified scored))]
    {:n         (count scored)
     :gp-freq   (frequencies (map #(get-in % [:stats :gp]) scored))
     :companies (into (sorted-set) (keep :company scored))
     :modified  (when (seq stamps)
                  {:min      (first stamps)
                   :max      (last stamps)
                   :span-ms  (- (last stamps) (first stamps))
                   :distinct (count (distinct stamps))})}))

(defn season-data
  "{:players [...] :vintage {...}} for a season, disk-cached.

  :players are normalized universe rows (:player-id :player-name :position :stats
  :vendor/by-format ...) exactly as the live pipeline produces them — which is
  why ADP is read through `adp-of` rather than off a flat key that ingestion is
  free to move.

  Deliberately does NOT swallow fetch errors. Sleeper answers 200 with an empty
  stats map on every entry for seasons it has no projections for (1999-2017:
  3112 entries, zero with stats), so a genuine absence is already distinguishable
  from a failure — but only if the failure is allowed to surface. Catching here
  would report a Sleeper outage as 'no projections published', which is the one
  thing a harness must never do: quietly turn a broken fetch into a finding.
  Callers that need to keep going catch it and say so."
  [season]
  (fetch/cached
   ;; v2 adds :raw-entries/:with-stats; v3 is the move of ADP off the flat
   ;; :sleeper/adp into :vendor/by-format. See fetch/cache-path on schema tokens.
   (fetch/cache-path "sleeper" "proj" "v3" season)
   (fn []
     (let [entries (sleeper/fetch-projections season)]
       {:players     (vec (sleeper/universe-from-entries entries))
        :vintage     (entry-vintage entries)
        :raw-entries (count entries)
        :with-stats  (count (filter (fn [e] (seq (:stats e))) entries))}))))

(defn players [season] (:players (season-data season)))
(defn vintage [season] (:vintage (season-data season)))

(def adp-formats
  "Which published formats' ADP the harness scores against, in preference order.

  Ingestion carries all three under `:vendor/by-format`. Before it did, the flat
  `:sleeper/adp` was PPR-*preferred with fallback* — Sleeper leaves a 999
  sentinel on formats it hasn't priced, and a player priced only in standard
  still belongs in the pool. Reproducing that order is what keeps these numbers
  comparable with every result already recorded; changing it moves every
  benchmark, which is a decision to make and report deliberately, not a default
  to drift into."
  [:ppr :half-ppr :standard])

(defn adp-of
  "One universe row's harness ADP, or nil. The single reader of the key, so a
  move like the one to `:vendor/by-format` breaks in one place rather than
  silently resolving to nil across the harness."
  [p]
  (some #(get-in p [:vendor/by-format % :sleeper/adp]) adp-formats))

(defn adp
  "{sleeper-player-id adp} for a season, from the same projection payload."
  [season]
  (into {} (keep (fn [p] (when-let [a (adp-of p)] [(:player-id p) a])))
        (players season)))
