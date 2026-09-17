(ns draft-day.ingestion.matchups
  "Provider-agnostic head-to-head: who plays whom this week, what each roster
  started, and what every player on it has actually scored.

  A third pair of multimethods rather than more keys on `league-sync`, one
  cadence further along: rules change yearly, state changes on a claim, a
  scoreboard changes while you are looking at it. Providers register by
  defmethoding onto these; this namespace never requires one.

  Two things a caller cannot derive. Ids stay in the provider's space, both the
  roster ids and the player ids inside `:player-points` — the crosswalk needs
  the universe, so `db/provider->player-id` bridges them at the point where both
  halves are in hand, the way `rankings.waiver` already does for a roster. And the week is asked of the provider, never computed from
  `:through-week`, which advances as games finish and so names week N+1 while
  week N is being played.

  The normalized shape, carrying no provider's vocabulary:

      {:week 3
       :matchups [{:matchup-id 1 :roster-ids [1 5]} ...]
       :scores   {roster-id {:official      96.1
                             :starter-ids   [\"4034\" ...]
                             :player-points {\"4034\" 18.4}}}}

  `:roster-ids` holds one id for a roster with no opponent rather than being
  absent: a team with nobody to play still has a lineup and still scores."
  (:require [draft-day.ingestion.league-sync :as league-sync]))

(defmulti fetch-raw-matchups
  "Network: raw provider-specific matchup payload for one league and week.
  Throws ex-info with :status on failure, as the other two pairs do."
  (fn [provider _league-id _week] provider))

(defmethod fetch-raw-matchups :default
  [provider _league-id _week]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-matchups
  "Pure: a provider's raw matchup payload -> `{:matchups [...] :scores {...}}`.

  `:player-points` covers the whole roster, not only the starters, or the
  optimal-lineup half of the board cannot say what a bench player scored. A
  provider publishes the zeros it is given; whether one means \"has not played\"
  is the matchup board's judgment against the kickoff clock, not a provider's —
  see `espn-schedule/not-started?`. That board is unmerged; see `docs/TODO.md`."
  (fn [provider _raw] provider))

(defmulti current-week
  "Network: which week this provider is currently showing.

  Throws ex-info with :status on failure. nil means a provider between seasons
  has no week to name — 'no matchup to show', never week zero."
  (fn [provider] provider))

(defmethod current-week :default
  [provider]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defn fetch-matchups
  "{:provider :league-id :week} -> {:ok true :week n :matchups [...] :scores {...}}
  or {:ok false :status :error}.

  The same envelope `sync-league` and `import-league` return. Sequential because
  the matchup document is addressed *by* week, so there is nothing to overlap;
  an explicit `:week` skips the first call. The unwrap still matters though
  nothing here derefs a future: a provider is free to fetch concurrently, and an
  `ExecutionException` would cost every 404 its status."
  [{:keys [provider league-id week]}]
  (let [provider (keyword provider)]
    (try
      (let [wk (or week (current-week provider))]
        (if-not wk
          {:ok false :status 404 :error "No current week for this provider"}
          ;; `:ok` and `:week` go on last: they are this dispatcher's
          ;; guarantee, not a provider's to set.
          (merge (normalize-matchups provider
                                     (fetch-raw-matchups provider league-id wk))
                 {:ok true :week wk})))
      (catch Exception e
        (let [cause (league-sync/unwrap-execution e)]
          {:ok false
           :status (or (:status (ex-data cause)) 502)
           :error  (ex-message cause)})))))
