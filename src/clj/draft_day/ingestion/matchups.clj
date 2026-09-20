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
  halves are in hand, the way `rankings.waiver` already does for a roster. And
  the week is asked of the provider, never computed from `:through-week`, which
  advances as games finish and so names week N+1 while week N is being played.

  The normalized shape, carrying no provider's vocabulary:

      {:week 3
       :slots    [\"QB\" \"RB\" ...]                    ; optional
       :matchups [{:matchup-id 1 :roster-ids [1 5]} ...]
       :scores   {roster-id {:official      96.1
                             :starter-ids   [\"4034\" ...]
                             :player-ids    [\"4034\" ...]
                             :player-points {\"4034\" 18.4}}}}

  `:roster-ids` holds one id for a roster with no opponent rather than being
  absent: a team with nobody to play still has a lineup and still scores.

  `:slots` is for a provider that puts its starters in an order of its own
  making rather than the league's: a lineup is read by position, so the list it
  was aligned to has to travel with it. A provider whose lineup is positional
  against the synced league omits it, and `routes/matchup-slots` reads the sync."
  (:require [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.season :as season]
            [draft-day.providers :as providers]))

(defmulti fetch-raw-matchups
  "Network: raw provider-specific matchup payload, from a request map of
  `{:league-id :season :credentials :week}` — the other two pairs' map plus the
  week the scoreboard is addressed by. Throws ex-info with :status on failure,
  as they do."
  (fn [provider _req] provider))

(defmethod fetch-raw-matchups :default
  [provider _req]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-matchups
  "Pure: a provider's raw matchup payload -> `{:matchups [...] :scores {...}}`,
  and `:slots` where its lineups are not positional against the synced league.

  `:player-points` covers the whole roster, not only the starters, or the
  optimal-lineup half of the board cannot say what a bench player scored. A
  provider publishes the zeros it is given; whether one means \"has not played\"
  is `rankings.matchup`'s judgment against the kickoff clock, not a provider's."
  (fn [provider _raw] provider))

(defmulti current-week
  "Network: which week this provider is currently showing, from the same
  `{:league-id :season :credentials}` map — Sleeper needs none of it, but a host
  that puts the current week on the league document (ESPN) needs all three.

  Throws ex-info with :status on failure. nil means a provider between seasons
  has no week to name — 'no matchup to show', never week zero."
  (fn [provider _req] provider))

(defmethod current-week :default
  [provider _req]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defn fetch-matchups
  "{:provider :league-id :season :credentials :week} -> {:ok true :week n
  :matchups [...] :scores {...}} or {:ok false :status :error}.

  The same envelope, and the same dispatcher's contract, as `sync-league` and
  `import-league`: the season is defaulted and access validated here, so no
  provider has to remember to. Sequential because
  the matchup document is addressed *by* week, so there is nothing to overlap;
  an explicit `:week` skips the first call. The unwrap still matters though
  nothing here derefs a future: a provider is free to fetch concurrently, and an
  `ExecutionException` would cost every 404 its status."
  [{:keys [provider league-id season credentials week]}]
  (let [provider (keyword provider)
        req      {:league-id   league-id
                  :season      (season/resolve-season season)
                  :credentials credentials}]
    (if-let [bad (providers/league-access-error provider league-id credentials)]
      {:ok false :status 400 :error (:error bad)}
      (try
        (let [wk (or week (current-week provider req))]
          (if-not wk
            {:ok false :status 404 :error "No current week for this provider"}
            ;; `:ok` and `:week` go on last: they are this dispatcher's
            ;; guarantee, not a provider's to set.
            (merge (normalize-matchups provider
                                       (fetch-raw-matchups provider (assoc req :week wk)))
                   {:ok true :week wk})))
        (catch Exception e
          (let [cause (league-sync/unwrap-execution e)]
            {:ok false
             :status (or (:status (ex-data cause)) 502)
             :error  (ex-message cause)}))))))
