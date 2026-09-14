(ns draft-day.ingestion.matchups
  "Provider-agnostic head-to-head: who plays whom this week, what each roster
  started, and what every player on it has actually scored.

  A THIRD PAIR OF MULTIMETHODS, not more keys on `league-sync`. The split is the
  same one `league-sync` draws against `league-import`, one cadence further
  along. An import is a league's *rules*, which change once a year; a sync is
  its *state*, which changes when somebody makes a claim; a matchup is a *live
  scoreboard*, which changes while you are looking at it. Folding this into the
  sync would serve Sunday-afternoon scores out of a cache written on Tuesday,
  behind a button labelled Re-sync rosters.

  Registration is the convention `league-import` and `league-sync` already use:
  a provider namespace defmethods onto these and this namespace never requires
  one, so adding ESPN is a new file plus a `:require` in `routes`.

  IDS STAY IN THE PROVIDER'S SPACE, both the roster ids and the player ids
  inside `:player-points`. Same rule and same reason as
  `league-sync.sleeper` — the crosswalk to the canonical GSIS ids the board is
  keyed by lives in `db/provider->player-id` and needs the universe, which
  ingestion of a *league* has no business loading. `rankings.matchup` bridges
  them where both halves are in hand.

  THE WEEK IS ASKED OF THE PROVIDER, never derived. Everywhere else in the app
  the season's progress is read off the data as `:through-week`, and the next
  week is `(inc through-week)`. That is right for a price and wrong for a
  scoreboard: `:through-week` advances as games finish, so partial Sunday rows
  push it to N and `(inc ...)` asks for week N+1 *while week N is being played*
  — which is the only hour anyone opens this view. `docs/TODO.md` already
  carries that bug against the waiver board; here it would not be a stale price
  but the wrong game entirely.

  The normalized shape, carrying no provider's vocabulary:

      {:week 3
       :matchups [{:matchup-id 1 :roster-ids [1 5]} ...]
       :scores   {roster-id {:official      96.1
                             :starter-ids   [\"4034\" ...]
                             :player-points {\"4034\" 18.4}}}}

  `:roster-ids` holds ONE id for a roster with no opponent — an odd league, or a
  provider that gives a team a bye — rather than being absent. A team with
  nobody to play still has a lineup and still scores, and dropping it would make
  the one manager it happens to look like a sync failure."
  (:require [draft-day.ingestion.league-sync :as league-sync]))

(defmulti fetch-raw-matchups
  "Network: raw provider-specific matchup payload for one league and week.
  Throws ex-info with :status on failure, as the other two pairs do."
  (fn [provider _league-id _week] provider))

(defmethod fetch-raw-matchups :default
  [provider _league-id _week]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-matchups
  "Pure: a provider's raw matchup payload -> `{:matchups [...] :scores {...}}`,
  in the shape the ns docstring names.

  `:player-points` must cover the whole roster, not only the starters: the
  optimal-lineup half of the board exists to say what a bench player would have
  scored, and it cannot say it from the starters alone.

  A provider that scores nothing yet — every game still to kick off — should
  return whatever it publishes rather than an empty map. Deciding that a zero is
  really 'has not played' is the *board's* judgment, made against the kickoff
  clock in `rankings.matchup`, and a provider that pre-empted it here would take
  away the one signal that tells the two apart."
  (fn [provider _raw] provider))

(defmulti current-week
  "Network: which week this provider is currently showing. See the ns docstring
  for why this is asked rather than computed.

  Throws ex-info with :status on failure. nil is a legitimate answer only in the
  sense that a provider between seasons may have no week to name; callers treat
  that as 'no matchup to show', never as week zero."
  (fn [provider] provider))

(defmethod current-week :default
  [provider]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defn fetch-matchups
  "{:provider :league-id :week} -> {:ok true :week n :matchups [...] :scores {...}}
  or {:ok false :status :error}.

  The same envelope `sync-league` and `import-league` return, so `routes`
  handles all of them with one shape.

  Sequential rather than concurrent, for `find-leagues`' reason exactly: the
  matchup document is addressed *by* week, so there is nothing to overlap. An
  explicit `:week` skips the first call entirely.

  The unwrap is not optional even though nothing here derefs a future — a
  provider is free to fetch its own documents concurrently, and
  `league-sync/unwrap-execution` exists so the first one that does is not the
  one that discovers every 404 reports as a 502."
  [{:keys [provider league-id week]}]
  (let [provider (keyword provider)]
    (try
      (let [wk (or week (current-week provider))]
        (if-not wk
          {:ok false :status 404 :error "No current week for this provider"}
          ;; The envelope's own keys go on LAST. `:ok` and `:week` are this
          ;; dispatcher's guarantee, not a provider's to set, and
          ;; `normalize-matchups` is contracted for `{:matchups :scores}` —
          ;; merging it over the top would let the first provider to return a
          ;; stray `:ok` be the one that discovers this.
          (merge (normalize-matchups provider
                                     (fetch-raw-matchups provider league-id wk))
                 {:ok true :week wk})))
      (catch Exception e
        (let [cause (league-sync/unwrap-execution e)]
          {:ok false
           :status (or (:status (ex-data cause)) 502)
           :error  (ex-message cause)})))))
