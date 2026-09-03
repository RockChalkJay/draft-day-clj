(ns draft-day.ingestion.league-sync
  "Provider-agnostic league *sync*: who is on whose roster right now, and what
  each manager has left to bid with.

  The in-season sibling of `league-import`, and deliberately a separate pair of
  multimethods rather than more keys on that one. They differ in the only way
  that matters for a cache: an import is a league's *rules*, which change maybe
  once a year, while a sync is its *state*, which changes every time anyone
  makes a claim. Folding the second into the first would mean either re-fetching
  the rules on every waiver refresh or serving month-old rosters.

  Same registration convention as `league-import`: a provider namespace
  defmethods onto both multimethods and this namespace never requires one, so
  adding Yahoo or ESPN is a new file plus a `:require` in `routes`, not a change
  here.")

(defmulti fetch-raw-rosters
  "Network: raw provider-specific roster payload. Throws ex-info with :status on
  failure, exactly as `league-import/fetch-raw-league` does."
  (fn [provider _league-id] provider))

(defmethod fetch-raw-rosters :default
  [provider _league-id]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-rosters
  "Pure: a provider's raw roster payload ->

    {:teams  [{:roster-id :owner-id :name :player-ids :starter-ids
               :faab-used :faab-left :waiver-position :wins :losses}]
     :waiver {:type :faab|:rolling|:reverse-standings :budget n}}"
  (fn [provider _raw] provider))

(defn unwrap-execution
  "An `ExecutionException`'s cause in its place; anything else unchanged.

  A provider is free to fetch its documents concurrently, and a future's deref
  wraps whatever the thunk threw in a `java.util.concurrent.ExecutionException`,
  which carries no ex-data of its own. So an `ex-info` saying `{:status 404}`
  arrives looking like a generic failure, and an unknown league id reports as a
  502 upstream error.

  It lives here rather than in a provider because the contract it protects is
  `fetch-raw-rosters`' — the multimethod's, shared by every provider — and
  because `sync-league` is what reads the status back out. A provider that
  fetches concurrently should not have to know its own 404 needs rescuing; the
  first one that forgets would regress this silently.

  Not in `parallel/all` either: that namespace declares its thunks best-effort,
  so a throwing thunk is already outside its contract, and every other caller
  wraps in `pipeline/best-effort` and never throws at all."
  [e]
  (if (instance? java.util.concurrent.ExecutionException e)
    (or (.getCause e) e)
    e))

(defn sync-league
  "{:provider :league-id} -> {:ok true :league {...}} or {:ok false :status :error}.

  The same envelope `league-import/import-league` returns, so `routes` handles
  both with one shape."
  [{:keys [provider league-id]}]
  (let [provider (keyword provider)]
    (try
      (let [raw (fetch-raw-rosters provider league-id)]
        {:ok true :league (normalize-rosters provider raw)})
      ;; One catch rather than two: the unwrap has to happen before the status is
      ;; read, and `ex-data` is nil for anything that is not an ex-info, so the
      ;; 502 default already covers what the second clause used to.
      (catch Exception e
        (let [cause (unwrap-execution e)]
          {:ok false
           :status (or (:status (ex-data cause)) 502)
           :error  (ex-message cause)})))))
