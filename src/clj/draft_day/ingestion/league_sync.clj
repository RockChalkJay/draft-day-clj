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

(defn sync-league
  "{:provider :league-id} -> {:ok true :league {...}} or {:ok false :status :error}.

  The same envelope `league-import/import-league` returns, so `routes` handles
  both with one shape."
  [{:keys [provider league-id]}]
  (let [provider (keyword provider)]
    (try
      (let [raw (fetch-raw-rosters provider league-id)]
        {:ok true :league (normalize-rosters provider raw)})
      (catch clojure.lang.ExceptionInfo e
        {:ok false :status (or (:status (ex-data e)) 502) :error (ex-message e)})
      (catch Exception e
        {:ok false :status 502 :error (ex-message e)}))))
