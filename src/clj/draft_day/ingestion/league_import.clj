(ns draft-day.ingestion.league-import
  "Provider-agnostic league import. A provider namespace (e.g. league_import/sleeper.clj)
  registers itself by defmethod-ing onto fetch-raw-league/normalize-league — this
  namespace never requires a specific provider, so adding one is a new file, not
  a change here.

  The network multimethod takes one request map rather than positional
  arguments, so a host that needs a season in its URL or a cookie on its
  request has somewhere to read them from. `normalize-league` stays positional:
  it is pure, and a credential has no business reaching it.

  What `req` holds is this namespace's contract, not a provider's: the season
  is defaulted and the credentials are validated here, so no provider has to
  remember either. That is `league-sync/unwrap-execution`'s argument — the
  first implementation that forgot would regress it silently."
  (:require [draft-day.ingestion.season :as season]
            [draft-day.providers :as providers]))

(defmulti fetch-raw-league
  "Network: raw provider-specific league payload, from a request map of
  `{:league-id :season :credentials}`. Throws ex-info with :status on failure
  (e.g. 404 unknown league, 401 rejected credentials, 502 upstream error).

  It is never a Ring request. A provider sees only what this namespace built."
  (fn [provider _req] provider))

(defmethod fetch-raw-league :default
  [provider _req]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-league
  "Pure: a provider's raw league payload ->
  {:scoring :unsupported-scoring :roster :num-teams :starting-bankroll :name :season}.

  `:starting-bankroll` is the auction budget, nil for any other kind of draft."
  (fn [provider _raw] provider))

(defn import-league
  "{:provider :league-id :season :credentials} -> {:ok true :config {...}}
  or {:ok false :status :error}."
  [{:keys [provider league-id season credentials]}]
  (let [provider (keyword provider)]
    (if-let [bad (providers/league-access-error provider league-id credentials)]
      {:ok false :status 400 :error (:error bad)}
      (try
        (let [raw (fetch-raw-league provider {:league-id   league-id
                                              :season      (season/resolve-season season)
                                              :credentials credentials})]
          {:ok true :config (normalize-league provider raw)})
        (catch clojure.lang.ExceptionInfo e
          {:ok false :status (or (:status (ex-data e)) 502) :error (ex-message e)})
        (catch Exception e
          {:ok false :status 502 :error (ex-message e)})))))
