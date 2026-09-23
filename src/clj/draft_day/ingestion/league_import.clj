(ns draft-day.ingestion.league-import
  "League import protocol shared across providers.
  Providers register `fetch-raw-league` and `normalize-league` by defmethod;
  this namespace stays provider-agnostic. The request map carries the provider's
  season and credentials, while the pure normalizer only receives the raw payload."
  (:require [draft-day.ingestion.season :as season]
            [draft-day.providers :as providers]))

(defmulti fetch-raw-league
  "Fetch the raw provider-specific league payload for a request map.
  The request shape is `{:league-id :season :credentials}`. Providers receive
  only that map and must raise `ex-info` with an HTTP-like `:status` on failure."
  (fn [provider _req] provider))

(defmethod fetch-raw-league :default
  [provider _req]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-league
  "Normalize a provider's raw league payload into the app's league config.
  The result includes scoring, unsupported rules, roster info, team count, and
  the auction bankroll when present; `:starting-bankroll` is nil for non-auction drafts."
  (fn [provider _raw] provider))

(defn import-league
  "Import a league for a provider and return a normalized config or an error map.
  Input shape: `{:provider :league-id :season :credentials}`. Successful calls
  return `{:ok true :config ...}`; failures return `{:ok false :status ...}`."
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
