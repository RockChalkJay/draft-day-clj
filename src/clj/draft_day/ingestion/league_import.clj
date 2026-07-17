(ns draft-day.ingestion.league-import
  "Provider-agnostic league import. A provider namespace (e.g. league_import/sleeper.clj)
  registers itself by defmethod-ing onto fetch-raw-league/normalize-league — this
  namespace never requires a specific provider, so adding one is a new file, not
  a change here.")

(defmulti fetch-raw-league
  "Network: raw provider-specific league payload. Throws ex-info with :status on
  failure (e.g. 404 unknown league, 502 upstream error)."
  (fn [provider _league-id] provider))

(defmethod fetch-raw-league :default
  [provider _league-id]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-league
  "Pure: a provider's raw league payload -> {:scoring :roster :num-teams :name :season}."
  (fn [provider _raw] provider))

(defn import-league
  "{:provider :league-id} -> {:ok true :config {...}} or {:ok false :status :error}."
  [{:keys [provider league-id]}]
  (let [provider (keyword provider)]
    (try
      (let [raw (fetch-raw-league provider league-id)
            cfg (normalize-league provider raw)]
        {:ok true :config cfg})
      (catch clojure.lang.ExceptionInfo e
        {:ok false :status (or (:status (ex-data e)) 502) :error (ex-message e)})
      (catch Exception e
        {:ok false :status 502 :error (ex-message e)}))))
