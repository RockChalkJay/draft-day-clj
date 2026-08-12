(ns draft-day.api.routes
  "Stateless JSON API. The browser owns draft state and sends only the lightweight
  LeagueState + config; the server runs static+live valuation on the cached
  universe and returns the valued board. Also serves the compiled SPA."
  (:require [clojure.string :as str]
            [reitit.ring :as ring]
            [reitit.ring.middleware.parameters :as parameters]
            [jsonista.core :as json]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.sleeper]
            [draft-day.rankings.engine :as engine]
            [draft-day.scoring :as scoring]
            [draft-day.rankings.market :as market]
            [draft-day.rankings.league-state :as ls]
            [draft-day.json :refer [mapper]]))

(defn- json-response [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (json/write-value-as-string body)})

(defn- read-json-body [req]
  (when-let [b (:body req)]
    (json/read-value b mapper)))

;; The universe is shared (not per-session) state; hold it in an atom so we don't
;; re-read disk on every rankings call.
(defonce ^:private universe-cache (atom nil))

(defn- universe [refresh?]
  (if (and (not refresh?) @universe-cache)
    @universe-cache
    (reset! universe-cache (pipeline/load-universe {:refresh refresh?}))))

(defn reset-universe!
  "Drop the in-memory universe so the next request reloads it (used in tests)."
  []
  (reset! universe-cache nil))

(defn players-handler [req]
  (let [refresh? (= "true" (get-in req [:query-params "refresh"]))
        {:keys [players source] :as u} (universe refresh?)]
    ;; :players/:count/:source stay top-level for existing clients; :universe is
    ;; the provenance (season, fetched-at, what validation dropped) that makes
    ;; "cache" a checkable claim rather than an unfalsifiable one.
    (json-response 200 {:players  players
                        :count    (count players)
                        :source   source
                        :universe (dissoc u :players)})))

(defn cache-reset-handler [_]
  (pipeline/delete-cache! pipeline/default-cache-path)
  (reset-universe!)
  (json-response 200 {:status "ok"}))

(defn league-import-handler [req]
  (let [{:keys [provider league-id]} (read-json-body req)
        league-id (str league-id)]
    (cond
      (str/blank? league-id)
      (json-response 400 {:error "league-id is required"})

      (not (re-matches #"\d+" league-id))
      (json-response 400 {:error "league-id must be numeric"})

      :else
      (let [{:keys [ok config status error]}
            (league-import/import-league {:provider provider :league-id league-id})]
        (if ok
          (json-response 200 config)
          (json-response status {:error error}))))))

(defn resolve-scoring
  "Coerce the request's scoring field into a scoring config. A preset keyword or
  string resolves to its preset map; a custom {stat weight} map is bounded to
  known stat keys so an oversized client map can't amplify per-player scoring;
  anything else falls back to PPR."
  [s]
  (cond
    (map? s) (select-keys s scoring/stat-keys)

    (or (string? s) (keyword? s))
    (get scoring/presets (keyword s) (:ppr scoring/presets))

    :else
    (:ppr scoring/presets)))


(defn- coerce-league-state [ls]
  (update ls :drafted-player-ids set))

(defn rankings-handler [req]
  (try
    (let [{:keys [scoring num-teams num-tiers replacement-config league-state]}
          (read-json-body req)
          scoring* (resolve-scoring scoring)]
      ;; An empty or all-zero custom map is not a league — it scores every player
      ;; 0.0 and prices the whole board at $0. Say so rather than returning a
      ;; plausible-looking board of zeroes.
      (if-not (scoring/scores-anything? scoring*)
        (json-response 400 {:error "scoring config has no non-zero weights"})
        (let [players  (:players (universe false))
              nt       (or num-teams 12)
              opts     {:num-tiers (or num-tiers 5) :replacement-config replacement-config}
              ls       (coerce-league-state league-state)
              live     (engine/live-valuation
                        (engine/static-rankings players scoring* nt opts) ls)
              ;; reference market price + edge, scaled to this league's pool
              players* (market/with-market (:players live) (ls/initial-cash ls))]
          (json-response 200 (-> (select-keys live [:inflation :inflation-index :market-heat])
                                 (assoc :players players*))))))
    (catch Exception e
      (json-response 400 {:error (str "invalid request: " (ex-message e))}))))

(def app
  (ring/ring-handler
   (ring/router
    [["/api/health"   {:get  (fn [_] (json-response 200 {:status "ok" :service "draft-day-clj"}))}]
     ["/api/players"  {:get  players-handler}]
     ["/api/cache/reset" {:post cache-reset-handler}]
     ["/api/rankings" {:post rankings-handler}]
     ["/api/league/import"   {:post league-import-handler}]]
    {:data {:middleware [parameters/parameters-middleware]}})
   (ring/routes
    (ring/create-resource-handler {:path "/" :root "public"})
    (ring/create-default-handler))))
