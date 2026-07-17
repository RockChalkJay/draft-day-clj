(ns draft-day.api.routes
  "Stateless JSON API. The browser owns draft state and sends only the lightweight
  LeagueState + config + profile; the server runs static+live valuation on the
  cached universe and returns the valued board. Also serves the compiled SPA."
  (:require [clojure.string :as str]
            [reitit.ring :as ring]
            [reitit.ring.middleware.parameters :as parameters]
            [jsonista.core :as json]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.sleeper]
            [draft-day.rankings.engine :as engine]
            [draft-day.rankings.scoring :as scoring]
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
        {:keys [players source]} (universe refresh?)]
    (json-response 200 {:players players :count (count players) :source source})))

(defn scoring-presets-handler [_]
  (json-response 200 {:presets scoring/presets :stat-keys scoring/stat-keys}))

(defn league-import-handler [req]
  (let [{:keys [provider league-id]} (read-json-body req)]
    (if (str/blank? league-id)
      (json-response 400 {:error "league-id is required"})
      (let [{:keys [ok config status error]} (league-import/import-league
                                               {:provider provider :league-id league-id})]
        (if ok
          (json-response 200 config)
          (json-response status {:error error}))))))

(defn- resolve-scoring [s]
  (cond
    (map? s) s

    (or (string? s) (keyword? s))  
    (get scoring/presets (keyword s) (:ppr scoring/presets))
    
    :else                          
    (:ppr scoring/presets)))


(defn- coerce-league-state [ls]
  (update ls :drafted-player-ids set))

(defn- lens-worths
  "player-id -> Worth under a given profile (for cross-lens divergence badges)."
  [players scoring num-teams opts league-state profile]
  (-> (engine/static-rankings players scoring num-teams (assoc opts :profile profile))
      (engine/live-valuation league-state {:profile profile})
      :players
      (->> (into {} (map (juxt :player-id :worth))))))

(defn rankings-handler [req]
  (let [{:keys [scoring num-teams num-tiers replacement-config profile league-state]}
        (read-json-body req)
        players  (:players (universe false))
        scoring* (resolve-scoring scoring)
        nt       (or num-teams 12)
        opts     {:num-tiers (or num-tiers 5) :replacement-config replacement-config}
        prof     (if (string? profile) (keyword profile) (or profile :balanced))
        ls       (coerce-league-state league-state)
        live     (engine/live-valuation
                  (engine/static-rankings players scoring* nt (assoc opts :profile prof)) ls
                  {:profile prof})
        ;; also value under Floor/Ceiling so the client can badge lens-sensitive players
        floor-w  (lens-worths players scoring* nt opts ls :floor)
        ceil-w   (lens-worths players scoring* nt opts ls :ceiling)
        players* (mapv #(assoc % :worth-floor   (get floor-w (:player-id %) 0)
                                 :worth-ceiling (get ceil-w  (:player-id %) 0))
                       (:players live))]
    (json-response 200 (-> (select-keys live [:inflation :inflation-index
                                              :position-inflation :market-heat :pdm-map :profile])
                           (assoc :players players*)))))

(def app
  (ring/ring-handler
   (ring/router
    [["/api/health"   {:get  (fn [_] (json-response 200 {:status "ok" :service "draft-day-clj"}))}]
     ["/api/players"  {:get  players-handler}]
     ["/api/rankings" {:post rankings-handler}]
     ["/api/scoring/presets" {:get scoring-presets-handler}]
     ["/api/league/import"   {:post league-import-handler}]]
    {:data {:middleware [parameters/parameters-middleware]}})
   (ring/routes
    (ring/create-resource-handler {:path "/" :root "public"})
    (ring/create-default-handler))))
