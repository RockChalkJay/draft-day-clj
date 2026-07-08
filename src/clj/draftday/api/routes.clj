(ns draftday.api.routes
  "Stateless JSON API. The browser owns draft state and sends only the lightweight
  LeagueState + config + profile; the server runs static+live valuation on the
  cached universe and returns the valued board. Also serves the compiled SPA."
  (:require [reitit.ring :as ring]
            [reitit.ring.middleware.parameters :as parameters]
            [jsonista.core :as json]
            [draftday.ingestion.pipeline :as pipeline]
            [draftday.rankings.engine :as engine]
            [draftday.rankings.scoring :as scoring]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))

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

(defn- resolve-scoring [s]
  (cond
    (map? s)     s
    (string? s)  (get scoring/presets (keyword s) (:ppr scoring/presets))
    (keyword? s) (get scoring/presets s (:ppr scoring/presets))
    :else        (:ppr scoring/presets)))

(defn- coerce-league-state [ls]
  (update ls :drafted-player-ids set))

(defn rankings-handler [req]
  (let [{:keys [scoring num-teams num-tiers replacement-config profile league-state]}
        (read-json-body req)
        players (:players (universe false))
        prof    (if (string? profile) (keyword profile) (or profile :balanced))
        static  (engine/static-rankings players (resolve-scoring scoring) (or num-teams 12)
                                        {:num-tiers          (or num-tiers 5)
                                         :replacement-config replacement-config
                                         :profile            prof})
        live    (engine/live-valuation static (coerce-league-state league-state) {:profile prof})]
    (json-response 200 (select-keys live [:players :inflation :inflation-index
                                          :position-inflation :market-heat :pdm-map :profile]))))

(def app
  (ring/ring-handler
   (ring/router
    [["/api/health"   {:get  (fn [_] (json-response 200 {:status "ok" :service "draft-day-clj"}))}]
     ["/api/players"  {:get  players-handler}]
     ["/api/rankings" {:post rankings-handler}]]
    {:data {:middleware [parameters/parameters-middleware]}})
   (ring/routes
    (ring/create-resource-handler {:path "/" :root "public"})
    (ring/create-default-handler))))
