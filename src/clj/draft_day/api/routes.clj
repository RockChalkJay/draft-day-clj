(ns draft-day.api.routes
  "Stateless JSON API. The browser owns draft state and sends only the lightweight
  LeagueState + config; the server runs static+live valuation on the cached
  universe and returns the valued board. Also serves the compiled SPA."
  (:require [clojure.string :as str]
            [reitit.ring :as ring]
            [reitit.ring.middleware.parameters :as parameters]
            [jsonista.core :as json]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.sleeper]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.sleeper]
            [draft-day.rankings.engine :as engine]
            [draft-day.rankings.ros :as ros]
            [draft-day.rankings.waiver :as waiver]
            [draft-day.scoring :as scoring]
            [draft-day.rankings.market :as market]
            [draft-day.rankings.vendor :as vendor]
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
    ;;
    ;; The per-format vendor bundle is stripped rather than flattened: there is
    ;; no league here to pick a format for, and the client reads the flat vendor
    ;; columns off /api/rankings anyway.
    (json-response 200 {:players  (vendor/without-bundle players)
                        :count    (count players)
                        :source   source
                        :universe (dissoc u :players)})))

(defn cache-reset-handler [_]
  (pipeline/delete-cache! pipeline/default-cache-path)
  (reset-universe!)
  (json-response 200 {:status "ok"}))

(defn league-id-error
  "The 400 body for an unusable league id, or nil when it is fine.

  Shared by import and sync because they take the same identifier from the same
  field, and a second copy of the rule is how one endpoint ends up accepting
  what the other rejects."
  [league-id]
  (cond
    (str/blank? league-id)          {:error "league-id is required"}
    (not (re-matches #"\d+" league-id)) {:error "league-id must be numeric"}))

(defn league-import-handler [req]
  (let [{:keys [provider league-id]} (read-json-body req)
        league-id (str league-id)]
    (if-let [bad (league-id-error league-id)]
      (json-response 400 bad)
      (let [{:keys [ok config status error]}
            (league-import/import-league {:provider provider :league-id league-id})]
        (if ok
          (json-response 200 config)
          (json-response status {:error error}))))))

(defn league-sync-handler
  "Who is rostered right now. Separate from the import for the same reason the
  namespaces are: an import is the league's rules and a sync is its state, and
  the state changes every time anyone in the league makes a claim."
  [req]
  (let [{:keys [provider league-id]} (read-json-body req)
        league-id (str league-id)]
    (if-let [bad (league-id-error league-id)]
      (json-response 400 bad)
      (let [{:keys [ok league status error]}
            (league-sync/sync-league {:provider provider :league-id league-id})]
        (if ok
          (json-response 200 league)
          (json-response status {:error error}))))))

(defn resolve-scoring
  "Coerce the request's scoring field into a scoring config, bounded to known
  stat keys so an oversized client map can't amplify per-player scoring.

  The preset-or-map coercion itself is `scoring/resolve-config`, shared with the
  browser: the client picks which vendor format to warn about from the same
  field, and the two spellings of this `cond` had already drifted on strings."
  [s]
  (select-keys (scoring/resolve-config s) scoring/stat-keys))


(defn- coerce-league-state [ls]
  (update ls :drafted-player-ids set))

(defn without-history
  "Drop `:nflverse/history` before ranking.

  Nothing in the valuation pipeline reads it, and this response is the whole
  board — re-POSTed after every pick and after every debounced settings edit —
  so three seasons of stat lines per player is weight on the hottest path in the
  app for data the engine never touches. Same argument as `vendor/with-format`
  dropping `:vendor/by-format`.

  The client does not lose it: `/api/players` ships the untrimmed universe once
  (`players-handler`), and the board's static facts are read from there while
  live valuation is read from here."
  [players]
  (mapv #(dissoc % :nflverse/history) players))

(defn rankings-handler [req]
  (try
    (let [{:keys [scoring num-teams replacement-config league-state]}
          (read-json-body req)
          scoring* (resolve-scoring scoring)]
      ;; An empty or all-zero custom map is not a league — it scores every player
      ;; 0.0 and prices the whole board at $0. Say so rather than returning a
      ;; plausible-looking board of zeroes.
      (cond
        (not (scoring/scores-anything? scoring*))
        (json-response 400 {:error "scoring config has no non-zero weight on a projected stat"})

        ;; A room that cannot put a dollar on every slot it has to fill is not a
        ;; league. Left alone the board hands out one dollar per slot anyway and
        ;; reports more money than the room holds — 150% of it at $10 bankrolls —
        ;; with every row reading $1 and nothing to tell them apart. Same argument
        ;; as the all-zero scoring map above: that is a lie, not a board.
        (let [ls (coerce-league-state league-state)]
          (and (seq (:teams ls))
               (< (ls/initial-cash ls) (ls/total-slots ls))))
        (json-response 400 {:error "each team's bankroll must cover $1 per roster slot"})

        :else
        (let [players  (-> (:players (universe false))
                           (vendor/for-scoring scoring*)
                           without-history)
              nt       (or num-teams 12)
              opts     {:replacement-config replacement-config}
              ls       (coerce-league-state league-state)
              live     (engine/live-valuation
                        (engine/static-rankings players scoring* nt opts) ls)
              ;; reference market price + edge, scaled to this league's pool
              players* (market/with-market (:players live) (ls/initial-cash ls))]
          (json-response 200 (-> (select-keys live [:inflation :inflation-index :market-heat])
                                 (assoc :players players*))))))
    (catch Exception e
      (json-response 400 {:error (str "invalid request: " (ex-message e))}))))

(defn waivers-handler
  "The in-season board: rest-of-season value over the free agents a synced
  league actually leaves available, and what to bid for them.

  Stateless on exactly the same terms as `rankings-handler` — the browser owns
  the synced league and re-POSTs it — but it shares none of that handler's
  money. Auction dollars price a whole roster out of a fixed bankroll on draft
  night; a waiver claim is one seat against a budget spent down over months. See
  `rankings.waiver`.

  `:through-week` comes from the universe envelope rather than from the request,
  so the board cannot be asked to price a week the data has not reached. In
  preseason it is 0 and this is the preseason board, which is the honest answer
  rather than an error."
  [req]
  (try
    (let [{:keys [scoring num-teams replacement-config league my-roster-id roster-size]}
          (read-json-body req)
          scoring* (resolve-scoring scoring)]
      (if-not (scoring/scores-anything? scoring*)
        ;; Same guard and the same reason as the rankings board: an all-zero
        ;; config scores every player 0.0, and a waiver board where nobody is an
        ;; upgrade over anybody is a lie, not a board.
        (json-response 400 {:error "scoring config has no non-zero weight on a projected stat"})
        (let [{:keys [players season through-week]} (universe false)
              season-games (nflverse/games-in-season (or season (sleeper/current-season)))
              ctx      {:league             league
                        :my-roster-id       my-roster-id
                        :roster-size        roster-size
                        :num-teams          (or num-teams 12)
                        :replacement-config replacement-config
                        :through-week       (or through-week 0)
                        :season-games       season-games
                        :playoff-week-start (:playoff-week-start league)}
              board    (-> players
                           (vendor/for-scoring scoring*)
                           without-history
                           (ros/with-ros scoring* ctx))]
          (json-response 200 (assoc (waiver/waiver-board board ctx)
                                    :through-week (or through-week 0)
                                    :season-games season-games)))))
    (catch Exception e
      (json-response 400 {:error (str "invalid request: " (ex-message e))}))))

(def app
  (ring/ring-handler
   (ring/router
    [["/api/health"   {:get  (fn [_] (json-response 200 {:status "ok" :service "draft-day-clj"}))}]
     ["/api/players"  {:get  players-handler}]
     ["/api/cache/reset" {:post cache-reset-handler}]
     ["/api/rankings" {:post rankings-handler}]
     ["/api/waivers"  {:post waivers-handler}]
     ["/api/league/import"   {:post league-import-handler}]
     ["/api/league/sync"     {:post league-sync-handler}]]
    {:data {:middleware [parameters/parameters-middleware]}})
   (ring/routes
    (ring/create-resource-handler {:path "/" :root "public"})
    (ring/create-default-handler))))
