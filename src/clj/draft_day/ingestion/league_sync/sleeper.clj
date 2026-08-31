(ns draft-day.ingestion.league-sync.sleeper
  "Sleeper provider for league sync: who is rostered right now, and what each
  manager has left to bid with.

  Three documents, fetched together because they answer one question and none of
  them depends on the others: the rosters (who holds whom), the users (whose
  roster it is), and the league itself (whether waivers are FAAB at all and for
  how much). `parallel/all` starts them at once — the same reason
  `pipeline/enrichment-tasks` exists, at a much smaller scale.

  The league document is fetched through `league-import/fetch-raw-league` rather
  than by building its URL again here. That is the one copy of both the endpoint
  and its error handling — including Sleeper's habit of answering an unknown
  league id with HTTP 200 and a JSON null body, which is a trap worth having in
  exactly one place.

  ROSTER IDS ARE SLEEPER IDS. This namespace deliberately does not translate
  them: the crosswalk to the canonical GSIS ids the board is keyed by lives in
  `db/sleeper->player-id` and needs the universe, which ingestion of a *league*
  has no business loading. `rankings.waiver` does the mapping where both halves
  are already in hand."
  (:require [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draft-day.json :refer [mapper]]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.sleeper :as import-sleeper]))

(defn- league-path [league-id path]
  (str "https://api.sleeper.app/v1/league/" league-id "/" path))

(defn fetch-json
  "Network: one Sleeper sub-resource as parsed JSON.

  A null or empty body is an error rather than an empty league: Sleeper answers
  an unknown id with 200 and `null`, and a sync that read that as 'nobody is
  rostered' would hand the manager a waiver board listing every player in the
  NFL as available."
  [league-id path]
  (let [{:keys [status body error]} @(http/get (league-path league-id path) {:timeout 30000})]
    (cond
      error             (throw (ex-info (str "Sleeper " path " fetch failed")
                                        {:status 502 :error error}))
      (not= 200 status) (throw (ex-info (str "Sleeper " path " non-200")
                                        {:status 502 :sleeper-status status}))
      :else
      (let [parsed (json/read-value body mapper)]
        (if (empty? parsed)
          (throw (ex-info "Sleeper league not found" {:status 404}))
          parsed)))))

(defn unwrapped
  "Run `f`, re-throwing an `ExecutionException`'s cause in its place.

  `parallel/all` derefs futures, and a future's deref wraps whatever the thunk
  threw in a `java.util.concurrent.ExecutionException` — so an `ex-info` carrying
  `{:status 404}` arrives as a plain Exception with no ex-data at all, and
  `league-sync/sync-league` reports every unknown league id as a 502 upstream
  failure. Nothing in `pipeline` ever hit this because every task there is
  wrapped in `best-effort`, which catches *inside* the thunk; this is the first
  caller that wants the exception back.

  Unwrapped here rather than in `parallel/all` because that function is on the
  cold-load path where the escape behaviour is deliberately blunt, and because
  the contract this restores is `fetch-raw-rosters`': it promises an ex-info
  with a :status, and it should keep that promise itself."
  [f]
  (try (f)
       (catch java.util.concurrent.ExecutionException e
         (throw (or (.getCause e) e)))))

(defmethod league-sync/fetch-raw-rosters :sleeper
  [_ league-id]
  (unwrapped
   #(parallel/all
     {:rosters (fn [] (fetch-json league-id "rosters"))
      :users   (fn [] (fetch-json league-id "users"))
      :league  (fn [] (league-import/fetch-raw-league :sleeper league-id))})))

(defn team-names
  "Pure: raw users -> `{user-id display-name}`.

  A manager's own team name wins over the account's display name, because that
  is what everyone in the league calls his team. Sleeper stores it under
  `metadata.team_name` and leaves it absent until it is set."
  [users]
  (into {}
        (map (fn [u] [(:user_id u)
                      (or (not-empty (get-in u [:metadata :team_name]))
                          (not-empty (:display_name u)))]))
        users))

(defn normalize-roster
  "Pure: one raw Sleeper roster + the name index + the league's waiver settings
  -> one normalized team.

  `:faab-left` is derived here rather than left to the caller because the two
  halves come from different documents — the budget from the league, the spend
  from the roster — and every consumer that recomputed it would need both.

  An orphan roster (no owner) keeps its seat: it still holds players, and its
  budget can still outbid yours. It is named for its slot rather than dropped."
  [names {:keys [type budget]} roster]
  (let [s     (:settings roster)
        used  (or (:waiver_budget_used s) 0)
        owner (:owner_id roster)]
    {:roster-id       (:roster_id roster)
     :owner-id        owner
     :name            (or (get names owner) (str "Roster " (:roster_id roster)))
     ;; Sleeper sends null, not [], for a roster nobody has drafted to yet, and
     ;; for `starters` in a league that has not set a lineup. Normalized here so
     ;; no consumer downstream has to handle both spellings of empty.
     :player-ids      (vec (:players roster))
     :starter-ids     (vec (:starters roster))
     :faab-used       used
     :faab-left       (when (= :faab type) (max 0 (- (or budget 0) used)))
     :waiver-position (:waiver_position s)
     :wins            (:wins s)
     :losses          (:losses s)}))

(defmethod league-sync/normalize-rosters :sleeper
  [_ {:keys [rosters users league]}]
  (let [waiver (import-sleeper/waiver-settings league)
        names  (team-names users)]
    {:teams  (mapv #(normalize-roster names waiver %) rosters)
     :waiver waiver
     :name   (:name league)
     :season (:season league)}))
