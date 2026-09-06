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
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.sleeper :as import-sleeper]))

(def ^:private base "https://api.sleeper.app/v1/")

(defn get-json
  "Network: one Sleeper document at `path` under the v1 base, as parsed JSON.

  `:empty-is-missing?` is a parameter rather than a constant because Sleeper
  answers 'no such thing' and 'nothing here' with the same status and different
  bodies, and the two need opposite treatment:

  - an unknown league or user is `200` with `null` — reading that as an empty
    league would hand back a waiver board listing the whole NFL as available, so
    it must throw 404;
  - a user with no leagues this season is `200` with `[]` — a real answer, and
    throwing 404 there would tell a manager his account does not exist.

  Both confirmed against the live API. `empty?` cannot tell them apart, which is
  exactly why the caller has to say which it expects."
  [path {:keys [empty-is-missing? not-found-msg]}]
  (let [{:keys [status body error]} @(http/get (str base path) {:timeout 30000})]
    (cond
      error             (throw (ex-info (str "Sleeper " path " fetch failed")
                                        {:status 502 :error error}))
      (not= 200 status) (throw (ex-info (str "Sleeper " path " non-200")
                                        {:status 502 :sleeper-status status}))
      :else
      (let [parsed (json/read-value body mapper)]
        (if (and empty-is-missing? (empty? parsed))
          (throw (ex-info (or not-found-msg "Sleeper resource not found") {:status 404}))
          parsed)))))

(defn fetch-json
  "One of a league's sub-resources. Missing means missing — see `get-json`."
  [league-id path]
  (get-json (str "league/" league-id "/" path)
            {:empty-is-missing? true :not-found-msg "Sleeper league not found"}))

;; The thunks here throw rather than being best-effort — a 404 from Sleeper is
;; the answer, not a missing column — so the ex-info comes back out of
;; `parallel/all` wrapped in an ExecutionException. `league-sync/unwrap-execution`
;; peels it where the status is read, so no provider has to remember to.
(defmethod league-sync/fetch-raw-rosters :sleeper
  [_ league-id]
  (parallel/all
   {:rosters (fn [] (fetch-json league-id "rosters"))
    :users   (fn [] (fetch-json league-id "users"))
    :league  (fn [] (league-import/fetch-raw-league :sleeper league-id))}))

(defn normalize-user
  "Pure: Sleeper's user document -> `{:user-id :display-name :avatar}`.

  Built, never passed through. The raw document carries `email`, `phone` and
  `token` alongside the three fields the app wants, and this function is the only
  thing standing between them and the browser — `/api/league/user` returns
  whatever it hands back."
  [raw]
  {:user-id      (str (:user_id raw))
   :display-name (or (not-empty (:display_name raw)) (:username raw))
   :avatar       (:avatar raw)})

(defmethod league-sync/find-user :sleeper
  [_ username]
  (normalize-user
   (get-json (str "user/" username)
             {:empty-is-missing? true :not-found-msg "Sleeper user not found"})))

(defn normalize-league-entry
  "Pure: one league from a user's league list -> what a picker needs.

  `:status` rides along because 'pre_draft' and 'in_season' are the difference
  between a league worth syncing rosters from and one that has none yet."
  [raw]
  {:league-id (str (:league_id raw))
   :name      (:name raw)
   :season    (:season raw)
   :num-teams (:total_rosters raw)
   :status    (:status raw)
   :avatar    (:avatar raw)})

(defmethod league-sync/list-leagues :sleeper
  [_ user-id season]
  (let [season (or season (sleeper/current-season))]
    ;; empty-is-missing? false: a manager with no leagues this season gets `[]`
    ;; with status 200, and that is an answer, not a missing account.
    (mapv normalize-league-entry
          (get-json (str "user/" user-id "/leagues/nfl/" season)
                    {:empty-is-missing? false}))))

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
  (let [s       (:settings roster)
        used    (or (:waiver_budget_used s) 0)
        owner   (:owner_id roster)
        ;; Sleeper sends null, not [], for a roster nobody has drafted to yet,
        ;; and for `starters` in a league that has not set a lineup. Normalized
        ;; once here so no consumer has to handle both spellings of empty.
        players (vec (:players roster))
        parked  (into #{} cat [(:reserve roster) (:taxi roster)])]
    {:roster-id       (:roster_id roster)
     :owner-id        owner
     :name            (or (get names owner) (str "Roster " (:roster_id roster)))
     :player-ids      players
     ;; Who occupies a seat a claim would need. `players` includes IR and taxi,
     ;; and they matter in opposite directions: counted, they fill a roster that
     ;; is not actually full; offered as a drop, they free no seat for the claim
     ;; being priced. They stay in `:player-ids` regardless, because a player on
     ;; IR is rostered — he is not a free agent.
     :active-ids      (filterv (complement parked) players)
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
     ;; This league's seats, not the draft config's. Whether a claim costs a
     ;; drop turns on this number, and a manager who syncs without importing
     ;; has never told the app what his real league looks like.
     :roster-size (count (:roster_positions league))
     ;; Carried so a re-sync is one click. It is the only thing the sync needs
     ;; and the only thing the reply did not have: without it the id lives in a
     ;; component-local atom that empties on reload, and a manager comes back to
     ;; month-old rosters with the re-sync button greyed out.
     :league-id   (str (:league_id league))
     ;; Carried on the sync as well as the import because the waiver board is
     ;; driven by the sync alone — a manager who syncs rosters without
     ;; re-importing the rules should still get bids bounded by the right
     ;; number of runs.
     :playoff-week-start (import-sleeper/playoff-week-start league)
     :name   (:name league)
     :season (:season league)}))
