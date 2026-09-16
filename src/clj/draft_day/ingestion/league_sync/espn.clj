(ns draft-day.ingestion.league-sync.espn
  "ESPN provider for league sync: who is rostered right now, what each manager
  has left to bid with, and which leagues an account plays in.

  One league document with three views, fetched through
  `league-import.espn/get-json` rather than by rebuilding the URL and its error
  handling here — the arrangement `league-sync.sleeper` has with its own
  import, and for the same reason.

  ROSTER IDS ARE ESPN IDS, spelled as strings. ESPN publishes integers and the
  crosswalk `db/provider->player-id` builds is string-keyed, so `league-sync`
  coerces them for every provider; this namespace emits strings anyway so the
  two never have to disagree.

  Team defenses are the exception, and they have to be. `player-ids/attach-ids`
  gives a defense `:ids {:sleeper \"ARI\" :team \"ARI\"}` and no ESPN entry at
  all — the id file it reads is a player file with no defense rows — so a
  defense keyed by its ESPN id resolves to nobody and every league's defenses
  sit on the free-agent board while their owners hold them. A roster entry at
  ESPN's D/ST position is keyed by its team abbreviation instead, which is the
  id space the board already uses, and `waiver/held-ids`' identity fallback
  carries it the rest of the way.

  Discovery is deliberately the weak half. ESPN's fan endpoint is undocumented
  and is the only thing here that could change shape without warning, so
  `find-user` never touches it: the SWID *is* the identity, so a listing that
  breaks costs the league picker and not the connection. `league-sync/find-leagues`
  is what turns that into a reported gap."
  (:require [clojure.string :as str]
            [jsonista.core :as json]
            [org.httpkit.client :as http]
            [draft-day.ingestion.league-import.espn :as import-espn]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.teams :as teams]
            [draft-day.json :refer [mapper]]))

(def dst-position-id
  "ESPN's `defaultPositionId` for a team defense."
  16)

(def bench-slot 20)

(def pro-team-abbrev
  "ESPN proTeamId -> ESPN's own team abbreviation.

  Its spellings, not the app's: `teams/normalize :espn` is what turns `WSH`
  into `WAS`, and hardcoding the app's spelling here would put that alias rule
  in two places. `teams/covers-vocabulary?` is the test that all thirty-two
  land on a team the board knows."
  {1  "ATL"  2  "BUF"  3  "CHI"  4  "CIN"  5  "CLE"  6  "DAL"
   7  "DEN"  8  "DET"  9  "GB"   10 "TEN"  11 "IND"  12 "KC"
   13 "LV"   14 "LAR"  15 "MIA"  16 "MIN"  17 "NE"   18 "NO"
   19 "NYG"  20 "NYJ"  21 "PHI"  22 "ARI"  23 "PIT"  24 "LAC"
   25 "SF"   26 "SEA"  27 "TB"   28 "WSH"  29 "CAR"  30 "JAX"
   33 "BAL"  34 "HOU"})

(defn entry-player-id
  "Pure: one roster entry -> the id the board is keyed by, or nil.

  A defense is its team abbreviation; everyone else is his ESPN id as a string.
  See the ns docstring for why the two cannot share a spelling."
  [entry]
  (let [p (get-in entry [:playerPoolEntry :player])]
    (if (= dst-position-id (:defaultPositionId p))
      (teams/normalize :espn (pro-team-abbrev (:proTeamId p)))
      (some-> (:playerId entry) str not-empty))))

(defn team-name
  "Pure: what everyone in the league calls this team.

  ESPN moved from location+nickname to a single `name` field, and leagues
  carried over from before still send both."
  [team]
  (or (not-empty (:name team))
      (not-empty (str/trim (str (:location team) " " (:nickname team))))
      (str "Team " (:id team))))

(defn normalize-team
  "Pure: one raw ESPN team + the league's waiver settings -> one normalized team.

  `:active-ids` excludes the injured-reserve seat and `:starter-ids` excludes
  the bench as well. Unlike Sleeper's, neither is positional: an ESPN entry
  names its own seat on `lineupSlotId`.

  `:faab-left` is derived here because its halves come from different parts of
  the document — the budget from the league's settings, the spend from the team."
  [{:keys [type budget]} team]
  (let [entries (get-in team [:roster :entries])
        ids     (fn [pred] (into [] (comp (filter pred) (keep entry-player-id)) entries))
        used    (or (get-in team [:transactionCounter :acquisitionBudgetSpent]) 0)]
    {:roster-id       (:id team)
     :owner-id        (or (first (:owners team)) (:primaryOwner team))
     :name            (team-name team)
     :player-ids      (ids (constantly true))
     :active-ids      (ids #(not= import-espn/ir-slot (:lineupSlotId %)))
     :starter-ids     (ids #(not (#{import-espn/ir-slot bench-slot} (:lineupSlotId %))))
     :faab-used       used
     :faab-left       (when (= :faab type) (max 0 (- (or budget 0) used)))
     :waiver-position (:waiverRank team)
     :wins            (get-in team [:record :overall :wins])
     :losses          (get-in team [:record :overall :losses])}))

(defmethod league-sync/fetch-raw-rosters :espn
  [_ {:keys [league-id season credentials]}]
  (import-espn/get-json {:season season :league-id league-id :credentials credentials
                         :views ["mTeam" "mRoster" "mSettings"]
                         :require-key :teams}))

(defmethod league-sync/normalize-rosters :espn
  [_ raw]
  (let [waiver (import-espn/waiver-settings raw)
        seats  (import-espn/roster-positions
                (get-in raw [:settings :rosterSettings :lineupSlotCounts]))]
    {:teams            (mapv #(normalize-team waiver %) (:teams raw))
     :waiver           waiver
     :roster-size      (count seats)
     :roster-positions seats
     :league-id        (str (:id raw))
     ;; Left unread until ESPN's spelling of it is confirmed against a live
     ;; league. nil is already the legal answer for a host that says nothing,
     ;; and `waiver/claims-left` degrades honestly on it — while a wrong week
     ;; mis-sizes every bid on the board and says nothing.
     :playoff-week-start nil
     :name             (get-in raw [:settings :name])
     :season           (str (:seasonId raw))}))

(defmethod league-sync/find-user :espn
  [_ {:keys [credentials]}]
  ;; No network: on ESPN the credential is the identity. Deliberate — see the
  ;; ns docstring. `:display-name` is nil rather than the SWID, which is half
  ;; the credential pair and must never reach the screen.
  {:user-id      (import-espn/normalize-swid (:swid credentials))
   :display-name nil
   :avatar       nil})

(def ^:private fan-base "https://fan.api.espn.com/apis/v2/fans/")

(defn league-entries
  "Pure: a fan document -> the fantasy football leagues it names.

  Written to find nothing rather than to throw when ESPN moves a field: an
  empty list is reported as a discovery gap and the manager pastes a league id,
  while an exception here would take the whole connect down with it."
  [raw]
  (into []
        (comp (keep #(get-in % [:metaData :entry]))
              (filter #(= "FFL" (:abbrev %)))
              (mapcat (fn [{:keys [seasonId groups]}]
                        (for [{:keys [groupId groupName]} groups
                              :when groupId]
                          (cond-> {:league-id (str groupId)
                                   :name      (or (not-empty groupName) (str groupId))}
                            seasonId (assoc :season (str seasonId)))))))
        (:preferences raw)))

(defmethod league-sync/list-leagues :espn
  [_ {:keys [user-id credentials]}]
  ;; The one URL in the app with a credential in its path, so its failures name
  ;; no URL: an error message carrying this one would carry the SWID with it.
  (let [url (str fan-base (java.net.URLEncoder/encode (str user-id) "UTF-8")
                 "?context=fantasy&displayEvents=true&displayNow=true&recentLimit=0")
        {:keys [status body error]} @(http/get url {:headers (import-espn/cookie-header credentials)
                                                    :timeout 30000})]
    (cond
      error             (throw (ex-info "ESPN would not list this account's leagues"
                                        {:status 502}))
      (not= 200 status) (let [[s msg] (import-espn/status-error status)]
                          (throw (ex-info msg {:status s})))
      :else             (league-entries (json/read-value body mapper)))))
