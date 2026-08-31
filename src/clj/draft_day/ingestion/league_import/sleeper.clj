(ns draft-day.ingestion.league-import.sleeper
  "Sleeper provider for league import: fetch a league's settings and normalize
  its scoring/roster config without collapsing scoring to a preset guess."
  (:require [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draft-day.json :refer [mapper]]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.scoring :as scoring]))

(defn- league-url [league-id]
  (str "https://api.sleeper.app/v1/league/" league-id))

(defmethod league-import/fetch-raw-league :sleeper
  [_ league-id]
  (let [{:keys [status body error]} @(http/get (league-url league-id) {:timeout 30000})]
    (cond
      error             (throw (ex-info "Sleeper league fetch failed" {:status 502 :error error}))
      (not= 200 status) (throw (ex-info "Sleeper league non-200" {:status 502 :sleeper-status status}))
      :else
      (let [parsed (json/read-value body mapper)]
        ;; Sleeper returns HTTP 200 with a JSON null body for an unknown league id.
        (if (nil? parsed)
          (throw (ex-info "Sleeper league not found" {:status 404}))
          parsed)))))

(defn- roster-config
  "Port of the frontend's sleeper->config roster-slot counting: FLEX/WRRB_FLEX/
  REC_FLEX all count toward :flex, BN plus any unrecognized slot counts toward :bench."
  [positions]
  (let [cnt   (fn [p] (count (filter #(= % p) positions)))
        known #{"QB" "RB" "WR" "TE" "K" "DEF" "BN" "FLEX" "WRRB_FLEX" "REC_FLEX"}
        flex  (+ (cnt "FLEX") (cnt "WRRB_FLEX") (cnt "REC_FLEX"))
        bench (+ (cnt "BN") (count (remove known positions)))]
    {:qb (cnt "QB") :rb (cnt "RB") :wr (cnt "WR") :te (cnt "TE")
     :flex flex :k (cnt "K") :dst (cnt "DEF") :bench bench}))

(defn unsupported-scoring
  "The league's own scoring rules that this app cannot score, sorted.

  `select-keys` against `scoring/stat-keys` silently drops everything else, and a
  real league carries a lot of it: FG distance buckets (Sleeper never emits a
  bare `fgm`, so an imported league loses field goals outright), DST
  points-allowed and yards-allowed tiers, yardage and long-play bonuses, TE
  premium. One live league dropped 65 of its 85 rules. Reporting a bare success
  hands back a config that looks complete and scores differently from the league
  it came from, so the import says what it could not take."
  [scoring-settings]
  (->> (apply dissoc scoring-settings scoring/stat-keys)
       (keep (fn [[k v]] (when (and (number? v) (not (zero? v))) (name k))))
       sort
       vec))

(def waiver-types
  "Sleeper's `settings.waiver_type` -> what the waiver run actually is.

  Read by `league-sync`, not by this namespace's own `normalize-league`. The
  waiver board is driven by the sync alone, and returning these on the import as
  well only looked tidy: `events/:league-import-loaded` select-keys them away on
  arrival and `db/reconcile-config` would strip them regardless, so the second
  copy was two keys nobody read and one more place for the rule to drift.

  The mapping is small and the *unknown* case is the one that matters. Anything
  not listed reads as `:rolling`, i.e. not FAAB — and that direction is chosen,
  not incidental. Suppressing a bid in a league that turns out to use FAAB costs
  the manager a column; inventing a dollar figure for a league that bids nothing
  puts a confident number on a transaction that does not exist. The board says
  when there is no market rather than guessing at one, exactly as it does for a
  player neither vendor prices."
  {0 :rolling 1 :reverse-standings 2 :faab})

(defn waiver-settings
  "Pure: a raw Sleeper league -> `{:type :faab :budget 100}`.

  The budget is only meaningful under `:faab`, but it is carried either way so a
  consumer never has to ask two questions to find out it should not be asking.
  Sleeper omits `waiver_budget` on leagues that never enabled FAAB; 0 is the
  honest reading of an absent budget and keeps every downstream share rule from
  dividing by a number nobody set."
  [raw]
  (let [s (:settings raw)]
    {:type   (get waiver-types (:waiver_type s) :rolling)
     :budget (or (:waiver_budget s) 0)}))

(defn playoff-week-start
  "The first week of the league's fantasy playoffs, or nil when it says nothing.

  A waiver claim made once the playoffs are under way buys at most a game or
  two, so this is what bounds how many runs a budget still has to cover — see
  `rankings.waiver/claims-left`. Read through one function rather than twice
  from two namespaces, because the sync fetches the same league document."
  [raw]
  (get-in raw [:settings :playoff_week_start]))

(defmethod league-import/normalize-league :sleeper
  [_ raw]
  {:scoring             (select-keys (:scoring_settings raw) scoring/stat-keys)
   :unsupported-scoring (unsupported-scoring (:scoring_settings raw))
   :roster              (roster-config (:roster_positions raw))
   :num-teams           (:total_rosters raw)
   :name                (:name raw)
   :season              (:season raw)})
