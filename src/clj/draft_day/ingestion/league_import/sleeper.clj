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

(defmethod league-import/normalize-league :sleeper
  [_ raw]
  {:scoring   (select-keys (:scoring_settings raw) scoring/stat-keys)
   :roster    (roster-config (:roster_positions raw))
   :num-teams (:total_rosters raw)
   :name      (:name raw)
   :season    (:season raw)})
