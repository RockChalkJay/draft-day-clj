(ns draft-day.ingestion.sleeper-players
  "Who Sleeper currently lists as injured, with the body part and its note.

  The projections feed embeds a player object per entry and its
  `:injury_status` is old (a receiver listed Questionable on Sleeper's own
  player list read null there, with a `news_updated` from two seasons back), so
  the universe's `:sleeper/injury-status` is replaced at request time by this
  list, the only fresh one Sleeper keeps. It speaks the same designations, so
  `db/serious-injury-statuses` and `rankings.injury` read it unchanged.

  The list is a dump of every player Sleeper has, so only the ones with a
  designation are kept. That makes the answer complete rather than partial: a
  player absent from a successful fetch is healthy as of it, and `with-injuries`
  clears his status instead of leaving the old one. A fetch that fails serves
  the last cached answer and is not retried for `pipeline/failure-backoff-ms`; offline,
  and while `DRAFTDAY_AS_OF_WEEK` replays a past week, there is none and every
  player keeps what the projections feed said, since today's designation is
  news that week never had.

  Cached apart from the universe on `DRAFTDAY_INJURY_TTL_HOURS` (default 3).
  Designations move by the hour on a game day, but Sleeper asks that this
  endpoint be called rarely."
  (:require [clojure.string :as str]
            [draft-day.ingestion.nflverse-weekly :as nflverse-weekly]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-http :as sleeper-http]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]))

(def schema-version 1)

(def default-cache-path (str "data/injuries.v" schema-version ".transit"))

(def players-url "https://api.sleeper.app/v1/players/nfl")

(defn ttl-hours []
  (Double/parseDouble (or (System/getenv "DRAFTDAY_INJURY_TTL_HOURS") "3")))

(defn fetch-raw
  "Network: Sleeper's player list, `{sleeper-id player}`."
  []
  (let [{:keys [status body error]} (sleeper-http/get! players-url {:timeout 60000})]
    (cond
      error          (throw (ex-info "Sleeper players fetch failed" {:error error}))
      (= 200 status) (json/read-value body mapper)
      :else          (throw (ex-info "Sleeper players non-200" {:status status})))))

(defn normalize
  "`{sleeper-id {:status :body-part :notes :updated-at}}` for the players with a
  designation. A blank status is none, which Sleeper spells both ways."
  [raw]
  (into {}
        (keep (fn [[id {:keys [injury_status injury_body_part injury_notes news_updated]}]]
                (when-let [status (some-> injury_status str str/trim not-empty)]
                  [(name id) (cond-> {:status status}
                               (not (str/blank? injury_body_part)) (assoc :body-part injury_body_part)
                               (not (str/blank? injury_notes))     (assoc :notes injury_notes)
                               (number? news_updated)              (assoc :updated-at news_updated))])))
        raw))

(defn live
  "The envelope for a fresh list. An answer with no player in it throws, since
  caching nobody would read as everybody healthy."
  []
  (let [raw (fetch-raw)]
    (when-not (and (map? raw) (some (comp :player_id val) raw))
      (throw (ex-info "Sleeper players list empty" {})))
    {:fetched-at (pipeline/now-iso)
     :injuries   (normalize raw)}))

(defn load-injuries
  "`{:fetched-at :injuries {sleeper-id {...}}}`, fresh from the cache, else live,
  else whatever is cached however old; nil offline, replaying a past week, or
  with nothing to serve."
  ([] (load-injuries {}))
  ([{:keys [path] :or {path default-cache-path}}]
   (when-not (or (pipeline/offline?) (nflverse-weekly/as-of-week))
     (pipeline/load-ttl-cache {:path           path
                               :ttl-hours      (ttl-hours)
                               :schema-version schema-version
                               :fetch          live
                               :label          "injury list"}))))

(defn assoc-injuries
  "Set each player's designation from `injuries`, by Sleeper id. One the list
  does not name is healthy, so his projections-feed status is cleared. nil
  `injuries` leaves every player as he was."
  [players injuries]
  (if (nil? injuries)
    players
    (mapv (fn [p]
            (let [{:keys [status body-part notes updated-at]}
                  (get injuries (or (get-in p [:ids :sleeper]) (:player-id p)))]
              (-> (dissoc p :sleeper/injury-body-part :sleeper/injury-notes :sleeper/injury-updated)
                  (assoc :sleeper/injury-status status)
                  (cond-> body-part  (assoc :sleeper/injury-body-part body-part)
                          notes      (assoc :sleeper/injury-notes notes)
                          updated-at (assoc :sleeper/injury-updated updated-at)))))
          players)))

(defonce ^:private joined-memo (atom nil))

(defn with-injuries
  "The universe with the current designations joined on. Only a live or cached
  universe is joined, as `pipeline/with-realized` does, and the join is
  memoized on both inputs since every board request asks for it."
  [{:keys [source] :as env}]
  (if-not (#{"live" "cache"} source)
    env
    (let [injuries (:injuries (load-injuries))
          memo     @joined-memo]
      (if (and (identical? (:base memo) env) (= injuries (:injuries memo)))
        (:joined memo)
        (let [joined (update env :players assoc-injuries injuries)]
          (reset! joined-memo {:base env :injuries injuries :joined joined})
          joined)))))
