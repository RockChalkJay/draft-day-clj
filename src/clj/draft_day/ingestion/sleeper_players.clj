(ns draft-day.ingestion.sleeper-players
  "Who Sleeper currently lists as injured, with the body part and its note, and
  who every player is.

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

  The same dump is also kept as a directory, `{sleeper-id {:name :position
  :team}}` for the skill positions. A roster names players by Sleeper id, and a
  player the projections feed carries no season line for is not in the
  universe, so the directory is the only place his name can come from.

  Cached apart from the universe on `DRAFTDAY_INJURY_TTL_HOURS` (default 3).
  Designations move by the hour on a game day, but Sleeper asks that this
  endpoint be called rarely."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [draft-day.ingestion.nflverse-weekly :as nflverse-weekly]
            [draft-day.ingestion.paths :as paths]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.sleeper-http :as sleeper-http]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]))

(def schema-version 2)

(def default-cache-path (str paths/cache-root "/injuries.v" schema-version ".transit"))

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

(defn directory
  "`{sleeper-id {:name :position :team}}` for every player scored at QB, RB, WR,
  TE or K, by `sleeper/fantasy-position`, so a two-way player is filed where he
  scores. Team defenses are left out: they are keyed by abbreviation in both
  id spaces and the universe already names them. `:team` is absent for a
  player on no roster."
  [raw]
  (into {}
        (keep (fn [[id {:keys [first_name last_name team] :as player}]]
                (let [pos (some-> (sleeper/fantasy-position player) sleeper/canon-pos)]
                  (when (and pos (not= "DST" pos))
                    [(name id) (cond-> {:name     (str/trim (str first_name " " last_name))
                                        :position pos}
                                 (not (str/blank? team)) (assoc :team team))]))))
        raw))

(defn live
  "The envelope for a fresh list. An answer with no player in it throws, since
  caching nobody would read as everybody healthy."
  []
  (let [raw (fetch-raw)]
    (when-not (and (map? raw) (some (comp :player_id val) raw))
      (throw (ex-info "Sleeper players list empty" {})))
    (let [injuries (normalize raw)
          names    (directory raw)]
      (log/info "Sleeper player list:" (count names) "players named,"
                (count injuries) "with a designation")
      {:fetched-at (pipeline/now-iso)
       :injuries   injuries
       :directory  names})))

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

(defn load-directory
  "`{sleeper-id {:name :position :team}}` off the same cache as the designations,
  or nil where there is none to read (offline, replaying a week, nothing cached
  and Sleeper silent). A caller treats nil as no names, never as no players."
  []
  (:directory (load-injuries)))

(defn assoc-injuries
  "Set each player's designation from `injuries`, by Sleeper id. One the list
  does not name is healthy, so his projections-feed status is cleared. nil
  `injuries` leaves every player as he was."
  [players injuries]
  (if (nil? injuries)
    players
    (mapv (fn [p]
            (let [{:keys [status body-part notes updated-at]}
                  (get injuries (pipeline/sleeper-id p))]
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

(defonce ^:private unseasoned-memo (atom nil))

(defonce ^:private unseasoned-skip (atom nil))

(defn- skip!
  "Logs why stubs were skipped, once per reason: the universe is asked for on
  every request, and a missing line would otherwise warn on each."
  [reason]
  (when (not= reason (first (reset-vals! unseasoned-skip reason)))
    (log/warn "unseasoned players skipped: no" reason)))

(defn- unseasoned-week
  "`week-fn`'s week when stubs apply to it, else nil: in season, after week one."
  [{:keys [source]} week-fn]
  (when (and (#{"live" "cache"} source)
             (not (pipeline/offline?))
             (nil? (nflverse-weekly/as-of-week)))
    (let [week (week-fn)]
      (when (and (number? week) (> week 1)) week))))

(defn with-unseasoned
  "The universe with the players Sleeper projects for the week but gave no season
  line added as `:unseasoned?` stubs (`pipeline/unseasoned-players`).

  `week-fn` names the week being played and is only asked once the universe
  qualifies: live or cached, in season past week one (the August board is
  already drawn), and not offline or replaying a week, where today's line would
  leak the future. Without a weekly line or a directory the universe is
  returned as it was, and the reason is logged. The join is memoized on its
  inputs, so an unchanged universe stays the same object for
  `pipeline/with-realized`'s own memo."
  [env season week-fn]
  (if-let [week (unseasoned-week env week-fn)]
    (let [lines     (:lines (pipeline/load-weekly season week))
          directory (load-directory)
          memo      @unseasoned-memo]
      (cond
        (or (empty? lines) (empty? directory))
        (do (skip! (if (empty? lines) "weekly line" "player directory"))
            env)

        (and (identical? (:base memo) env) (= week (:week memo))
             (= lines (:lines memo)) (= directory (:directory memo)))
        (do (reset! unseasoned-skip nil)
            (:joined memo))

        :else
        (let [stubs  (pipeline/unseasoned-players (:players env) lines directory)
              joined (if (seq stubs) (update env :players into stubs) env)]
          (reset! unseasoned-skip nil)
          (when (seq stubs)
            (log/info "week" week "has a line for" (count stubs)
                      "players with no season line, added to the board:"
                      (str/join ", " (map :player-name stubs))))
          (reset! unseasoned-memo {:base env :week week :lines lines
                                   :directory directory :joined joined})
          joined)))
    env))
