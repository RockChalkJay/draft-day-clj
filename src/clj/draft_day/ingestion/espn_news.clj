(ns draft-day.ingestion.espn-news
  "One player's news and injury designation, from ESPN's public endpoints.
  Keyless, a few KB per player, fetched when a card opens rather than with the
  universe.

  WHY THIS IS NOT IN `ingestion.espn` OR `espn-schedule`. Different endpoints
  again, and a different cadence from both: a designation moves on a Wednesday
  practice report, so the TTL is minutes where the universe's is a day. See
  `espn-schedule` for why a shared namespace would make a thin column
  undiagnosable.

  TWO URLS, TWO HALVES. The fantasy news feed carries the Rotowire items; the
  athlete document carries the injury designation. Each is best-effort on its
  own, so a failed feed still returns the status and the other way round, and
  `:errors` names which half is missing rather than showing an empty list as if
  he had no news.

  ONLY ROTOWIRE ITEMS. The same feed mixes in ESPN's own long-form stories,
  which are not about the player's situation and run to pages. Rotowire writes
  one short note per event.

  TEXT, NOT MARKUP. Items are stripped of tags here; the browser never renders
  them as HTML."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]
            [org.httpkit.client :as http]))

(def max-items 10)

(defn feed-url [espn-id]
  (str "https://site.api.espn.com/apis/fantasy/v2/games/ffl/news/players?playerId="
       espn-id "&limit=50"))

(defn athlete-url [espn-id]
  (str "https://site.web.api.espn.com/apis/common/v3/sports/football/nfl/athletes/"
       espn-id))

(defn fetch-json
  "Network. Throws on every failure shape, so `player-news` can say which half
  went missing."
  [url]
  (let [{:keys [status body error]} @(http/get url {:timeout 15000})]
    (cond
      error (throw (ex-info "espn news fetch failed" {:url url} error))
      (not= 200 status) (throw (ex-info "espn news non-200" {:url url :status status}))
      :else (json/read-value body mapper))))

(defn fetch-feed [espn-id] (fetch-json (feed-url espn-id)))

(defn fetch-athlete [espn-id] (fetch-json (athlete-url espn-id)))

(defn plain-text
  "`s` with any tags removed and whitespace collapsed; nil for blank."
  [s]
  (when s
    (let [t (-> s (str/replace #"<[^>]*>" " ") (str/replace #"\s+" " ") str/trim)]
      (when-not (str/blank? t) t))))

(defn parse-feed
  "Decoded news feed -> up to `max-items` `{:published :headline :story}`,
  newest first, Rotowire items only."
  [payload]
  (->> (:feed payload)
       (filter #(= "Rotowire" (:type %)))
       (keep (fn [{:keys [published headline story description]}]
               (when-let [h (plain-text (or headline description))]
                 {:published published
                  :headline  h
                  :story     (plain-text story)})))
       (sort-by :published #(compare %2 %1))
       (take max-items)
       vec))

(defn parse-status
  "Decoded athlete document -> `{:status :abbr :short :long :date}` off his
  first injury entry, or nil when he has none. An \"Active\" entry is a note on
  a player with no designation, so it is nil too."
  [payload]
  (when-let [{:keys [status type shortComment longComment date]}
             (first (get-in payload [:athlete :injuries]))]
    (when-not (= "Active" status)
      {:status status
       :abbr   (:abbreviation type)
       :short  (plain-text shortComment)
       :long   (plain-text longComment)
       :date   date})))

(defn ttl-ms []
  (long (* 60000 (Double/parseDouble
                  (or (System/getenv "DRAFTDAY_NEWS_TTL_MINUTES") "15")))))

;; Held per ESPN id until the TTL passes. Only a reply with no errors goes in:
;; caching a half-failed one would pin the failure for the whole TTL.
(defonce cache (atom {}))

(defn reset-cache! [] (reset! cache {}))

(defn attempt
  "Run `f`; `[value nil]` on success, `[nil error-map]` on any failure."
  [source f]
  (try [(f) nil]
       (catch Exception e
         (log/warn e "espn" (name source) "fetch failed:" (ex-message e))
         [nil {:source source :message (ex-message e)}])))

(defn live
  "Both halves fetched now, concurrently, each best-effort."
  [espn-id]
  (let [{[feed feed-err] :news [athlete athlete-err] :status}
        (parallel/all {:news   #(attempt :news (fn [] (fetch-feed espn-id)))
                       :status #(attempt :status (fn [] (fetch-athlete espn-id)))})]
    {:status     (some-> athlete parse-status)
     :news       (if feed (parse-feed feed) [])
     :fetched-at (str (java.time.Instant/now))
     :errors     (vec (keep identity [feed-err athlete-err]))}))

(defn player-news
  "`{:status :news :fetched-at :errors}` for one ESPN id. Empty offline."
  [espn-id]
  (if (pipeline/offline?)
    {:status nil :news [] :fetched-at nil :errors []}
    (let [now (System/currentTimeMillis)
          hit (get @cache espn-id)]
      (if (and hit (< (- now (:at hit)) (ttl-ms)))
        (:reply hit)
        (let [reply (live espn-id)]
          (when (empty? (:errors reply))
            (swap! cache assoc espn-id {:at now :reply reply}))
          reply)))))
