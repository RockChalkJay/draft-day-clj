(ns draft-day.ingestion.espn-news
  "One player's latest news, from ESPN's public fantasy news feed. Keyless, a
  few KB per player, fetched when a card opens rather than with the universe.

  WHY THIS IS NOT IN `ingestion.espn` OR `espn-schedule`. Different endpoints
  again, and a different cadence from both: a designation moves on a Wednesday
  practice report, so the TTL is minutes where the universe's is a day. See
  `espn-schedule` for why a shared namespace would make a thin column
  undiagnosable.

  NO INJURY STATUS. The designation is `ingestion.sleeper-players`' job, so the
  board and the card read one source. A failed feed puts its error in `:errors`
  rather than showing an empty list as if he had no news.

  ONLY ROTOWIRE ITEMS. The same feed mixes in ESPN's own long-form stories,
  which are not about the player's situation and run to pages. Rotowire writes
  one short note per event.

  TEXT, NOT MARKUP. Items are stripped of tags here; the browser never renders
  them as HTML."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]
            [org.httpkit.client :as http]))

(def max-items 10)

(defn feed-url [espn-id]
  (str "https://site.api.espn.com/apis/fantasy/v2/games/ffl/news/players?playerId="
       espn-id "&limit=50"))

(defn fetch-json
  "Network. Throws on every failure shape, so `player-news` can report it."
  [url]
  (let [{:keys [status body error]} @(http/get url {:timeout 15000})]
    (cond
      error (throw (ex-info "espn news fetch failed" {:url url} error))
      (not= 200 status) (throw (ex-info "espn news non-200" {:url url :status status}))
      :else (json/read-value body mapper))))

(defn fetch-feed [espn-id] (fetch-json (feed-url espn-id)))

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
  "The feed fetched now, best-effort."
  [espn-id]
  (let [[feed err] (attempt :news (fn [] (fetch-feed espn-id)))]
    {:news       (if feed (parse-feed feed) [])
     :fetched-at (str (java.time.Instant/now))
     :errors     (if err [err] [])}))

(defn player-news
  "`{:news :fetched-at :errors}` for one ESPN id. Empty offline."
  [espn-id]
  (if (pipeline/offline?)
    {:news [] :fetched-at nil :errors []}
    (let [now (System/currentTimeMillis)
          hit (get @cache espn-id)]
      (if (and hit (< (- now (:at hit)) (ttl-ms)))
        (:reply hit)
        (let [reply (live espn-id)]
          (when (empty? (:errors reply))
            (swap! cache assoc espn-id {:at now :reply reply}))
          reply)))))
