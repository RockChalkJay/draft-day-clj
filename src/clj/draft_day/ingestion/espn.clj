(ns draft-day.ingestion.espn
  "Enrichment: ESPN's live auction values (crowd market price) and its published
  season projections, joined by name. ESPN's read mirror ignores the filter's
  limit/sort and returns ~11k players (~37MB), so we use the JDK's
  java.net.http client (http-kit's client chokes on a body that large).
  Best-effort.

  The projections ride in the *same* response as the auction values — a player's
  `stats` array carries one entry per (season, source, split), and the season
  projection is the one at `statSourceId 1, statSplitTypeId 0`. Reading it costs
  no extra request, which is the whole reason projected targets are affordable:
  Sleeper, the app's projection backbone, publishes no target column at all."
  (:require [jsonista.core :as json]
            [draft-day.ingestion.match :as match]
            [draft-day.json :refer [mapper]])
  (:import [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.net URI]))

(def ^:private position-map {1 "QB" 2 "RB" 3 "WR" 4 "TE" 5 "K" 16 "DST"})

;; ESPN keys its stat values by numeric id. They arrive as *keywords* (:58, :53)
;; rather than strings, because `draft-day.json/mapper` keywordizes every key on
;; decode — looking them up as strings silently finds nothing and reports the
;; whole column as absent. Only the usage columns the board shows are decoded
;; here; the rest of the ~150 ids stay unread.
(def ^:private stat-targets :58)
(def ^:private stat-receptions :53)

;; Any x-fantasy-filter makes ESPN populate ownership + draftRanks for all players.
(def ^:private filter-header "{\"players\":{\"filterActive\":{\"value\":true}}}")

(defn- api-url [year]
  (str "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/" year
       "/players?scoringPeriodId=0&view=kona_player_info"))

(defn- auction-value
  "Prefer the live crowd average; fall back to ESPN's published PPR auction value."
  [p]
  (let [own (get-in p [:ownership :auctionValueAverage])
        ppr (get-in p [:draftRanksByRankType :PPR :auctionValue])]
    (cond
      (and (number? own) (pos? own)) (double own)
      (and (number? ppr) (pos? ppr)) (double ppr))))

(defn projected-usage
  "Pure: one ESPN player -> its projected usage for `season`, or nil.

  Picks the `statSourceId 1` (projection, not actual) / `statSplitTypeId 0`
  (whole season, not a single week) entry. The current season's *actual* block
  is present too and is all zeros until games are played, so matching on the
  source id is what keeps a projection from being reported as a flat zero."
  [season p]
  (when-let [st (some (fn [s]
                        (when (and (= season (:seasonId s))
                                   (= 1 (:statSourceId s))
                                   (= 0 (:statSplitTypeId s)))
                          (:stats s)))
                      (:stats p))]
    (let [tgt (get st stat-targets)
          rec (get st stat-receptions)]
      (not-empty
       (cond-> {}
         (number? tgt) (assoc :espn/proj-targets (double tgt))
         (number? rec) (assoc :espn/proj-receptions (double rec)))))))

(defn enrichment
  "Pure: ESPN response -> {match-key {:espn/auction-value :espn/adp
  :espn/proj-targets :espn/proj-receptions}}.

  A player is kept if ESPN priced him *or* projected him. Gating on the auction
  value alone — which is what this did when it only carried prices — would have
  thrown away the projections for everyone ESPN ranks but does not price."
  [season data]
  (let [arr (if (map? data) (:players data) data)]
    (into {}
          (keep (fn [p]
                  (let [pos  (position-map (:defaultPositionId p))
                        av   (auction-value p)
                        proj (projected-usage season p)]
                    (when (and pos (or av proj))
                      [(match/key-for (:fullName p) pos)
                       (cond-> (or proj {})
                         av (assoc :espn/auction-value av
                                   :espn/adp (get-in p [:ownership :averageDraftPosition])))]))))
          arr)))

(defn- http-get-string [url headers]
  (let [builder (HttpRequest/newBuilder (URI/create url))]
    (doseq [[k v] headers] (.header builder k v))
    (let [resp (.send (HttpClient/newHttpClient)
                      (.build (.GET builder))
                      (HttpResponse$BodyHandlers/ofString))]
      (when (= 200 (.statusCode resp)) (.body resp)))))

(defn fetch
  "Network: fetch + parse ESPN auction values and season projections -> by-key
  enrichment map. nil on failure."
  [year]
  (when-let [body (http-get-string (api-url year) {"x-fantasy-filter" filter-header})]
    (enrichment year (json/read-value body mapper))))
