(ns draftday.ingestion.espn
  "Enrichment: ESPN's live auction values (crowd market price), joined by name.
  ESPN's read mirror ignores the filter's limit/sort and returns ~11k players
  (~37MB), so we use the JDK's java.net.http client (http-kit's client chokes on
  a body that large) and keep only players with a real auction value. Best-effort."
  (:require [jsonista.core :as json]
            [draftday.ingestion.match :as match])
  (:import [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.net URI]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))

(def ^:private position-map {1 "QB" 2 "RB" 3 "WR" 4 "TE" 5 "K" 16 "DST"})

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

(defn enrichment
  "Pure: ESPN response -> {match-key {:espn/auction-value :espn/adp}}, keeping only
  players with a real auction value."
  [data]
  (let [arr (if (map? data) (:players data) data)]
    (into {}
          (keep (fn [p]
                  (let [pos (position-map (:defaultPositionId p))
                        av  (auction-value p)]
                    (when (and pos av)
                      [(match/key-for (:fullName p) pos)
                       {:espn/auction-value av
                        :espn/adp (get-in p [:ownership :averageDraftPosition])}]))))
          arr)))

(defn- http-get-string [url headers]
  (let [builder (HttpRequest/newBuilder (URI/create url))]
    (doseq [[k v] headers] (.header builder k v))
    (let [resp (.send (HttpClient/newHttpClient)
                      (.build (.GET builder))
                      (HttpResponse$BodyHandlers/ofString))]
      (when (= 200 (.statusCode resp)) (.body resp)))))

(defn fetch
  "Network: fetch + parse ESPN auction values for a season -> by-key enrichment
  map. nil on failure."
  [year]
  (when-let [body (http-get-string (api-url year) {"x-fantasy-filter" filter-header})]
    (enrichment (json/read-value body mapper))))
