(ns draft-day.benchmark.sources.wayback
  "Internet Archive access: enumerate captures of a URL and fetch one.

  This is the only route to vintage projections older than Sleeper's 2021 floor.
  It carries a hazard the other sources do not: an archived page is a snapshot of
  a LIVE page, and live pages change. FantasyPros recomputes its draft-projection
  consensus during the season — comparing its 2015 WR page at Sept 8 against Nov
  27 leaves only 40 of ~170 rows identical, with players dropping out and the
  order shifting. So a capture is trustworthy only if it predates that season's
  Week 1 kickoff; callers must enforce that, and `fantasypros-archive` does.

  Fetches go through the `id_` variant of the Wayback URL, which returns the
  original bytes without the archive's injected toolbar and scripts."
  (:require [clojure.string :as str]
            [draft-day.benchmark.fetch :as fetch]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]))

(def ^:private cdx-endpoint "http://web.archive.org/cdx/search/cdx")

(def polite-delay-ms
  "Pause between archive.org requests. The archive is a free public service and
  rate-limits accordingly; a full 5-season backfill is only ~20 fetches, so
  being slow costs nothing and being rude risks the whole source."
  1500)

(defn cdx-url [target from to]
  (str cdx-endpoint "?url=" target
       "&matchType=prefix&output=json&filter=statuscode:200"
       "&from=" from "&to=" to "&limit=50000"))

(defn parse-cdx
  "Pure: CDX JSON (header row + rows) -> [{:timestamp :original}]."
  [payload]
  (let [[header & rows] payload]
    (when (seq header)
      (let [ti (.indexOf ^java.util.List header "timestamp")
            ui (.indexOf ^java.util.List header "original")]
        (when (and (nat-int? ti) (nat-int? ui) (pos? (count rows)))
          (mapv (fn [r] {:timestamp (nth r ti) :original (nth r ui)}) rows))))))

(defn captures
  "All 200-status captures under `target` (a URL prefix) between two years.
  Disk-cached: the archive's index for a closed year never changes."
  [target from to]
  (fetch/cached
   (fetch/cache-path "wayback" "cdx" (str/replace target #"[^a-zA-Z0-9]" "_") from to)
   (fn []
     (Thread/sleep polite-delay-ms)
     (if-let [body (fetch/http-get-string (cdx-url target from to))]
       (or (parse-cdx (json/read-value body mapper)) [])
       []))))

(defn snapshot-url
  "The `id_` form returns the archived page as originally served."
  [timestamp original]
  (str "https://web.archive.org/web/" timestamp "id_/" original))

(defn fetch-snapshot
  "Archived page HTML, disk-cached by timestamp. nil if the archive 404s."
  [timestamp original]
  (fetch/cached
   (fetch/cache-path "wayback" "page" timestamp
                     (str/replace original #"[^a-zA-Z0-9]" "_"))
   (fn []
     (Thread/sleep polite-delay-ms)
     {:html (fetch/http-get-string (snapshot-url timestamp original))})))
