(ns draft-day.benchmark.fetch
  "HTTP + disk cache for benchmark sources.

  Every source here is a static, per-season archive, so once a season is fetched
  it never changes — the cache has no TTL, unlike the live ingestion cache. Runs
  after the first are offline and fast, which matters because a full sweep touches
  ~25 seasons across four sources.

  Uses the JDK client rather than http-kit for the same reason `ingestion/espn.clj`
  does (multi-MB bodies), plus redirect following: nflverse release URLs 302 to
  objectstorage, and the JDK client does not follow redirects by default."
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.ingestion.pipeline :as pipeline])
  (:import [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpResponse$BodyHandlers]
           [java.net URI]))

(def cache-dir "data/benchmark_cache")

(defn cache-path
  "Path for a cache entry. Include a schema token (\"v2\", ...) in `parts` for any
  source whose cached value is a DERIVED map rather than raw parsed rows, and
  bump it whenever that shape changes.

  Learned the hard way: adding two diagnostic fields to the Sleeper entry left
  old cache files in the previous shape, and the report happily rendered the
  missing fields as 0 for every season — reporting 'Sleeper returned 0 entries'
  for seasons that plainly have data. A stale cache that yields wrong numbers is
  worse than no cache, because it looks like a finding."
  [& parts]
  (str cache-dir "/" (str/join "-" (map str parts)) ".transit"))

(def user-agent
  "Identify ourselves. The JDK client's default `Java-http-client/...` gets
  throttled and connection-reset by archive.org, which surfaced as sporadic,
  season-dependent parse failures that looked like missing data rather than a
  transport problem."
  "draft-day-benchmark/0.1 (fantasy football research; +https://github.com/RockChalkJay/draft-day-clj)")

(defn decode-body
  "Response bytes -> string, decompressing when the server said gzip.

  The JDK client does NOT transparently decompress, unlike curl --compressed.
  archive.org gzips by default, so without this the bytes reach Jsoup as
  mojibake, every table parse silently returns zero rows, and the harness reports
  it as 'this season has no projections' — a transport bug wearing a data bug's
  clothing."
  [^bytes body gzip?]
  (if gzip?
    (with-open [in (java.util.zip.GZIPInputStream. (io/input-stream body))]
      (slurp in :encoding "UTF-8"))
    (String. body "UTF-8")))

(defn http-get-once
  "One attempt. 200 -> body string; 404/410 -> nil (definitively absent);
  anything else throws."
  [url]
  (let [client (-> (HttpClient/newBuilder)
                   (.followRedirects HttpClient$Redirect/NORMAL)
                   (.build))
        req    (-> (HttpRequest/newBuilder (URI/create url))
                   (.header "User-Agent" user-agent)
                   (.header "Accept" "*/*")
                   (.header "Accept-Encoding" "gzip")
                   (.GET)
                   (.build))
        resp   (.send client req (HttpResponse$BodyHandlers/ofByteArray))
        status (.statusCode resp)
        gzip?  (= "gzip" (-> (.headers resp) (.firstValue "content-encoding")
                             (.orElse "") str/lower-case))]
    (cond
      (= 200 status)      (decode-body (.body resp) gzip?)
      (#{404 410} status) nil
      :else (throw (ex-info "unexpected HTTP status" {:url url :status status})))))

(defn http-get-string
  "GET url -> body string, or nil when the server definitively says there is
  nothing there (404/410). Follows redirects and retries transient failures with
  linear backoff.

  The nil-vs-throw split is what lets `cached` safely remember an empty answer:
  nil means 'this does not exist and never will', while a reset connection or a
  5xx means 'ask again later' and must not be frozen into the cache. Retries
  matter because archive.org resets connections under load, and a harness that
  gives up on the first reset silently reports missing history."
  ([url] (http-get-string url {}))
  ([url {:keys [retries backoff-ms] :or {retries 5 backoff-ms 3000}}]
   (loop [attempt 1]
     (let [result (try
                    {:ok (http-get-once url)}
                    (catch Exception e
                      (if (< attempt retries)
                        {:retry e}
                        (throw e))))]
       (if (contains? result :ok)
         (:ok result)
         (do (Thread/sleep (* attempt backoff-ms))
             (recur (inc attempt))))))))

(defn parse-csv
  "CSV text -> vector of maps keyed by header string. Values stay strings;
  coercion is each source's business, since 'NA' and '' both mean missing in
  nflverse exports and only the caller knows which columns are numeric."
  [text]
  (let [[header & rows] (csv/read-csv text)]
    (mapv #(zipmap header %) rows)))

(defn num-or-nil
  "Parse a CSV cell as a double. nflverse writes both \"\" and \"NA\" for missing."
  [s]
  (when (and s (not= "" s) (not= "NA" s))
    (try (Double/parseDouble s) (catch Exception _ nil))))

(defn num0 [s] (or (num-or-nil s) 0.0))

(def ^:dynamic *refresh*
  "When true, bypass the disk cache and re-fetch, overwriting it. Bound by the
  report's --refresh flag so a suspect cache entry can be re-verified against the
  source without hand-deleting files."
  false)

(defn cached
  "Return the transit-cached value at `path`, else compute `(f)`, cache it, and
  return it.

  Empty results ARE cached. Historical seasons never change, so 'Fantasy Football
  Calculator publishes no 2009 ADP' is a permanent fact worth remembering; not
  caching it means re-asking on every single run forever. Only a thrown exception
  (network failure, unexpected status — see `http-get-string`) skips the cache,
  so transient trouble is retried rather than frozen in."
  [path f]
  (if-let [hit (and (not *refresh*) (pipeline/read-transit path))]
    hit
    (let [v (f)]
      (pipeline/write-transit! path v)
      v)))

(defn cache-exists? [path] (.exists (io/file path)))
