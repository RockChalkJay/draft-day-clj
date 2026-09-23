(ns draft-day.ingestion.transactions
  "Provider-agnostic bid history: every FAAB auction a league has run, losing
  bids included, rebuilt from the host's transaction log and cached on disk.

  A fourth multimethod pair beside `league-import`, `league-sync` and
  `matchups`, working a season at a time. It is fetched on the sync's cadence,
  since a waiver run changes both, but kept apart because it reads a different
  document and fails differently: a history that will not load costs the board
  its league-specific prices, and folded into the sync it would cost the rosters
  too.

  A normalized season is the whole contract a provider owes:

    {:season :league-id :budget :final? :previous {:league-id :season}
     :non-competing n
     :auctions [{:week :at :player-id
                 :bids [{:roster-id :owner-id :amount :won?}]}]}

  Only bids that competed are kept — the winner and the bids it beat. A claim
  that failed for want of a roster spot never competed, and counting it would
  teach the model a price nobody was outbid at; `:non-competing` counts the
  managers left out. Ids stay in the provider's space, as a sync's do, and
  `:owner-id` rides on every bid because a roster id names a seat for one season
  while a manager's habits outlast it. `:previous` is the season this one
  continues, which lets the history reach back a year without the dispatcher
  knowing how a host links its seasons. A host may only be able to guess the
  predecessor's season, so once it has been fetched the link is rewritten to the
  season its own document states — the one it is cached under.

  The seasons live on the server, one file per league-season under
  `cache-dir`, and never cross the wire. A sync refreshes an unfinished season
  and reads a finished one straight from the cache, the current one included,
  since it cannot change. The browser is handed a `summary`, and the waiver board reads `cached-history` by
  league id — no second fetch, and no credentials on its request. A refresh that
  fails leaves the last good file in place, so the board keeps pricing from it
  and the sync reports the failure beside a summary of what is still cached.

  `bid-history` keeps `league-sync/sync-league`'s promises for its reason: access
  is checked before any fetch, the season is defaulted, ids are strings — the
  league's, its predecessor's and every bid's — and a concurrent fetch's
  `ExecutionException` is unwrapped before its status is read."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.season :as season]
            [draft-day.providers :as providers])
  (:import [java.nio.file CopyOption Files StandardCopyOption]))

(defmulti fetch-raw-season
  "Network: one league-season's raw transaction payload, from a request map of
  `{:league-id :season :credentials}`. Throws ex-info with `:status` on
  failure, as `league-sync/fetch-raw-rosters` does."
  (fn [provider _req] provider))

(defmethod fetch-raw-season :default
  [provider _req]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-season
  "Pure: a provider's raw season -> the season shape in the ns docstring."
  (fn [provider _raw] provider))

(defn string-ids
  "One auction with its player id and every owner id as strings. A missing owner
  stays nil — `(str nil)` is an owner called \"\"."
  [auction]
  (-> auction
      (update :player-id str)
      (update :bids (fn [bids] (mapv #(update % :owner-id (fn [o] (some-> o str))) bids)))))

(defn normalized
  "A provider's normalized season with the ids this namespace promises, the
  season among them: it is half of every cache path."
  [season]
  (-> season
      (update :league-id #(some-> % str))
      (update :season #(some-> % str))
      (update :previous #(some-> % (update :league-id str) (update :season str)))
      (update :auctions (fn [as] (mapv string-ids as)))))

(def schema-version
  "Version of a cached season's shape; the directory carries it, so a bump
  orphans the old files rather than reading them into missing keys."
  2)

(def cache-dir (str "data/bid_history.v" schema-version))

(defn cache-path
  "Where one league-season is cached, or nil for an id or season that could not
  have come from a real league. The waiver board reads this with ids off a
  request body, so both are held to the patterns a fetch is."
  [provider league-id season]
  (when (and (nil? (providers/league-id-error provider league-id))
             (re-matches #"\d{4}" (str season)))
    (str cache-dir "/" (name provider) "-" league-id "-" season ".transit")))

(defn read-season
  "The cached league-season, or nil when there is none or it will not read."
  [provider league-id season]
  (when-let [path (cache-path provider league-id season)]
    (try (pipeline/read-transit path)
         (catch Exception e
           (log/warn e "unreadable bid history cache" path)
           nil))))

(defn write-season!
  "Cache one normalized season. Written beside its final path and moved into
  place, so two syncs of one league cannot leave a half-written file for the
  waiver board to read; a write that fails takes its temporary file with it."
  [provider {:keys [league-id season] :as s}]
  (when-let [path (cache-path provider league-id season)]
    (let [tmp (io/file (str path ".tmp-" (random-uuid)))]
      (try
        (pipeline/write-transit! tmp s)
        (Files/move (.toPath tmp) (.toPath (io/file path))
                    (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                            StandardCopyOption/REPLACE_EXISTING]))
        (finally (io/delete-file tmp true))))))

(defn fetch-season
  "Network: fetch and normalize one league-season. Not cached here: the current
  season is written only once its link to the previous one is settled."
  [provider req]
  (-> (normalize-season provider (fetch-raw-season provider req))
      normalized
      (assoc :fetched-at (pipeline/now-iso))))

(defn previous-season!
  "The season `link` names: from the cache when it is finished, fetched and cached
  otherwise, and nil when there is none or the host no longer knows it. Any
  other failure throws, since a history silently missing a season reads exactly
  like a league that did not bid.

  What it throws is always an Exception: a cause that is an Error stays wrapped,
  so `bid-history` can keep its promise not to throw."
  [provider link credentials]
  (when-let [{:keys [league-id season]} link]
    (let [cached (read-season provider league-id season)]
      (if (:final? cached)
        cached
        (try
          (let [s (fetch-season provider {:league-id league-id :season season
                                          :credentials credentials})]
            (write-season! provider s)
            s)
          (catch Exception e
            (let [cause (league-sync/unwrap-execution e)]
              (when-not (= 404 (:status (ex-data cause)))
                (throw (if (instance? Exception cause) cause e))))))))))

(defn previous-link
  "Which season `fresh` continues. The last cached copy's link wins while it
  names the same league, because it has been rewritten to the season that
  league's own document states and the fresh one is the host's guess again."
  [fresh cached]
  (let [guess (:previous fresh)]
    (if (and guess (= (:league-id guess) (get-in cached [:previous :league-id])))
      (:previous cached)
      guess)))

(defn linked
  "`current` pointing at the season `previous` actually is, when there is one."
  [current previous]
  (cond-> current
    previous (assoc :previous (select-keys previous [:league-id :season]))))

(defn cached-history
  "`{:seasons [current previous]}` as the last sync left them, or nil when this
  league-season has never been cached. Reads disk only."
  [provider league-id season]
  (when-let [current (read-season (keyword provider) league-id season)]
    (let [{prev-id :league-id prev-season :season} (:previous current)
          previous (when prev-id (read-season (keyword provider) prev-id prev-season))]
      {:seasons (cond-> [current] previous (conj previous))})))

(defn summary
  "What the browser is told about a history: per season, how much there is and
  when it was fetched. The auctions themselves stay on the server."
  [{:keys [seasons]}]
  {:seasons (mapv (fn [{:keys [season auctions non-competing fetched-at]}]
                    {:season        season
                     :auctions      (count auctions)
                     :contested     (count (filter #(< 1 (count (:bids %))) auctions))
                     :non-competing (or non-competing 0)
                     :fetched-at    fetched-at})
                  seasons)})

(defn cached-summary
  "`summary` of what the waiver board would price from, or nil when nothing is
  cached or the cache will not summarize — asked on a failure path, where
  throwing would take the rosters down with it."
  [provider league-id season]
  (try (some-> (cached-history provider league-id season) summary)
       (catch Exception e
         (log/warn e "unreadable bid history summary" provider league-id season)
         nil)))

(defn bid-history
  "{:provider :league-id :season :credentials} -> `{:ok true :history summary}`,
  `{:ok false :unsupported? true}` for a host with no bid history to read, or
  `{:ok false :status :error :cached summary}`, where `:cached` summarizes what
  the waiver board will still price from, or is nil when nothing is.

  Never throws. The sync runs it beside the rosters, and a history that fails
  must not take them down with it."
  [{:keys [provider league-id season credentials]}]
  (let [provider-kw     (keyword provider)
        resolved-season (season/resolve-season season)
        access-error    (providers/league-access-error provider-kw league-id credentials)]
    (cond
      (not (providers/bid-history? provider-kw))
      {:ok false :unsupported? true}

      access-error
      {:ok false :status 400 :error (:error access-error)}

      :else
      (try
        (let [cached   (read-season provider-kw league-id resolved-season)
              current  (if (:final? cached)
                         cached
                         (fetch-season provider-kw {:league-id league-id :season resolved-season
                                                    :credentials credentials}))
              previous (previous-season! provider-kw (previous-link current cached) credentials)
              current  (linked current previous)]
          (when (not= current cached)
            (write-season! provider-kw current))
          {:ok true :history (summary {:seasons (cond-> [current] previous (conj previous))})})
        (catch Exception e
          (let [cause (league-sync/unwrap-execution e)]
            {:ok     false
             :status (or (:status (ex-data cause)) 502)
             :error  (or (ex-message cause) (str (class cause)))
             :cached (cached-summary provider-kw league-id resolved-season)}))))))
