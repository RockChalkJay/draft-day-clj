(ns draft-day.faab.leagues
  "The league-seasons the FAAB harnesses replay: every one in the crawled corpus
  a replay can be built for, frozen in `replay_leagues.edn` so a result stays
  reproducible as the corpus grows.

  Eligible is a finished 2024 or 2025 season that ran FAAB, had at least
  `min-auctions` auctions and `min-contested` with two or more bidders, and
  continues a previous season — the live board prices from a history that
  reaches back a year, so the replay does too.

  Each league carries its stratum (season, kind, a $100 budget or another,
  superflex or not), a half and an order. The halves split every stratum
  evenly between `:fit`, which `draft-day.faab.interest` fits on and
  `draft-day.faab.sweep` tunes on, and `:score`, which neither ever sees. The
  order is a seeded random one, so the first n leagues of either half are a
  random sample of it, a smaller sample always inside a larger one: a learning
  curve's subsamples are nested, and a fetch stopped part way has fetched a
  random subset rather than the busiest leagues.

  The corpus is a walk over one account's leaguemates, not a random sample of
  Sleeper: dynasty and superflex are over-represented against Sleeper at
  large, which is why results are reported by stratum as well as pooled.

    lein run -m draft-day.faab.leagues -- --select   ; rewrite the frozen set
    lein run -m draft-day.faab.leagues -- --fetch    ; fetch what a replay needs"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.faab.corpus :as corpus]
            [draft-day.faab.replay :as replay]))

(def set-file "dev/draft_day/faab/replay_leagues.edn")

(def seed
  "The seed every split and order is drawn from."
  20260926)

(def min-auctions 50)

(def min-contested 20)

(def seasons #{"2024" "2025"})

(defn previous-id
  "The season a league continues, or nil: Sleeper spells none as nil or \"0\"."
  [{:keys [previous-league-id]}]
  (when (and previous-league-id (not= "0" (str previous-league-id)))
    (str previous-league-id)))

(defn eligible?
  "Can a replay be built for this corpus season, and is there enough in it to
  score?"
  [{:keys [meta season]}]
  (let [auctions (:auctions season)]
    (and (contains? seasons (str (:season meta)))
         (:final? season)
         (pos? (or (:budget meta) 0))
         (previous-id meta)
         (>= (count auctions) min-auctions)
         (>= (count (filter #(< 1 (count (:bids %))) auctions)) min-contested))))

(defn stratum
  "What results are broken down by."
  [{:keys [season kind budget superflex?]}]
  {:season     (str season)
   :kind       kind
   :budget     (if (== 100 budget) :b100 :other)
   :superflex? (boolean superflex?)})

(defn order-key
  "A league's place in the seeded random order."
  [league-id]
  (.nextDouble (java.util.Random. (hash [seed (str league-id)]))))

(defn assign
  "The frozen rows for `metas`: in the seeded order, each stratum's leagues
  alternating between the halves, so each half holds half of every stratum."
  [metas]
  (let [rows (->> metas
                  (map (fn [m] {:league-id (str (:league-id m))
                                :previous  (previous-id m)
                                :stratum   (stratum m)
                                :order     (order-key (:league-id m))}))
                  (sort-by :order))]
    (->> (group-by :stratum rows)
         vals
         (mapcat (fn [rs] (map-indexed (fn [i r] (assoc r :half (if (even? i) :fit :score))) rs)))
         (sort-by :order)
         vec)))

(defn select!
  "Rewrite the frozen set from the corpus."
  []
  (let [rows (assign (map :meta (filter eligible? (corpus/load-seasons))))]
    (spit set-file (str "[" (str/join "\n " (map #(pr-str (dissoc % :order)) rows)) "]\n"))
    rows))

(defn load-set
  "The frozen set, in its order."
  []
  (->> (edn/read-string (slurp set-file))
       (map #(assoc % :order (order-key (:league-id %))))
       (sort-by :order)
       vec))

(defn pick
  "The frozen set's `half` (:fit, :score or :all), its first `n` when given."
  [rows half n]
  (cond->> (filter #(or (= :all half) (= half (:half %))) rows)
    n (take n)))

(def skipped-file (str replay/cache-dir "/skipped.edn"))

(defn fetch!
  "Everything a replay of `rows` needs, one request at a time at the crawl's
  pace: each league's documents and its previous season's, and each season's
  played weeks and byes. Documents already cached cost nothing, so a stopped
  fetch resumes where it stopped. A league Sleeper will not serve is recorded
  in `skipped-file` and passed over, not retried every run."
  [rows]
  (let [skipped (atom (or (some-> (io/file skipped-file) (#(when (.exists %) (edn/read-string (slurp %)))))
                          {}))
        n       (count rows)]
    (doseq [season (distinct (map (comp parse-long :season :stratum) rows))]
      (replay/actual-rows season)
      (replay/byes season))
    (doseq [[i {:keys [league-id previous]}] (map-indexed vector rows)
            :when (not (contains? @skipped league-id))]
      (try
        (replay/league-docs league-id)
        (replay/league-docs previous)
        (catch Exception e
          (swap! skipped assoc league-id (ex-message e))
          (io/make-parents skipped-file)
          (spit skipped-file (pr-str @skipped))))
      (when (zero? (mod (inc i) 25))
        (println (format "  fetched %d of %d, %d skipped" (inc i) n (count @skipped)))
        (flush)))
    @skipped))

(defn skipped
  "League ids `fetch!` could not fetch."
  []
  (let [f (io/file skipped-file)]
    (if (.exists f) (set (keys (edn/read-string (slurp f)))) #{})))

(defn -main [& args]
  (let [flags (set args)]
    (when (flags "--select")
      (let [rows (select!)]
        (println (count rows) "leagues;"
                 (frequencies (map :half rows)) ";"
                 (into (sorted-map) (frequencies (map (comp (juxt :season :kind) :stratum) rows))))))
    (when (flags "--fetch")
      (let [rows (load-set)
            s    (fetch! rows)]
        (println "done:" (count rows) "leagues," (count s) "skipped")))
    (shutdown-agents)))
