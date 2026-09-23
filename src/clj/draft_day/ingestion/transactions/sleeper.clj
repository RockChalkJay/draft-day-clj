(ns draft-day.ingestion.transactions.sleeper
  "Sleeper provider for bid history: one league-season's transaction log, one
  request per week played.

  Sleeper publishes failed waiver claims beside the ones that went through, each
  with its bid, so every waiver auction a league has run can be rebuilt. A season that
  did not run FAAB has no auctions, and its log is not asked for.

  Which failed claims lost an auction is read from the structure, never from the
  note text. A run's claims on one player with a completed claim among them are
  an auction, and its failed claims bidding no more than the winner are the bids
  it beat. A failed claim bidding more than the winner cannot have lost on price
  — the highest bid is processed first — so it failed on a roster or budget
  rule. And one manager's several claims on a player are one bidder: his own
  duplicates also read \"claimed by another owner\". A run is its processing
  time alone — every claim it decides carries the same `status_updated`, which
  all 308 losing bids in a real league's two seasons shared with their winner,
  and daily waivers process several runs a week.

  A bid's owner is the claim's `creator`, the manager who placed it, and not
  whoever holds the roster when the log is read: a roster taken over mid-season
  would otherwise hand its old manager's bids to the new one, and a finished
  season is cached for good.

  Each Sleeper season is its own league, linked back by `previous_league_id`;
  that link becomes the season's `:previous`, and `ingestion.transactions`
  decides whether to follow it."
  (:require [draft-day.ingestion.league-import.sleeper :as import-sleeper]
            [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.transactions :as transactions]))

(defn fetch-week
  "Network: one week's transactions. A week with none is `[]`, a real answer."
  [league-id week]
  (or (sync-sleeper/get-json (str "league/" league-id "/transactions/" week)
                             {:empty-is-missing? false})
      []))

(defn weeks-played
  "Every week the league has reached, the current one included: a waiver run is
  filed under the week it is processed in, and this week's may already have
  happened."
  [league]
  (let [weeks-played (or (get-in league [:settings :leg]) 0)]
    (range 1 (inc weeks-played))))

(defn faab?
  "Did this league-season bid for its waivers? One that did not still files its
  claims, each with no bid on it, and read as auctions they would say every
  player on the wire went for $0."
  [league]
  (= :faab (:type (import-sleeper/waiver-settings league))))

(defn fetch-season
  "Network: one league-season's league document and weekly logs — or its league
  document alone when the season did not run FAAB (see `faab?`), which has no
  auctions to read."
  [league-id]
  (let [league (import-sleeper/fetch-league league-id)
        weeks  (when (faab? league)
                 (parallel/all (into {}
                                     (map (fn [w] [w #(fetch-week league-id w)]))
                                     (weeks-played league))))]
    {:league league
     :weeks  (into (sorted-map) weeks)}))

(defmethod transactions/fetch-raw-season :sleeper
  [_ {:keys [league-id]}]
  (fetch-season league-id))

(defn previous-league-id
  "The league this one continues, or nil. Sleeper spells \"none\" both as a null
  and as \"0\"."
  [league]
  (let [p (some-> (:previous_league_id league) str)]
    (when (and (seq p) (not= "0" p)) p)))

(defn previous
  "The season this one continues, as `ingestion.transactions` addresses it, or
  nil. The season is a guess at the year before, which `ingestion.transactions`
  replaces with the one the predecessor's own document states."
  [league]
  (let [prev-league-id (previous-league-id league)
        season-year    (some-> (:season league) str parse-long)]
    (when (and prev-league-id season-year)
      {:league-id prev-league-id
       :season    (str (dec season-year))})))

(defn claims
  "One week's processed waiver claims, one row per player added. Free-agent adds
  and trades are not auctions, and a pending claim has not happened yet.

  `:adds` is keyed by player id, and `draft-day.json/mapper` keywordizes every
  key it decodes, so `{\"4034\": 3}` arrives as `{:4034 3}` — hence `name`."
  [week txs]
  (letfn [(claim-row [tx]
            (map (fn [[pid rid]]
                   {:week      week
                    :at        (:status_updated tx)
                    :player-id (name pid)
                    :roster-id rid
                    :owner-id  (:creator tx)
                    :amount    (or (get-in tx [:settings :waiver_bid]) 0)
                    :won?      (= "complete" (:status tx))})
                 (:adds tx)))]
    (into []
          (comp
            (filter #(= "waiver" (:type %)))
            (filter #(#{"complete" "failed"} (:status %)))
            (mapcat claim-row))
          txs)))

(defn one-per-roster
  "A roster's claims on one player in one run as its single best bid. Two claims
  from one manager — different drops, same target — are one bidder."
  [claims]
  (->> claims
       (group-by :roster-id)
       vals
       (map #(first (sort-by (juxt (comp not :won?) (comp - :amount)) %)))))

(defn auction
  "One run's claims on one player -> `[auction non-competing]`, the auction nil
  when nobody won, `non-competing` counting managers rather than claims. See the
  ns docstring for which failed claims are bids.

  Which claims competed is decided before one manager's are collapsed to one:
  his highest claim can fail on a roster rule while a lower one on the same
  player lost on price, and that lower one is the bid he was outbid at."
  [claims]
  (let [bidders (count (distinct (map :roster-id claims)))]
    (if-let [winner (first (sort-by (comp - :amount) (filter :won? claims)))]
      (let [bids (->> claims
                      (filter #(or (:won? %) (<= (:amount %) (:amount winner))))
                      one-per-roster
                      (sort-by (juxt (comp not :won?) (comp - :amount))))]
        [{:week      (:week winner)
          :at        (:at winner)
          :player-id (:player-id winner)
          :bids      (mapv #(select-keys % [:roster-id :owner-id :amount :won?]) bids)}
         (- bidders (count bids))])
      [nil bidders])))

(defmethod transactions/normalize-season :sleeper
  [_ {:keys [league weeks]}]
  (let [results (->> weeks
                     (mapcat (fn [[w txs]] (claims w txs)))
                     (group-by (juxt :at :player-id))
                     (sort-by key)
                     (map #(auction (val %))))]
    {:season        (some-> (:season league) str)
     :league-id     (some-> (:league_id league) str)
     :budget        (or (get-in league [:settings :waiver_budget]) 0)
     :final?        (= "complete" (:status league))
     :previous      (previous league)
     :auctions      (into [] (keep first) results)
     :non-competing (reduce + 0 (map second results))}))
