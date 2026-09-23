(ns draft-day.ingestion.transactions.sleeper
  "Sleeper provider for bid history: one league-season's transaction log, one
  request per week played.

  Sleeper publishes failed waiver claims beside the ones that went through, each
  with its bid, so every auction a league has run can be rebuilt. A season that
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

  Each Sleeper season is its own league, linked back by `previous_league_id`;
  that link becomes the season's `:previous`, and `ingestion.transactions`
  decides whether to follow it."
  (:require [draft-day.ingestion.league-import.sleeper :as import-sleeper]
            [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.transactions :as transactions])
  (:import [java.util.concurrent Semaphore]))

(def max-in-flight
  "How many requests Sleeper sees from one history fetch at once. Its documented
  ceiling is 1000 a minute, so this is politeness rather than a limit: forty
  weekly logs fired together is a burst from one IP for no gain a handful of
  connections does not already give."
  4)

(defonce ^:private throttle (Semaphore. max-in-flight true))

(defn throttled
  "Call `f` holding one of the host's permits, released however `f` leaves — a
  fetch that throws must not retire a permit for good."
  [f]
  (.acquire throttle)
  (try (f) (finally (.release throttle))))

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
  (range 1 (inc (long (or (get-in league [:settings :leg]) 0)))))

(defn faab?
  "Did this league-season bid for its waivers? One that did not still files its
  claims, each with no bid on it, and read as auctions they would say every
  player on the wire went for $0."
  [league]
  (= :faab (:type (import-sleeper/waiver-settings league))))

(defn fetch-season
  "Network: one league-season's league document, rosters and weekly logs — or
  its league document alone when the season did not run FAAB (see `faab?`),
  which has no auctions to read."
  [league-id]
  (let [league (throttled #(import-sleeper/fetch-league league-id))
        tasks  (if (faab? league)
                 (into {:rosters #(throttled (fn [] (sync-sleeper/fetch-json league-id "rosters")))}
                       (map (fn [w] [w #(throttled (fn [] (fetch-week league-id w)))]))
                       (weeks-played league))
                 {})
        got    (parallel/all tasks)]
    {:league  league
     :rosters (:rosters got)
     :weeks   (into (sorted-map) (dissoc got :rosters))}))

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
  nil. A Sleeper predecessor is always the year before."
  [league]
  (when-let [pid (previous-league-id league)]
    (when-let [yr (parse-long (str (:season league)))]
      {:league-id pid :season (str (dec yr))})))

(defn owners
  "`{roster-id owner-id}` for one season; an orphan roster maps to nil."
  [rosters]
  (into {} (map (juxt :roster_id :owner_id)) rosters))

(defn claims
  "One week's processed waiver claims, one row per player added. Free-agent adds
  and trades are not auctions, and a pending claim has not happened yet.

  `:adds` is keyed by player id, and `draft-day.json/mapper` keywordizes every
  key it decodes, so `{\"4034\": 3}` arrives as `{:4034 3}` — hence `name`."
  [week txs]
  (into []
        (comp (filter #(= "waiver" (:type %)))
              (filter #(#{"complete" "failed"} (:status %)))
              (mapcat (fn [tx]
                        (map (fn [[pid rid]]
                               {:week      week
                                :at        (:status_updated tx)
                                :player-id (name pid)
                                :roster-id rid
                                :amount    (or (get-in tx [:settings :waiver_bid]) 0)
                                :won?      (= "complete" (:status tx))})
                             (:adds tx)))))
        txs))

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
  [claims owner-of]
  (let [bidders (count (distinct (map :roster-id claims)))]
    (if-let [winner (first (sort-by (comp - :amount) (filter :won? claims)))]
      (let [bids (->> claims
                      (filter #(or (:won? %) (<= (:amount %) (:amount winner))))
                      one-per-roster
                      (sort-by (juxt (comp not :won?) (comp - :amount))))]
        [{:week      (:week winner)
          :at        (:at winner)
          :player-id (:player-id winner)
          :bids      (mapv (fn [{:keys [roster-id amount won?]}]
                             {:roster-id roster-id
                              :owner-id  (owner-of roster-id)
                              :amount    amount
                              :won?      won?})
                           bids)}
         (- bidders (count bids))])
      [nil bidders])))

(defmethod transactions/normalize-season :sleeper
  [_ {:keys [league rosters weeks]}]
  (let [owner-of (owners rosters)
        results  (->> weeks
                      (mapcat (fn [[w txs]] (claims w txs)))
                      (group-by (juxt :at :player-id))
                      (sort-by key)
                      (map #(auction (val %) owner-of)))]
    {:season        (some-> (:season league) str)
     :league-id     (some-> (:league_id league) str)
     :budget        (or (get-in league [:settings :waiver_budget]) 0)
     :final?        (= "complete" (:status league))
     :previous      (previous league)
     :auctions      (into [] (keep first) results)
     :non-competing (reduce + 0 (map second results))}))
