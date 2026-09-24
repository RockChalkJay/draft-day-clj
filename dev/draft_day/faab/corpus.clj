(ns draft-day.faab.corpus
  "The crawled FAAB seasons as rows the report can slice: one per waiver
  auction (see `draft-day.ingestion.transactions`), with its bids as shares of
  the season's budget, and one per manager-season.

  A bid is read as a share of the budget its league started the season with,
  so a $100 league and a $1000 one land on one scale. Positions come from
  Sleeper's own player dump, fetched once and cached, because a corpus spanning
  three seasons names players the app's universe has long since dropped.

  The two denominators that matter are kept apart. `:share` is of the season's
  budget, the scale bids are compared on; `:remaining` is what that manager
  still had before the run the bid was decided in, reconstructed from his own
  earlier wins — the ceiling a broke manager bids against."
  (:require [clojure.java.io :as io]
            [draft-day.faab.crawl :as crawl]
            [draft-day.ingestion.pipeline :as pipeline]))

(def positions-file (str crawl/cache-dir "/positions.transit"))

(defn fetch-positions!
  "Network: `{player-id position}` for every player Sleeper knows, cached. The
  dump is several megabytes and Sleeper asks that it be read at most daily; a
  corpus needs it once."
  []
  (or (try (pipeline/read-transit positions-file) (catch Exception _ nil))
      (let [resp (crawl/fetch "/players/nfl")]
        (when-not (:ok? resp)
          (throw (ex-info "Sleeper player dump unreadable" {:reason (:reason resp)})))
        (let [m (into {} (keep (fn [[id p]]
                                 (when-let [pos (or (:position p) (first (:fantasy_positions p)))]
                                   [(name id) pos])))
                      (:body resp))]
          (io/make-parents positions-file)
          (pipeline/write-transit! positions-file m)
          m))))

(defn load-seasons
  "Every accepted season the crawl has saved, as `{:meta :season}`."
  ([] (load-seasons (str crawl/cache-dir "/seasons")))
  ([dir]
   (->> (file-seq (io/file dir))
        (filter #(re-matches #"\d+\.transit" (.getName %)))
        (keep #(try (pipeline/read-transit (.getPath %)) (catch Exception _ nil)))
        vec)))

(defn phase
  "Early, middle or late season, by the week an auction was decided in."
  [week]
  (cond (<= week 4) :early (<= week 10) :mid :else :late))

(defn bucket
  "Bidder count, with four and up pooled: the expensive auctions are rare enough
  that splitting them further leaves cells too thin to read."
  [n]
  (min 4 n))

(defn team-bucket
  "Small, standard or large league; unknown when the league did not say."
  [n]
  (cond (nil? n) :unknown (<= n 8) :small (<= n 12) :standard :else :large))

(defn auction-rows
  "One season's auctions as report rows, in the order they were decided.
  Spending advances a run at a time, a run being the auctions that share `:at`."
  [{:keys [meta season]} positions]
  (let [budget (double (:budget meta))
        fmt    (select-keys meta [:league-id :season :num-teams :kind :superflex?
                                  :scoring-rec :budget :daily? :previous-league-id])
        row    (fn [spent {:keys [week at player-id bids]}]
                 (let [shares (mapv #(/ (:amount %) budget) bids)
                       sorted (sort > shares)]
                   (merge fmt
                          {:week      week
                           :at        at
                           :phase     (phase week)
                           :player-id player-id
                           :position  (get positions player-id "?")
                           :n         (count bids)
                           :bucket    (bucket (count bids))
                           :winner    (first sorted)
                           :second    (second sorted)
                           :bids      (mapv (fn [b s]
                                              (assoc (select-keys b [:owner-id :roster-id :amount :won?])
                                                     :share s
                                                     :remaining (/ (- budget (get spent (:roster-id b) 0.0))
                                                                   budget)))
                                            bids shares)})))
        spend  (fn [spent {:keys [bids]}]
                 (if-let [winner (first (filter :won? bids))]
                   (update spent (:roster-id winner) (fnil + 0.0) (double (:amount winner)))
                   spent))]
    (first
     (reduce (fn [[rows spent] run]
               [(into rows (map #(row spent %)) run)
                (reduce spend spent run)])
             [[] {}]
             (partition-by :at (sort-by :at (:auctions season)))))))

(defn corpus-rows
  "Every auction row across the corpus."
  [seasons positions]
  (vec (mapcat #(auction-rows % positions) seasons)))

(defn bid-rows
  "Every competitive bid, carrying its auction's slicing keys."
  [rows]
  (vec (mapcat (fn [r]
                 (map #(merge (dissoc r :bids :winner :second) %) (:bids r)))
               rows)))

(defn weeks-played
  "How many weeks a season's auctions span: the denominator for claims a week."
  [rows]
  (count (distinct (map :week rows))))

(defn manager-seasons
  "One row per manager per league-season: how often he bids, how often he bids
  nothing, and how far his positive bids sit from the corpus's typical bid for
  the same kind of auction. `typical` maps `[bucket phase]` to that median
  share."
  [rows typical]
  (->> (group-by :league-id rows)
       (mapcat (fn [[lid season-rows]]
                 (let [weeks (max 1 (weeks-played season-rows))
                       meta  (select-keys (first season-rows) [:league-id :season :kind :superflex?
                                                               :num-teams :previous-league-id])]
                   (->> (bid-rows season-rows)
                        (filter :owner-id)
                        (group-by :owner-id)
                        (map (fn [[owner bs]]
                               (let [pos  (filter #(pos? (:share %)) bs)
                                     logs (keep (fn [b]
                                                  (when-let [t (get typical [(:bucket b) (:phase b)])]
                                                    (when (pos? t) (Math/log (/ (:share b) t)))))
                                                pos)]
                                 (merge meta
                                        {:owner-id   owner
                                         :bids       (count bs)
                                         :per-week   (/ (count bs) (double weeks))
                                         :zero-share (/ (count (remove #(pos? (:share %)) bs))
                                                        (double (count bs)))
                                         :positive   (count pos)
                                         :aggression (when (seq logs)
                                                       (let [v (vec (sort logs))]
                                                         (nth v (quot (count v) 2))))}))))))))
       vec))
