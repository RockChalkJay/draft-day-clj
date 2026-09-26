(ns draft-day.faab.replay
  "Score the FAAB bid model against a league's real waiver auctions: rebuild each
  past waiver run from what was known before it, price it through the shipped
  code, and compare every prediction with the bids that were actually placed.

  What the board would have known at a run is rebuilt, not read:

  - Rosters are the season's final rosters with every transaction from the run
    onward undone, newest first — a trade or a drop reverses as cleanly as a
    claim. Nobody is on IR, because Sleeper keeps no history of who was.
  - What each manager had left is the budget less the claims he won before the
    run, so a manager who spent early is broke here too.
  - Rest of season is the replay universe (that season's preseason projection)
    blended with Sleeper's own weekly actuals for the weeks played before the
    run, as the live board blends them.
  - The bid history is the league's own, cut at the run, beside the whole of
    the season before it.
  - The Sleeper-wide backbone is refit without this league, whose seasons are
    in the corpus it was measured from. `--league-prior` keeps the committed one.
  - Heat is off: Sleeper keeps no past trending lists, and waiver order is
    unknown, so every tie is a coin flip.

  Then `waiver/market-inputs` and `faab/market` price it, the one road the
  board takes too, and four things are scored:

  - Who bids. For every team and every free agent at a week's first run, the
    chance he bids on him that week, against whether he did. Per week, because
    the model's claim rate is a weekly one and daily waivers run several times
    a week.
  - Win chance. Each real bid's P(win | its amount), from its bidder's side,
    against whether it won.
  - The top rival bid. From each bidder's side, the model's range for the
    highest other bid against the one that was placed, and its chance that
    nobody else bid against whether anybody did.
  - The bid itself. For every auction's winner, what the value bid would have
    paid and whether it would still have won, beside today's walk-away and the
    Sleeper-wide median for the predicted bidder count.

  Intervals resample whole weeks, since the auctions in a week share its news.

    lein run -m draft-day.faab.replay                  ; the author's 2025 league
    lein run -m draft-day.faab.replay -- --league 123 --league-prior"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.benchmark.metrics :as bm]
            [draft-day.bid-prior :as prior]
            [draft-day.db :as db]
            [draft-day.faab.corpus :as corpus]
            [draft-day.faab.crawl :as crawl]
            [draft-day.faab.report :as report]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.sleeper]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.sleeper]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.sleeper-actual :as actual]
            [draft-day.ingestion.transactions :as transactions]
            [draft-day.ingestion.transactions.sleeper]
            [draft-day.rankings.faab :as faab]
            [draft-day.rankings.ros :as ros]
            [draft-day.rankings.waiver :as waiver]
            [draft-day.replay.universe :as universe]
            [draft-day.scoring :as scoring]))

(def default-league
  "The author's 2025 league, whose 2024 season it continues."
  "1230018965216956416")

(def cache-dir (str crawl/cache-dir "/replay"))

(defn cached!
  "`f`'s value, read from `path` when it has been written before."
  [path f]
  (or (pipeline/read-transit path)
      (let [v (f)]
        (io/make-parents path)
        (pipeline/write-transit! path v)
        v)))


(defn body!
  "A Sleeper path's body, or a throw: a backtest missing a week reads exactly
  like a league that did not bid that week."
  [path]
  (let [{:keys [ok? body reason]} (crawl/fetch path)]
    (if ok? body (throw (ex-info "Sleeper fetch failed" {:path path :reason reason})))))

(defn league-docs
  "A finished league-season's league, users, final rosters and every week's
  transactions."
  [league-id]
  (cached! (str cache-dir "/league-" league-id ".transit")
           #(let [league (body! (str "/league/" league-id))]
              (when-not (= "complete" (:status league))
                (throw (ex-info "only a finished season can be replayed" {:league-id league-id})))
              {:league  league
               :users   (body! (str "/league/" league-id "/users"))
               :rosters (body! (str "/league/" league-id "/rosters"))
               :weeks   (into (sorted-map)
                              (map (fn [w] [w (body! (str "/league/" league-id "/transactions/" w))]))
                              (range 1 19))})))

(defn actual-rows
  "Every played game of a season, as `sleeper-actual` reads them."
  [season]
  (cached! (str cache-dir "/actuals-" season ".transit")
           #(into [] (comp (mapcat (fn [w] (actual/fetch-week season w)))
                           (keep actual/entry->row))
                  (range 1 (inc (nflverse/games-in-season season))))))

(defn byes [season]
  (cached! (str cache-dir "/byes-" season ".transit") #(sleeper/fetch-byes season)))


(defn moves
  "A season's completed transactions, oldest first: `{:at :type :adds :drops
  :bid}`, with `:adds` and `:drops` as `{player-id roster-id}`."
  [weeks]
  (->> (vals weeks)
       (apply concat)
       (filter #(= "complete" (:status %)))
       (map (fn [tx]
              {:at    (:status_updated tx)
               :type  (:type tx)
               :adds  (update-keys (or (:adds tx) {}) name)
               :drops (update-keys (or (:drops tx) {}) name)
               :bid   (get-in tx [:settings :waiver_bid])}))
       (sort-by :at)
       vec))

(defn rosters-at
  "`{roster-id #{player-id}}` just before `t`: the final rosters with every move
  from `t` on undone, newest first."
  [final moves t]
  (reduce (fn [r {:keys [adds drops]}]
            (as-> r r
              (reduce (fn [r [pid rid]] (update r rid disj pid)) r adds)
              (reduce (fn [r [pid rid]] (update r rid (fnil conj #{}) pid)) r drops)))
          final
          (reverse (filter #(>= (:at %) t) moves))))

(defn spent-before
  "`{roster-id dollars}` won in waiver claims before `t`."
  [moves t]
  (reduce (fn [acc {:keys [type adds bid at]}]
            (if (and (= "waiver" type) (< at t) (pos? (or bid 0)) (seq adds))
              (update acc (val (first adds)) (fnil + 0) bid)
              acc))
          {} moves))

(defn season-before
  "A normalized season cut to the auctions decided before `t`."
  [season t]
  (update season :auctions (fn [as] (filterv #(< (:at %) t) as))))

(defn league-at
  "The synced league as the board would have read it at `t`."
  [synced final moves t]
  (let [rosters (rosters-at final moves t)
        spent   (spent-before moves t)
        budget  (get-in synced [:waiver :budget])]
    (update synced :teams
            (fn [ts]
              (mapv (fn [{:keys [roster-id] :as team}]
                      (let [held (vec (sort (get rosters roster-id)))]
                        (assoc team
                               :player-ids held :active-ids held :starter-ids []
                               :faab-left (- budget (get spent roster-id 0))
                               :waiver-position nil)))
                    ts)))))

(defn board-at
  "The universe as the board would have ranked it for week `w`'s waivers: its
  preseason line blended with the weeks played before `w`."
  [players rows byes scoring season w]
  (let [through (dec w)
        by-id   (actual/accumulate (filterv #(<= (:week %) through) rows))]
    (-> (mapv (fn [p]
                (cond-> (merge p (get by-id (:player-id p)))
                  (get byes (:team p)) (assoc :bye (get byes (:team p)))))
              players)
        (ros/with-ros scoring {:through-week through
                               :season-games (nflverse/games-in-season season)})
        (waiver/with-form-points scoring))))


(defn prior-vars
  "A `report/backbone` as the values `bid-prior` holds. Sleeper spells a team
  defense DEF; the app, and the table, DST."
  [bb]
  {#'prior/bid-share      (:bid-share bb)
   #'prior/position-shift (update-keys (:position bb) #(if (= "DEF" %) "DST" %))
   #'prior/team-shift     (select-keys (:teams bb) [:small :standard :large])
   #'prior/budget-shift   (:budget-shift bb)
   #'prior/heaping        (:heaping bb)
   #'prior/managers       (select-keys (:managers bb) [:per-week :zero-share :aggression])})

(defn backbone-without
  "The backbone measured over every crawled league-season but `league-ids`."
  [league-ids]
  (cached! (str cache-dir "/backbone-without-" (str/join "-" (sort league-ids)) ".transit")
           #(let [out     (set league-ids)
                  seasons (remove (fn [s] (out (get-in s [:meta :league-id]))) (corpus/load-seasons))
                  rows    (corpus/corpus-rows seasons (corpus/fetch-positions!))
                  bids    (corpus/bid-rows rows)]
              (report/backbone bids (report/manager-habits rows bids)))))


(defn top-cdf
  "P(the highest rival bid is at most `x`), nobody bidding counting as below."
  [active x]
  (reduce * 1.0 (map (fn [{:keys [p ^doubles cdf]}]
                       (+ (- 1.0 p) (* p (aget cdf (min (long x) (dec (alength cdf)))))))
                     active)))

(defn outcome
  "Does bid `x` beat a top rival bid of `other` (nil for none)? 1, 0, or ½ on a
  tie, which waiver order decides and the replay cannot see."
  [x other]
  (cond (nil? x) 0.0 (nil? other) 1.0 (> x other) 1.0 (= x other) 0.5 :else 0.0))

(defn median-rule
  "The Sleeper-wide median winning bid for the predicted bidder count."
  [n-hat budget]
  (long (Math/round (* budget (get-in prior/winning-bid [(prior/bucket (max 1 n-hat)) :p50])))))

(defn bid-rows
  "One row per real bid in `auction`, each scored from its bidder's side."
  [{:keys [bids] :as auction} priced week first-run?]
  (keep (fn [{:keys [roster-id amount won?]}]
          (when-let [{:keys [p market]} (get priced roster-id)]
            (let [active (faab/active-rivals p market)
                  others (keep #(when (not= roster-id (:roster-id %)) (:amount %)) bids)
                  top    (when (seq others) (reduce max others))
                  n-hat  (Math/round (+ 1.0 (reduce + 0.0 (map :p active))))
                  {:keys [min-bid left budget]} market
                  worth  (:walk-away p)
                  value  (when worth (faab/value-bid active worth min-bid left))]
              {:season      (str "w" week)          ; the bootstrap's block
               :week        week
               :first-run?  first-run?
               :roster-id   roster-id
               :amount      amount
               :won?        won?
               :p-win       (faab/win-chance active amount)
               :top         top
               :top-range   (faab/top-bid active budget)
               :p-none      (reduce * 1.0 (map #(- 1.0 (:p %)) active))
               :pit         (when top (top-cdf active top))
               :rivals-hat  (reduce + 0.0 (map :p active))
               :rivals      (count others)
               :walk-away   worth
               :value       value
               :median-rule (median-rule n-hat budget)})))
        bids))

(defn price-for
  "`{roster-id {:p row :market m}}`: `player-id` as each bidder's own board
  priced him at the run, or absent where that board has no row for him — not in
  the universe, or on a roster by the reconstruction."
  [board ctx roster-ids player-id]
  (into {}
        (keep (fn [rid]
                (let [in (waiver/market-inputs board (assoc ctx :my-roster-id rid))]
                  (when-let [p (first (filter #(= player-id (:player-id %)) (:fas in)))]
                    [rid {:p p :market (faab/market (:fas in) (:market in))}]))))
        roster-ids))

(defn who-bids-rows
  "Every team's chance of bidding on every free agent over week `w`, against
  whether it did, from the week's first run."
  [board ctx week-bids w]
  (let [{:keys [fas xwalk by-id seats habits]} (waiver/market-inputs board (dissoc ctx :my-roster-id))
        {:keys [league starting-slots]} ctx
        waiver-s (:waiver league)
        needs    (waiver/rival-needs (:teams league) nil xwalk by-id seats starting-slots fas)
        teams    (faab/bidders needs {} fas habits (:budget waiver-s) (or (:min-bid waiver-s) 0)
                               (faab/heat-of board))]
    (for [t teams, p fas]
      {:season (str "w" w)
       :week   w
       :player (:player-id p)
       :vorp   (:ros-vorp p)
       :form   (:form-points p)
       :p      (faab/bid-chance (get (:rates t) (:player-id p) 0.0))
       :bid?   (contains? week-bids [(:roster-id t) (:player-id p)])})))


(defn replay
  "Every scored row for one finished league-season."
  [league-id {:keys [league-prior?]}]
  (let [{:keys [league users rosters weeks] :as docs} (league-docs league-id)
        season   (Long/parseLong (str (:season league)))
        current  (transactions/normalize-season :sleeper {:league league :weeks weeks})
        previous (when-let [prev (some-> (:previous current) :league-id)]
                   (let [d (league-docs prev)]
                     (transactions/normalize-season :sleeper {:league (:league d) :weeks (:weeks d)})))
        synced   (assoc (league-sync/normalize-rosters :sleeper {:rosters rosters :users users :league league})
                        :provider :sleeper)
        final    (into {} (map (fn [r] [(:roster_id r) (set (map str (:players r)))])) rosters)
        ms       (moves weeks)
        cfg      (league-import/normalize-league :sleeper league)
        scoring  (scoring/resolve-config (:scoring cfg))
        players  (universe/season-universe season)
        rows     (actual-rows season)
        bye-map  (byes season)
        base-ctx {:num-teams          (count (:teams synced))
                  :replacement-config (select-keys (:roster cfg) [:qb :rb :wr :te :flex])
                  :season-games       (nflverse/games-in-season season)
                  :playoff-week-start (:playoff-week-start synced)
                  :starting-slots     (db/scoring-slots (:roster-positions synced))}
        runs     (sort-by key (group-by :at (:auctions current)))
        run!     (fn []
                   (reduce
                    (fn [acc [t auctions]]
                      (let [w      (:week (first auctions))
                            board  (board-at players rows bye-map scoring season w)
                            ctx    (assoc base-ctx
                                          :league (league-at synced final ms t)
                                          :through-week (dec w)
                                          :bid-history {:seasons (cond-> [(season-before current t)]
                                                                   previous (conj previous))})
                            first? (= t (reduce min (map :at (filter #(= w (:week %)) (:auctions current)))))
                            acc    (if first?
                                     (let [wb (into #{} (mapcat (fn [a] (map (fn [b] [(:roster-id b) (:player-id a)]) (:bids a))))
                                                    (filter #(= w (:week %)) (:auctions current)))]
                                       (update acc :who into (who-bids-rows board ctx wb w)))
                                     acc)]
                        (reduce (fn [acc {:keys [player-id bids] :as a}]
                                  (let [priced (price-for board ctx (map :roster-id bids) player-id)
                                        scored (bid-rows a priced w first?)]
                                    (-> acc
                                        (update :bids into scored)
                                        (update :missing + (- (count bids) (count scored))))))
                                acc auctions)))
                    {:bids [] :who [] :missing 0 :auctions (count (:auctions current)) :runs (count runs)}
                    runs))]
    (if league-prior?
      (run!)
      (with-redefs-fn (prior-vars (backbone-without (cond-> [league-id]
                                                      previous (conj (:league-id previous)))))
        run!))))


(defn mean [xs] (when (seq xs) (/ (reduce + 0.0 xs) (count xs))))

(defn brier [ps outcomes] (mean (map (fn [p o] (let [d (- p o)] (* d d))) ps outcomes)))

(defn reliability
  "`[[lo hi n mean-predicted observed-rate]]` over `edges`."
  [rows p-key outcome edges]
  (keep (fn [[lo hi]]
          (let [in (filter #(and (>= (p-key %) lo) (< (p-key %) hi)) rows)]
            (when (seq in)
              [lo hi (count in) (mean (map p-key in)) (mean (map outcome in))])))
        (partition 2 1 edges)))

(defn print-reliability [title rows p-key outcome edges]
  (println (str "\n-- " title " --"))
  (println "  predicted       n     mean pred   observed")
  (doseq [[lo hi n mp ob] (reliability rows p-key outcome edges)]
    (println (format "  %4.0f%%–%3.0f%%  %6d   %8.1f%%   %7.1f%%" (* 100.0 lo) (* 100.0 (min 1 hi)) n (* 100 mp) (* 100 ob))))
  (let [ps (map p-key rows) os (map outcome rows) base (mean os)]
    (println (format "  Brier %.4f   against %.4f for the base rate alone (%.1f%%)"
                     (brier ps os) (brier (repeat (count os) base) os) (* 100 base)))))

(defn ci-str [{:keys [point lo hi]}]
  (if lo (format "%+.2f [%+.2f, %+.2f]" point lo hi) (format "%+.2f" point)))

(defn print-report [{:keys [bids who missing auctions runs]}]
  (let [winners (filter :won? bids)
        one     #(if (:won? %) 1.0 0.0)]
    (println (format "\n%d auctions over %d waiver runs; %d bids scored, %d not (no board row for the player)"
                     auctions runs (count bids) missing))
    (print-reliability "Who bids: a team's chance of bidding on a free agent that week"
                       who :p #(if (:bid? %) 1.0 0.0) [0 0.01 0.02 0.05 0.1 0.2 0.5 1.01])
    (let [player-weeks (vals (group-by (juxt :week :player) who))
          bid-on       (fn [rs] (if (some :bid? rs) 1.0 0.0))
          rate         (fn [rs] (format "%5.1f%% of %5d" (* 100 (mean (map bid-on rs))) (count rs)))]
      (println "\n-- What free agents drew any bid that week, by what the board knew --")
      (doseq [[label pred] [["value over replacement > 0"   #(pos? (or (:vorp (first %)) 0))]
                            ["value over replacement <= 0"  #(not (pos? (or (:vorp (first %)) 0)))]
                            ["form >= 10 points a game"     #(>= (or (:form (first %)) 0) 10)]
                            ["form 5-10"                    #(<= 5 (or (:form (first %)) 0) 9.999)]
                            ["form under 5, or none"        #(< (or (:form (first %)) 0) 5)]]]
        (println (format "  %-30s %s" label (rate (filter pred player-weeks))))))
    (print-reliability "Win chance at the amount actually bid" bids :p-win one
                       [0 0.1 0.25 0.5 0.75 0.9 0.99 1.01])
    (doseq [[label rs] [["first run of the week" (filter :first-run? bids)]
                        ["later runs" (remove :first-run? bids)]]]
      (when (seq rs)
        (println (format "  %-22s n=%-4d mean pred %.1f%%  won %.1f%%" label (count rs)
                         (* 100 (mean (map :p-win rs))) (* 100 (mean (map one rs)))))))
    (let [contested (filter :top bids)]
      (println "\n-- The top rival bid, from each bidder's side --")
      (println (format "  predicted chance nobody else bids %.1f%%, observed %.1f%%"
                       (* 100 (mean (map :p-none bids)))
                       (* 100 (mean (map #(if (:top %) 0.0 1.0) bids)))))
      (println (format "  rivals: predicted %.2f a bid, observed %.2f"
                       (mean (map :rivals-hat bids)) (mean (map (comp double :rivals) bids))))
      (when (seq contested)
        (let [ranged (filter :top-range contested)]
          (println (format "  where somebody else bid (n=%d): at or under the p50 %.0f%% (target 50%%), under the p90 %.0f%% (target 90%%)"
                           (count ranged)
                           (* 100 (mean (map #(if (<= (:top %) (first (:top-range %))) 1.0 0.0) ranged)))
                           (* 100 (mean (map #(if (<= (:top %) (second (:top-range %))) 1.0 0.0) ranged)))))
          (println (format "  median |p50 - actual| $%.1f"
                           (report/quantile (map #(Math/abs (double (- (:top %) (first (:top-range %))))) ranged) 0.5))))))
    (println "\n-- Each winner's bid under three rules --")
    (println (format "  over the %d winners every rule can bid for; %d had spent out and won at $0"
                     (count (filter :walk-away winners)) (count (remove :walk-away winners))))
    (println "  rule          saved a claim   claims kept   (paid less than the winner did, kept = still wins)")
    (let [rule-row (fn [label k]
                     (let [rs  (filter :walk-away winners)
                           win (map #(outcome (k %) (:top %)) rs)
                           sav (map (fn [r o] (* o (- (:amount r) (k r)))) rs win)]
                       (println (format "  %-12s  $%6.2f        %5.1f%% of %d" label (mean sav)
                                        (* 100 (mean win)) (count rs)))))]
      (rule-row "value bid" :value)
      (rule-row "walk-away" :walk-away)
      (rule-row "median rule" :median-rule))
    (let [paired (filter #(and (:value %) (:walk-away %)) winners)
          gain   (fn [k] (fn [rows] (mean (map (fn [r] (let [o (outcome (k r) (:top r))]
                                                         (- (* o (- (:amount r) (k r)))
                                                            (* (- 1 o) 10.0))))
                                              rows))))
          diff   (fn [a b] (fn [rows] (- ((gain a) rows) ((gain b) rows))))]
      (when (seq paired)
        (println "\n  Paired, a lost claim costed at $10, 95% interval resampling whole weeks:")
        (println (str "  value bid − walk-away    " (ci-str (bm/block-bootstrap-ci paired (diff :value :walk-away)))))
        (println (str "  value bid − median rule  " (ci-str (bm/block-bootstrap-ci paired (diff :value :median-rule)))))))))

(defn -main [& args]
  (let [flags     (set args)
        league-id (or (second (drop-while #(not= "--league" %) args)) default-league)]
    (println "Replaying league" league-id
             (if (flags "--league-prior") "(committed prior)" "(prior refit without it)"))
    (print-report (replay league-id {:league-prior? (boolean (flags "--league-prior"))})))
  (shutdown-agents))
