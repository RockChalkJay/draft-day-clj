(ns draft-day.ingestion.transactions-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [draft-day.ingestion.league-import.sleeper :as import-sleeper]
            [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.transactions :as transactions]
            [draft-day.ingestion.transactions.sleeper :as tx-sleeper]))

(defn- claim
  "One Sleeper transaction as `draft-day.json/mapper` decodes it: the `adds`
  keys, being player ids, arrive keywordized. Placed by roster N's manager,
  `uN`, unless `:creator` says otherwise."
  [status roster player bid at & {:keys [type creator]
                                  :or   {type "waiver" creator (str "u" roster)}}]
  {:type type :status status :status_updated at :creator creator
   :adds {(keyword player) roster} :settings {:waiver_bid bid}})

(defn- season-of [weeks]
  (transactions/normalize-season
   :sleeper
   {:league {:season "2025" :league_id 99 :settings {:waiver_budget 100}}
    :weeks  weeks}))

(defn- delete-tree [f]
  (when (.isDirectory f) (run! delete-tree (.listFiles f)))
  (.delete f))

(use-fixtures :each
  (fn [t]
    ;; Every test caches into a directory of its own, never the real `data/`.
    (let [dir (io/file (System/getProperty "java.io.tmpdir") (str "dd-bid-history-" (random-uuid)))]
      (try
        (with-redefs [transactions/cache-dir (str dir)] (t))
        (finally (delete-tree dir))))))

(deftest a-winner-and-the-bids-it-beat-are-one-auction
  (let [s (season-of {3 [(claim "complete" 2 "11560" 12 1000)
                         (claim "failed" 12 "11560" 0 1000)]})]
    (is (= [{:week 3 :at 1000 :player-id "11560"
             :bids [{:roster-id 2 :owner-id "u2" :amount 12 :won? true}
                    {:roster-id 12 :owner-id "u12" :amount 0 :won? false}]}]
           (:auctions s)))
    (is (= "2025" (:season s)))
    (is (= "99" (:league-id s)))
    (is (= 100 (:budget s)) "bids are later read as shares of it")
    (is (zero? (:non-competing s)))))

(deftest a-failed-claim-bidding-more-than-the-winner-never-competed
  (let [s (season-of {3 [(claim "complete" 2 "p" 5 1000)
                         (claim "failed" 12 "p" 20 1000)
                         (claim "failed" 7 "p" 3 1000)]})]
    (is (= [[2 5 true] [7 3 false]]
           (mapv (juxt :roster-id :amount :won?) (:bids (first (:auctions s)))))
        "the highest bid is processed first, so a failed $20 lost on a roster rule, not on price")
    (is (= 1 (:non-competing s)))))

(deftest a-claim-nobody-won-is-not-an-auction
  (let [s (season-of {3 [(claim "failed" 2 "p" 5 1000)]})]
    (is (empty? (:auctions s)))
    (is (= 1 (:non-competing s)) "left out, but counted")))

(deftest a-tie-the-winner-took-on-waiver-order-is-still-a-losing-bid
  (let [s (season-of {3 [(claim "complete" 2 "p" 5 1000)
                         (claim "failed" 7 "p" 5 1000)]})]
    (is (= [[2 true] [7 false]] (mapv (juxt :roster-id :won?) (:bids (first (:auctions s))))))))

(deftest a-claim-with-no-bid-on-it-is-a-zero-bid
  (let [s (season-of {3 [(assoc (claim "complete" 2 "p" 0 1000) :settings nil)]})]
    (is (= 0 (:amount (first (:bids (first (:auctions s)))))))))

(deftest free-agent-adds-trades-and-pending-claims-are-not-auctions
  (let [s (season-of {3 [(claim "complete" 2 "p" 0 1000 :type "free_agent")
                         (claim "complete" 7 "q" 0 1000 :type "trade")
                         (claim "pending" 12 "r" 9 nil)]})]
    (is (empty? (:auctions s)))
    (is (zero? (:non-competing s)) "none of them was a claim to leave out")))

(deftest the-same-player-in-two-runs-is-two-auctions
  (let [s (season-of {3 [(claim "complete" 7 "p" 2 2000)
                         (claim "complete" 2 "p" 9 1000)]})]
    (is (= [[1000 2] [2000 7]]
           (mapv (juxt :at (comp :roster-id first :bids)) (:auctions s)))
        "daily waivers: dropped after one run, claimed in the next, ordered by processing")))

(deftest a-managers-lower-claim-still-competes-when-his-higher-one-broke-a-rule
  (let [s (season-of {3 [(claim "complete" 2 "p" 5 1000)
                         (claim "failed" 12 "p" 20 1000)
                         (claim "failed" 12 "p" 4 1000)]})]
    (is (= [[2 5] [12 4]] (mapv (juxt :roster-id :amount) (:bids (first (:auctions s)))))
        "the $20 failed on a roster rule, the $4 lost on price")
    (is (zero? (:non-competing s)) "he was outbid, so he competed")))

(deftest one-runs-claims-are-one-auction-whatever-week-they-are-filed-under
  (let [s (season-of {3 [(claim "complete" 2 "p" 5 1000)]
                      4 [(claim "failed" 12 "p" 3 1000)]})]
    (is (= [[2 true] [12 false]] (mapv (juxt :roster-id :won?) (:bids (first (:auctions s))))))
    (is (zero? (:non-competing s)))))

(deftest two-claims-from-one-manager-are-one-bidder
  (let [s (season-of {3 [(claim "complete" 2 "p" 5 1000)
                         (claim "failed" 12 "p" 0 1000)
                         (claim "failed" 12 "p" 4 1000)]})]
    (is (= [[2 5] [12 4]] (mapv (juxt :roster-id :amount) (:bids (first (:auctions s))))))))

(deftest a-bid-belongs-to-the-manager-who-placed-it
  (let [s (season-of {3 [(claim "complete" 9 "p" 1 1000 :creator "u-before")]})]
    (is (= "u-before" (:owner-id (first (:bids (first (:auctions s))))))
        "not whoever holds roster 9 when the log is read")))

(deftest a-claim-with-no-creator-bids-with-no-owner
  (let [s (season-of {3 [(claim "complete" 9 "p" 1 1000 :creator nil)]})]
    (is (nil? (:owner-id (first (:bids (first (:auctions s)))))))))

(deftest a-season-says-whether-it-is-over-and-which-one-it-continues
  (let [raw (fn [league] {:league league :rosters [] :weeks {}})
        s   (transactions/normalize-season
             :sleeper (raw {:season "2026" :league_id 200 :status "in_season"
                            :previous_league_id 100 :settings {:waiver_budget 100}}))]
    (is (false? (:final? s)) "an unfinished season is refetched on every sync")
    (is (= {:league-id "100" :season "2025"} (:previous s)))
    (is (:final? (transactions/normalize-season :sleeper (raw {:season "2025" :status "complete"}))))
    (is (nil? (:previous (transactions/normalize-season
                          :sleeper (raw {:season "2025" :previous_league_id "0"}))))
        "\"0\" is Sleeper for no predecessor")))

(deftest ids-come-back-as-strings-and-a-missing-owner-stays-missing
  (let [out (transactions/normalized
             {:auctions [{:player-id 4034
                          :bids [{:roster-id 1 :owner-id 77 :amount 3 :won? true}
                                 {:roster-id 2 :owner-id nil :amount 0 :won? false}]}]})
        a   (first (:auctions out))]
    (is (= "4034" (:player-id a)))
    (is (= ["77" nil] (mapv :owner-id (:bids a))) "(str nil) would be an owner called \"\"")))

(deftest the-season-and-league-ids-come-back-as-strings-too
  ;; ESPN publishes both as integers, and both are half of a cache path.
  (let [out (transactions/normalized {:league-id 123 :season 2026
                                      :previous {:league-id 123 :season 2025}
                                      :auctions []})]
    (is (= ["123" "2026"] ((juxt :league-id :season) out)))
    (is (= {:league-id "123" :season "2025"} (:previous out)))
    (is (nil? (:previous (transactions/normalized {:previous nil :auctions []}))))))

(def ^:private leagues
  {"200" {:league_id "200" :season "2026" :status "in_season" :previous_league_id "100"
          :settings {:leg 3 :waiver_type 2 :waiver_budget 100}}
   "100" {:league_id "100" :season "2025" :status "complete" :previous_league_id "0"
          :settings {:leg 2 :waiver_type 2 :waiver_budget 100}}})

(defn- stubbed
  "Run `f` against a fake Sleeper serving `leagues`, recording every path asked
  for. `fail` maps a path to the ex-info its fetch throws instead."
  ([f] (stubbed {} f))
  ([fail f]
   (let [asked (atom #{})
         serve (fn [path v]
                 (swap! asked conj path)
                 (if-let [e (get fail path)] (throw e) v))]
     (with-redefs [import-sleeper/fetch-league (fn [id] (serve (str "league/" id) (get leagues id)))
                   sync-sleeper/get-json       (fn [path _] (serve path []))]
       (f asked)))))

(defn- history [] (transactions/bid-history {:provider "sleeper" :league-id "200" :season "2026"}))

(deftest every-week-played-is-asked-for-and-the-season-before
  (stubbed
   (fn [asked]
     (let [out (history)]
       (is (:ok out))
       (is (= ["2026" "2025"] (mapv :season (get-in out [:history :seasons]))))
       (is (every? @asked ["league/200/transactions/1" "league/200/transactions/2"
                           "league/200/transactions/3" "league/100/transactions/1"
                           "league/100/transactions/2"]))
       (is (not (@asked "league/200/transactions/4")) "no week the league has not reached")))))

(deftest the-browser-is-told-how-much-history-there-is-not-handed-it
  (with-redefs [sync-sleeper/get-json
                (fn [path _]
                  (if (= path "league/200/transactions/1")
                    [{:type "waiver" :status "complete" :status_updated 1000
                      :adds {:p1 2} :settings {:waiver_bid 5}}
                     {:type "waiver" :status "failed" :status_updated 1000
                      :adds {:p1 7} :settings {:waiver_bid 1}}]
                    []))
                import-sleeper/fetch-league (fn [id] (get leagues id))]
    (let [[now before] (get-in (history) [:history :seasons])]
      (is (= {:season "2026" :auctions 1 :contested 1 :non-competing 0}
             (dissoc now :fetched-at)))
      (is (string? (:fetched-at now)))
      (is (= 0 (:auctions before)))
      (is (not (contains? now :bids)) "the auctions stay on the server"))))

(deftest a-finished-season-is-read-from-the-cache-rather-than-fetched-again
  (stubbed (fn [_] (history)))
  (stubbed
   (fn [asked]
     (is (:ok (history)))
     (is (@asked "league/200/transactions/3") "the current season is always refreshed")
     (is (not-any? #(re-find #"league/100" %) @asked)
         "a completed season cannot change, so it costs nothing after the first sync"))))

(deftest an-unfinished-previous-season-is-fetched-again
  (with-redefs [leagues (assoc-in leagues ["100" :status] "in_season")]
    (stubbed (fn [_] (history)))
    (stubbed (fn [asked]
               (history)
               (is (@asked "league/100/transactions/1"))))))

(deftest the-cache-hands-the-waiver-board-both-seasons
  (stubbed (fn [_] (history)))
  (let [{:keys [seasons]} (transactions/cached-history "sleeper" "200" "2026")]
    (is (= ["2026" "2025"] (mapv :season seasons)))
    (is (every? vector? (map :auctions seasons)) "the full auctions, which never cross the wire"))
  (is (nil? (transactions/cached-history :sleeper "999" "2026")) "never synced is nothing"))

(deftest a-cache-lookup-with-ids-no-league-could-have-is-nothing
  ;; The waiver board looks these up with ids off a request body.
  (is (nil? (transactions/cache-path :sleeper "../../etc" "2026")))
  (is (nil? (transactions/cache-path :sleeper "200" "2026/../x")))
  (is (nil? (transactions/cached-history :sleeper "../200" "2026"))))

(deftest a-season-without-faab-is-not-asked-for-its-log
  (with-redefs [leagues (assoc-in leagues ["100" :settings :waiver_type] 0)]
    (stubbed
     (fn [asked]
       (let [[_ before] (get-in (history) [:history :seasons])]
         (is (= 0 (:auctions before)) "the season is still there, with no auctions")
         (is (not-any? #(re-find #"league/100/" %) @asked)
             "its claims carry no bid — read as auctions they are all $0"))))))

(deftest a-predecessor-sleeper-no-longer-knows-is-no-predecessor
  (stubbed {"league/100" (ex-info "Sleeper league not found" {:status 404})}
           (fn [_]
             (is (= ["2026"] (mapv :season (get-in (history) [:history :seasons])))))))

(deftest a-week-that-will-not-load-fails-the-history-rather-than-thinning-it
  (stubbed {"league/200/transactions/2" (ex-info "Sleeper non-200" {:status 502})}
           (fn [_]
             (let [out (history)]
               (is (false? (:ok out)) "a history missing a week reads like a league that did not bid")
               (is (= 502 (:status out)) "the status survives the concurrent fetch")
               (is (nil? (:cached out)) "nothing was ever cached to fall back on")))))

(deftest a-refresh-that-fails-keeps-the-last-good-cache-and-says-what-it-holds
  (stubbed (fn [_] (history)))
  (stubbed {"league/200/transactions/2" (ex-info "Sleeper non-200" {:status 502})}
           (fn [_]
             (let [out (history)]
               (is (false? (:ok out)))
               (is (= ["2026" "2025"] (mapv :season (get-in out [:cached :seasons])))
                   "the board still prices from the last good copy, and the sync says so")
               (is (some? (transactions/cached-history :sleeper "200" "2026"))
                   "the failed refresh did not overwrite it")))))

(deftest a-predecessor-that-breaks-rather-than-vanishes-fails-too
  (with-redefs [leagues (assoc-in leagues ["100" :status] "in_season")]
    (stubbed {"league/100/transactions/1" (ex-info "Sleeper non-200" {:status 502})}
             (fn [_]
               (is (= 502 (:status (history))))))))

(deftest a-finished-current-season-is-read-from-the-cache-too
  (with-redefs [leagues (assoc-in leagues ["200" :status] "complete")]
    (stubbed (fn [_] (history)))
    (stubbed (fn [asked]
               (is (= ["2026" "2025"] (mapv :season (get-in (history) [:history :seasons]))))
               (is (empty? @asked) "neither season can change, so a sync asks for nothing")))))

(deftest a-predecessor-is-cached-under-the-season-its-own-document-states
  ;; Sleeper's link only guesses the year before; a league re-created mid-season
  ;; is the case where the guess is wrong.
  (with-redefs [leagues (assoc-in leagues ["100" :season] "2026")]
    (stubbed (fn [_] (history)))
    (is (= ["2026" "2026"] (mapv :season (:seasons (transactions/cached-history :sleeper "200" "2026"))))
        "the waiver board finds it")
    (stubbed (fn [asked]
               (history)
               (is (not-any? #(re-find #"league/100" %) @asked)
                   "and a finished predecessor is not fetched again under the guess")))))

(deftest an-error-in-the-predecessor-is-a-failed-history-not-a-thrown-one
  (with-redefs [leagues (assoc-in leagues ["100" :status] "in_season")]
    (stubbed {"league/100/transactions/1" (StackOverflowError.)}
             (fn [_]
               (let [out (history)]
                 (is (false? (:ok out)) "bid-history promises never to throw")
                 (is (string? (:error out))))))))

(deftest a-cache-write-that-fails-leaves-no-temporary-file
  (with-redefs [pipeline/write-transit! (fn [path _] (spit path "partial") (throw (ex-info "disk full" {})))]
    (is (thrown? Exception (transactions/write-season! :sleeper {:league-id "200" :season "2026"})))
    (is (empty? (.listFiles (io/file transactions/cache-dir))))))

(deftest a-league-id-that-is-not-one-is-refused-before-any-fetch
  (stubbed
   (fn [asked]
     (is (= 400 (:status (transactions/bid-history {:provider "sleeper" :league-id "../x"}))))
     (is (empty? @asked)))))

(deftest a-host-with-no-bid-history-is-not-asked
  (stubbed
   (fn [asked]
     (is (= {:ok false :unsupported? true}
            (transactions/bid-history {:provider "espn" :league-id "123"})))
     (is (empty? @asked)))))

(deftest the-weeks-played-run-through-the-current-one
  (is (= [1 2 3] (tx-sleeper/weeks-played {:settings {:leg 3}})))
  (is (empty? (tx-sleeper/weeks-played {:settings {}})) "preseason has no weeks"))
