(ns draft-day.ingestion.pipeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [draft-day.ingestion.espn :as espn]
            [draft-day.ingestion.espn-schedule :as espn-schedule]
            [draft-day.ingestion.fantasypros :as fantasypros]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.nflverse-weekly :as nflverse-weekly]
            [draft-day.ingestion.pipeline :as pipeline :refer [apply-enrichment]]
            [draft-day.ingestion.player-ids :as player-ids]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.scoring :as scoring]))

(defn- tmp [name] (str (System/getProperty "java.io.tmpdir") "/dd-" name ".transit"))

(defn universe-fixture
  "A universe big enough to clear validate's systemic-failure floor, so the
  chain tests exercise the real gate rather than a disabled one."
  [n]
  (mapv (fn [i] {:player-id (str "p" i) :player-name (str "Player " i)
                 :position "RB" :stats {} :sleeper/pts-ppr 100.0})
        (range n)))

(deftest transit-roundtrip-and-freshness
  (let [path (tmp "roundtrip")
        data [{:player-id "a" :position "RB" :stats {:rush_yd 100.0} :sleeper/adp 1.4}]]
    (pipeline/write-transit! path data)
    (is (= data (pipeline/read-transit path)))       ; namespaced keys survive
    (is (pipeline/cache-fresh? path 24))
    (.delete (io/file path))
    (is (not (pipeline/cache-fresh? path 24)))))     ; missing file -> not fresh

(deftest delete-cache-removes-the-file-and-is-idempotent
  (let [path (tmp "delete")]
    (pipeline/write-transit! path {:players []})
    (is (.exists (io/file path)))
    (pipeline/delete-cache! path)
    (is (not (.exists (io/file path))))
    (is (nil? (pipeline/delete-cache! path)))))

(deftest bundled-sample-loads
  (let [{:keys [players]} (pipeline/cached->universe (pipeline/load-sample))]
    (is (seq players))
    (is (every? :player-id players))))

(deftest sample-universe-reads-both-shapes
  (testing "a stamped sample reports its own provenance"
    (with-redefs [pipeline/load-sample
                  (constantly {:schema-version 1
                               :season 2026
                               :captured-at "2026-08-09T00:00:00Z"
                               :sources {:espn {:ok? true}}
                               :players [{:player-id "99999999"}]})]
      (let [u (pipeline/sample-universe)]
        (is (= "sample" (:source u)))
        (is (= 2026 (:season u)))
        (is (= "2026-08-09T00:00:00Z" (:fetched-at u)))
        (is (= {:espn {:ok? true}} (:sources u)))
        (is (= [{:player-id "99999999" :ids {:sleeper "99999999"}}]
               (:players u))
            "an id absent from the snapshot is left exactly as it was"))))

  (testing "the legacy bare vector admits it has no provenance"
    (with-redefs [pipeline/load-sample (constantly [{:player-id "99999999"}])]
      (let [u (pipeline/sample-universe)]
        (is (= "sample" (:source u)))
        (is (= 0 (:schema-version u)) "schema 0 — it predates versioning")
        (is (nil? (:season u)))
        (is (nil? (:fetched-at u)))
        (is (= [{:player-id "99999999" :ids {:sleeper "99999999"}}]
               (:players u))
            "an id absent from the snapshot is left exactly as it was"))))

  (testing "a missing sample degrades to an empty universe, not an exception"
    (with-redefs [pipeline/load-sample (constantly nil)]
      (is (= [] (:players (pipeline/sample-universe)))))))

(deftest sample-claims-match-its-contents
  ;; Guards the failure that produced this work: an enrichment source is added
  ;; to the pipeline, the fixture is never recaptured, and its column renders
  ;; blank offline forever with nothing to say the column is absent by
  ;; construction. Once the sample is recaptured with a stamp, any source it
  ;; claims must actually be present in the rows.
  ;;
  ;; The format split silently disarmed this once already: the lookup still
  ;; keyed on the pre-split `:fantasypros/ecr`, so every format-scoped label
  ;; resolved to nil and was skipped by the `:when`, and the columns had moved
  ;; under :vendor/by-format where the old accessor could not have seen them
  ;; anyway. Hence the last assertion — an unrecognized label now fails the test
  ;; instead of quietly excusing itself from it.
  (let [{:keys [players sources]} (pipeline/cached->universe
                                   (pipeline/load-sample))
        ;; The per-position expert tiers land on both sides of this split, which
        ;; is the point of `fantasypros/pos-formats`: RB/WR/TE publish a page per
        ;; scoring format and so are scoped, while QB/K/DST publish one page for
        ;; all three and join flat, exactly as ESPN does.
        ;; A label may name more than one column. ESPN earns that: its auction
        ;; value and its projected usage come from the same response but from
        ;; different corners of it, and the projections shipped empty once
        ;; already — the stat ids decode to keywords, not strings — while the
        ;; auction value kept the label looking healthy.
        flat    (into {:fantasypros/sleepers [:fantasypros/sleeper?]
                       :espn                 [:espn/auction-value :espn/proj-targets]
                       :nflverse/player-stats [:nflverse/prior-targets
                                               :nflverse/prior-target-share]
                       :sleeper/byes         [:bye]}
                      (keep (fn [[label pos _]]
                              (when-not (get fantasypros/pos-formats pos)
                                [label [:fantasypros/ecr-pos-tier]])))
                      pipeline/pos-tier-tasks)
        scoped  (into (into {}
                            (mapcat (fn [fmt]
                                      [[(pipeline/format-label :fantasypros/ecr fmt)
                                        [fmt :fantasypros/ecr]]
                                       [(pipeline/format-label :fantasypros/aav fmt)
                                        [fmt :fantasypros/aav]]]))
                            scoring/formats)
                      (keep (fn [[label pos fmt]]
                              (when (get fantasypros/pos-formats pos)
                                [label [fmt :fantasypros/ecr-pos-tier]])))
                      pipeline/pos-tier-tasks)]
    (is (seq players))
    (is (empty? (remove (some-fn flat scoped) (keys sources)))
        "a source label this test does not know about means the guard has drifted")
    (doseq [[label {:keys [ok?]}] sources
            :when ok?]
      (if-let [ks (get flat label)]
        (doseq [k ks]
          (is (some k players)
              (format "sample claims %s but no row carries %s" label k)))
        (when-let [[fmt col] (get scoped label)]
          (is (some #(get-in % [:vendor/by-format fmt col]) players)
              (format "sample claims %s but no row carries %s under [:vendor/by-format %s]"
                      label col fmt)))))))

(deftest resolution-chain
  ;; `offline?` reads DRAFTDAY_OFFLINE at call time, so without this the whole
  ;; chain short-circuits to the sample in any shell that exports it — which
  ;; CLAUDE.md recommends for dev.
  (with-redefs [pipeline/offline? (constantly false)
                ;; The chain is about which source wins, not about enrichment;
                ;; stubbing it keeps the test off the network entirely.
                pipeline/enrich-universe (fn [_season universe] {:players universe :sources {}})]
    (let [path    (tmp "chain")
          fixture (universe-fixture 120)]
      (.delete (io/file path))
      ;; live success writes the cache
      (with-redefs [sleeper/fetch-universe (fn [& _] fixture)]
        (let [r (pipeline/load-universe {:refresh true :cache-path path})]
          (is (= "live" (:source r)))
          (is (= (mapv :player-id fixture) (mapv :player-id (:players r))))
          (is (every? :ids (:players r)) "anchoring attaches the crosswalk")))
      ;; fetch down -> stale cache preferred over sample
      (with-redefs [sleeper/fetch-universe (fn [& _] (throw (ex-info "down" {})))]
        (let [r (pipeline/load-universe {:refresh true :cache-path path})]
          (is (= "cache" (:source r)))
          (is (= (mapv :player-id fixture) (mapv :player-id (:players r))))))
      ;; fetch down + no cache -> bundled sample
      (.delete (io/file path))
      (with-redefs [sleeper/fetch-universe (fn [& _] (throw (ex-info "down" {})))]
        (let [r (pipeline/load-universe {:refresh true :cache-path path})]
          (is (= "sample" (:source r)))
          (is (seq (:players r))))))))

(deftest universe-is-stamped-with-its-provenance
  (with-redefs [pipeline/offline? (constantly false)
                pipeline/enrich-universe (fn [_ u] {:players u :sources {}})
                pipeline/now-iso (constantly "2026-08-09T12:00:00Z")
                sleeper/fetch-universe (fn [& _] (universe-fixture 120))]
    (let [path (tmp "stamp")]
      (.delete (io/file path))
      (let [r (pipeline/load-universe {:refresh true :cache-path path
                                       :season 2026})]
        (is (= pipeline/schema-version (:schema-version r)))
        (is (= 2026 (:season r)))
        (is (= "2026-08-09T12:00:00Z" (:fetched-at r)))
        (is (= "live" (:source r)))
        (is (= 120 (get-in r [:validation :kept]))))

      (testing "a cached read reports when it was *fetched*, not read"
        (with-redefs [pipeline/now-iso (constantly "2027-01-01T00:00:00Z")]
          (let [r (pipeline/load-universe {:cache-path path})]
            (is (= "cache" (:source r)))
            (is (= "2026-08-09T12:00:00Z" (:fetched-at r))
                "provenance travels with the data, not the read")
            (is (= 2026 (:season r)))))))))

(deftest cached-universe-rejects-a-foreign-schema
  (let [path (tmp "schema")]
    (testing "a bare pre-versioning vector is schema 0, so it is not reused"
      (pipeline/write-transit! path (universe-fixture 120))
      (is (= {:schema-version 0 :players (universe-fixture 120)}
             (pipeline/cached->universe (pipeline/read-transit path))))
      (is (nil? (pipeline/cached-universe path))))

    (testing "a future schema is refused rather than read as missing columns"
      (pipeline/write-transit! path {:schema-version 999
                                     :players (universe-fixture 120)})
      (is (nil? (pipeline/cached-universe path))))

    (testing "a cache that validates down to nothing is not served"
      (pipeline/write-transit! path {:schema-version pipeline/schema-version
                                     :players [{:player-id nil}]})
      (is (nil? (pipeline/cached-universe path))))

    (.delete (io/file path))
    (is (nil? (pipeline/cached-universe path)))))

(deftest a-stale-cache-still-beats-the-sample
  (with-redefs [pipeline/offline? (constantly false)
                pipeline/enrich-universe (fn [_ u] {:players u :sources {}})]
    (let [path    (tmp "stale")
          fixture (universe-fixture 120)]
      (.delete (io/file path))
      (with-redefs [sleeper/fetch-universe (fn [& _] fixture)]
        (pipeline/load-universe {:refresh true :cache-path path :season 2025}))
      (with-redefs [sleeper/fetch-universe (fn [& _] (throw (ex-info "down" {})))]
        (let [r (pipeline/load-universe {:refresh true :cache-path path})]
          (is (= "cache" (:source r)))
          (is (= 2025 (:season r))
              "the stale season is visible, so the board can say it is old"))))))

(deftest offline-short-circuits-to-the-sample
  (with-redefs [pipeline/offline? (constantly true)
                sleeper/fetch-universe (fn [& _]
                                         (throw (AssertionError. "no network offline")))]
    (let [r (pipeline/load-universe {:refresh true :cache-path (tmp "unused")})]
      (is (= "sample" (:source r)))
      (is (seq (:players r))))))

(deftest every-universe-id-belongs-to-exactly-one-space
  ;; The invariant that lets :player-id mix GSIS ids, team abbreviations and
  ;; Sleeper fallbacks without prefixing any of them. Runs against the real
  ;; bundled universe, which is how the one malformed GSIS id was found.
  (let [players (:players (pipeline/sample-universe))]
    (is (seq players))
    (doseq [p players]
      (is (player-ids/id-space (:player-id p))
          (format "%s (%s) has id %s, which is in no known id space"
                  (:player-name p) (:position p) (pr-str (:player-id p)))))
    (is (every? player-ids/anchor-consistent? players)
        "player-id must always equal the best member of its own :ids envelope")
    (is (= (count players) (count (distinct (map :player-id players))))
        "anchoring must not collide two players onto one id")))

(deftest every-enrichment-fetch-goes-out-together
  ;; Twenty-three independent fetches behind a 30-second timeout each, on the
  ;; request thread that missed the cache. Awaited in turn they stack to ten
  ;; minutes, so what has to hold is that `enrich-universe` starts *all* of them
  ;; before it blocks on any — not merely that some helper can start six.
  ;;
  ;; Testing the helper alone was not enough: hoisting the deref above the other
  ;; fetches re-serializes the whole thing and a helper-level test stays green.
  ;; Every stub here parks until all of them have arrived, so any fetch left on
  ;; the sequential path deadlocks its own wait and reports unavailable.
  (let [expected (count (pipeline/enrichment-tasks 2026))
        latch    (java.util.concurrent.CountDownLatch. expected)
        arrive!  (fn [what]
                   (.countDown latch)
                   (when-not (.await latch 10 java.util.concurrent.TimeUnit/SECONDS)
                     (throw (ex-info "this fetch ran on its own" {:fetch what}))))
        rows     (fn [k] [{:key k :fantasypros/ecr 1}])]
    (is (= 23 expected)
        "three formats x (ECR + AAV), 12 per-position tier pages, plus byes, sleepers, ESPN, and nflverse's prior-season and in-season files")
    (with-redefs [sleeper/fetch-byes    (fn [_] (arrive! :byes) {"ATL" 5})
                  fantasypros/fetch-sleepers (fn [] (arrive! :sleepers)
                                               (rows "player0_rb"))
                  espn/fetch            (fn [_] (arrive! :espn)
                                          {"player0_rb" {:espn/auction-value 1.0}})
                  nflverse/fetch        (fn [_] (arrive! :nflverse)
                                          {:by-key    {"00-0000000" {:nflverse/prior-targets 1.0}}
                                           :positions {"00-0000000" "RB"}})
                  nflverse-weekly/fetch (fn [_] (arrive! :nflverse-weekly)
                                          {:by-key    {"00-0000000"
                                                       {:nflverse/season-to-date
                                                        {:games 4 :stats {:rec 20.0}}}}
                                           :through-week 4
                                           :positions {"00-0000000" "RB"}})
                  fantasypros/fetch-ecr (fn [fmt] (arrive! [:ecr fmt])
                                          (rows "player0_rb"))
                  fantasypros/fetch-aav (fn [fmt] (arrive! [:aav fmt])
                                          (rows "player0_rb"))
                  fantasypros/fetch-pos-ecr (fn [pos fmt] (arrive! [:pos-tier pos fmt])
                                              [{:key "player0_rb"
                                                :fantasypros/ecr-pos-tier 1}])]
      (let [{:keys [sources]} (pipeline/enrich-universe 2026 (universe-fixture 5))]
        (is (= (set pipeline/enrichment-source-labels) (set (keys sources)))
            "every source still reports, and under its own label")
        (is (every? :ok? (vals sources))
            "all of them resolved, which only happens if all were in flight")))))

(deftest a-fetch-that-throws-costs-only-its-own-column
  ;; `parallel/all` must not let one thrower take the rest down — that is the
  ;; whole contract `best-effort` had when the fetches were sequential.
  (with-redefs [sleeper/fetch-byes    (fn [_] {"ATL" 5})
                fantasypros/fetch-sleepers (fn [] nil)
                nflverse/fetch        (fn [_] {:by-key {} :positions {}})
                nflverse-weekly/fetch (fn [_] {:by-key {} :through-week 0 :positions {}})
                espn/fetch            (fn [_] (throw (ex-info "espn down" {})))
                fantasypros/fetch-ecr (fn [fmt]
                                        (if (= :standard fmt)
                                          (throw (ex-info "scrape blew up" {}))
                                          [{:key "player0_rb" :fantasypros/ecr 1}]))
                fantasypros/fetch-aav (fn [_] [{:key "player0_rb" :fantasypros/aav 3.0}])
                ;; `pos-tier-tasks` is a dozen more scrapes; unstubbed they go to
                ;; the wire and `best-effort` swallows the evidence.
                fantasypros/fetch-pos-ecr (fn [_ _] nil)]
    (let [{:keys [sources]} (pipeline/enrich-universe 2026 (universe-fixture 5))]
      (is (false? (:ok? (get sources :espn))))
      (is (false? (:ok? (get sources (pipeline/format-label :fantasypros/ecr :standard)))))
      (is (true? (:ok? (get sources (pipeline/format-label :fantasypros/ecr :ppr))))
          "a sibling format's scrape is unaffected")
      (is (every? #(true? (:ok? (get sources (pipeline/format-label :fantasypros/aav %))))
                  scoring/formats)))))

(deftest a-source-whose-rows-cannot-all-land-says-so-in-its-report
  ;; The per-position hit-rate warning exists to say "a join that should land is
  ;; not landing". A prior-season source can never land every row — last year's
  ;; retirees have no row on this year's board — so it opts out rather than
  ;; teaching the reader to scroll past warnings.
  (let [universe [{:player-name "A" :position "RB" :ids {:gsis "00-0000001"}}]
        join     (fn [opts]
                   (-> (apply-enrichment {:players universe :sources {}}
                                         :src
                                         {"00-0000001" {:x 1} "00-0000002" {:x 2}}
                                         (merge {:key-fn #(get-in % [:ids :gsis])} opts))
                       (get-in [:sources :src])))]
    (is (true? (:expected-partial? (join {:expected-partial? true}))))
    (is (false? (:expected-partial? (join {})))
        "the default stays off, so every other source keeps the warning")
    (testing "opting out changes only the reporting, never the join"
      (is (= (dissoc (join {:expected-partial? true}) :expected-partial?)
             (dissoc (join {}) :expected-partial?))))))

;; ---- weekly projections cache ----

(def ^:private weekly-lines
  {"9509" {:stats {:rush_yd 81.0 :rec 3.8} :opponent "TB" :home? true
           :updated-at 1788755441199}})

(deftest weekly-cache-answers-only-for-its-own-week
  ;; A week rollover invalidates regardless of TTL: a fresh file for last week
  ;; is not a fresh file.
  (let [env {:schema-version pipeline/weekly-schema-version
             :season 2026 :week 5 :lines weekly-lines}]
    (is (true?  (pipeline/weekly-answers? env 2026 5)))
    (is (false? (pipeline/weekly-answers? env 2026 6)))
    (is (false? (pipeline/weekly-answers? env 2025 5)))
    (is (false? (pipeline/weekly-answers? (assoc env :schema-version 0) 2026 5)))
    (is (false? (pipeline/weekly-answers? nil 2026 5)))
    ;; "Nobody is projected" IS an answer. Treating it as no answer is what made
    ;; the offseason refetch on every request.
    (is (true?  (pipeline/weekly-answers? (assoc env :lines {}) 2026 5)))))

(deftest weekly-past-the-regular-season-is-cached-and-reported-as-nothing
  ;; Sleeper serves week 19 with a 200 and a full page of *unprojected* entries
  ;; rather than a 404, so this is the normal offseason reply. Two things must
  ;; hold: the board is told there is no week (or the banner announces one that
  ;; does not exist), and the empty answer still caches (or every request
  ;; refetches, forever).
  (with-redefs [pipeline/offline? (constantly false)]
    (let [path  (tmp "weekly-empty")
          calls (atom 0)]
      (pipeline/delete-cache! path)
      (with-redefs [sleeper/fetch-weekly (fn [& _] (swap! calls inc) {})
                    espn-schedule/fetch (constantly nil)]
        (is (nil? (pipeline/load-weekly 2026 19 {:path path})))
        (is (nil? (pipeline/load-weekly 2026 19 {:path path})))
        (is (= 1 @calls) "the empty answer was cached, not refetched")))))

(deftest weekly-join-crosses-the-two-id-spaces
  ;; The lines arrive in Sleeper's id space; :player-id is the GSIS id wherever
  ;; one resolves. Joining them directly matches only the players who have no
  ;; crosswalk entry, which reads as a thin vendor rather than as a bug — the
  ;; same failure `waiver/held-ids` exists to prevent.
  (let [players [{:player-id "00-0036223" :ids {:sleeper "9509"}}    ; crosswalked
                 {:player-id "ARI"}                                  ; DST: same in both
                 {:player-id "00-0031234" :ids {:sleeper "4034"}}]   ; not projected
        lines   (assoc weekly-lines "ARI" {:stats {:sack 2.4} :opponent "LAC"
                                           :home? false :updated-at 1})
        [a d b] (pipeline/assoc-weekly players lines)]
    (is (= "TB" (:week/opponent a)))
    (is (= "LAC" (:week/opponent d)))
    (is (not (contains? b :week/stats)))))

(deftest weekly-join-leaves-unprojected-players-untouched
  ;; No key at all rather than a zero — a bye and a projected nothing are
  ;; different answers, and only absence can say the first.
  (let [[a b] (pipeline/assoc-weekly [{:player-id "9509"} {:player-id "4034"}]
                                     weekly-lines)]
    (is (= "TB" (:week/opponent a)))
    (is (true? (:week/home? a)))
    (is (= {:rush_yd 81.0 :rec 3.8} (:week/stats a)))
    (is (= 1788755441199 (:week/updated-at a)))
    (is (= {:player-id "4034"} b))
    (is (not (contains? b :week/stats)))))

(deftest weekly-join-is-a-no-op-with-no-lines
  ;; The weekly asset does not exist until week 1 is played, so every consumer
  ;; has to work with these columns entirely absent.
  (is (= [{:player-id "1"}] (pipeline/assoc-weekly [{:player-id "1"}] nil))))

(deftest weekly-offline-fetches-nothing
  (with-redefs [pipeline/offline? (constantly true)]
    (is (nil? (pipeline/load-weekly 2026 1 {:path (tmp "weekly-offline")})))))

(deftest weekly-falls-back-to-the-stale-copy-for-the-same-week
  (with-redefs [pipeline/offline? (constantly false)]
    (let [path (tmp "weekly-stale")]
      (pipeline/delete-cache! path)
      (with-redefs [sleeper/fetch-weekly (fn [& _] weekly-lines)
                    espn-schedule/fetch (constantly nil)]
        (is (= weekly-lines (:lines (pipeline/load-weekly 2026 5 {:refresh true :path path})))))
      ;; Sleeper down: the cached week still answers …
      (with-redefs [sleeper/fetch-weekly (fn [& _] (throw (ex-info "down" {})))
                    espn-schedule/fetch (constantly nil)]
        (is (= weekly-lines (:lines (pipeline/load-weekly 2026 5 {:refresh true :path path}))))
        ;; … but not for a different week, where it would be simply wrong.
        (is (nil? (pipeline/load-weekly 2026 6 {:refresh true :path path})))))))

(deftest weekly-fresh-cache-is-not-re-read-per-call
  ;; The decoded envelope is held in memory for the same reason `api.routes`
  ;; holds the universe: a fresh file should not be re-read and re-decoded on
  ;; every /api/waivers. It must still expire with the file, which the mtime
  ;; and season/week checks — not the memo — decide.
  (with-redefs [pipeline/offline? (constantly false)]
    (let [path  (tmp "weekly-memo")
          reads (atom 0)]
      (pipeline/delete-cache! path)
      (with-redefs [sleeper/fetch-weekly (fn [& _] weekly-lines)
                    espn-schedule/fetch (constantly nil)]
        (pipeline/load-weekly 2026 5 {:refresh true :path path}))
      (with-redefs [pipeline/read-transit (fn [p] (swap! reads inc) nil)]
        (is (= weekly-lines (:lines (pipeline/load-weekly 2026 5 {:path path}))))
        (is (= weekly-lines (:lines (pipeline/load-weekly 2026 5 {:path path}))))
        (is (zero? @reads) "a fresh, already-decoded week was served from memory")
        ;; A different week is a different answer, memo or not.
        (with-redefs [sleeper/fetch-weekly (fn [& _] {})
                      espn-schedule/fetch (constantly nil)]
          (is (nil? (pipeline/load-weekly 2026 6 {:path path})))
          (is (pos? @reads) "a week the memo cannot answer for still reads disk"))))))

;; ---- kickoff times ----

(def ^:private kickoffs
  {"KC"  {:kickoff "2026-09-13T17:00Z" :status "STATUS_SCHEDULED"
          :detail "9/13 - 1:00 PM EDT" :venue "Arrowhead Stadium"
          :opponent "LV" :home? true :neutral? false}
   "LV"  {:kickoff "2026-09-13T17:00Z" :status "STATUS_SCHEDULED"
          :detail "9/13 - 1:00 PM EDT" :venue "Arrowhead Stadium"
          :opponent "KC" :home? false :neutral? false}})

(deftest a-kickoff-reaches-a-player-with-no-weekly-projection
  ;; The whole reason this join is separate from `assoc-weekly`.
  (let [[p] (pipeline/assoc-kickoffs [{:player-id "x" :team "KC"}] kickoffs)]
    (is (= "2026-09-13T17:00Z" (:kickoff/at p)))
    (is (= "LV" (:kickoff/opponent p)))
    (is (true? (:kickoff/home? p)))
    (is (nil? (:week-points p)) "and it did not invent a projection")))

(deftest a-team-on-bye-gets-no-kickoff-keys-at-all
  ;; Absent, not nil-valued: "no game" and "no kickoff data" are different.
  (let [[p] (pipeline/assoc-kickoffs [{:player-id "x" :team "BUF"}] kickoffs)]
    (is (= {:player-id "x" :team "BUF"} p))))

(deftest a-player-with-no-team-is-left-alone
  (let [[p] (pipeline/assoc-kickoffs [{:player-id "x"}] kickoffs)]
    (is (= {:player-id "x"} p))))

(deftest no-kickoffs-at-all-is-a-no-op
  (let [players [{:player-id "x" :team "KC"}]]
    (is (= players (pipeline/assoc-kickoffs players {})))
    (is (= players (pipeline/assoc-kickoffs players nil)))))

(deftest a-scoreboard-that-is-down-does-not-cost-the-weekly-projection
  ;; The envelope is still written and the board still prices the week.
  (with-redefs [pipeline/offline?       (constantly false)
                sleeper/fetch-weekly    (fn [& _] weekly-lines)
                espn-schedule/fetch     (fn [& _] nil)]
    (let [env (pipeline/load-weekly 2026 5 {:refresh true :path (tmp "weekly-no-espn")})]
      (is (= weekly-lines (:lines env)))
      (is (= {} (:kickoffs env)) "empty, not nil, so the shape is stable"))))

(deftest a-schema-1-weekly-file-is-never-read-back
  ;; It carries no kickoffs, which is indistinguishable from a failed fetch.
  (let [env {:schema-version 1 :season 2026 :week 5 :lines weekly-lines}]
    (is (false? (pipeline/weekly-answers? env 2026 5)))))
