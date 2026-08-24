(ns draft-day.ingestion.merge-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.merge :as merge]
            [draft-day.ingestion.match :as match]))

(deftest normalize-collapses-punctuation-and-suffixes
  (is (= (match/key-for "T.J. Hockenson" "TE") (match/key-for "TJ Hockenson" "TE")))
  (is (= "bijanrobinson_rb" (match/key-for "Bijan Robinson" "RB")))
  (is (= (match/key-for "Michael Pittman Jr." "WR") (match/key-for "Michael Pittman" "WR"))))

(deftest left-join-attaches-columns-only
  (let [universe   [{:player-id "9509" :player-name "Bijan Robinson" :position "RB"}
                    {:player-id "z" :player-name "Unmatched Guy" :position "WR"}]
        enrichment {(match/key-for "Bijan Robinson" "RB") {:fantasypros/ecr 2 :bye 11}}
        joined     (merge/left-join universe enrichment)]
    (is (= 2 (count joined)))                        ; universe rows preserved (no rows added/removed)
    (is (= 2 (:fantasypros/ecr (first joined))))     ; matched row gets columns
    (is (= 11 (:bye (first joined))))
    (is (nil? (:fantasypros/ecr (second joined)))))) ; unmatched row unchanged

(deftest left-join-report-counts-what-the-join-accomplished
  (let [universe   [{:player-name "Bijan Robinson" :position "RB"}
                    {:player-name "Unmatched Guy" :position "WR"}]
        enrichment {(match/key-for "Bijan Robinson" "RB") {:fantasypros/ecr 2}
                    (match/key-for "Nobody Here" "TE")    {:fantasypros/ecr 9}}
        {:keys [players report]} (merge/left-join-report universe enrichment)]
    (testing "rows are joined exactly as left-join does"
      (is (= players (merge/left-join universe enrichment))))

    (testing "counts cover both sides of the join"
      (is (= 2 (:rows report)) "what the source published")
      (is (= 1 (:matched report)) "universe players that gained columns")
      (is (= 0.5 (:coverage report)))
      (is (= 0.5 (:hit-rate report))))

    (testing "enrichment rows with no universe home are sampled by name"
      (is (= [(match/key-for "Nobody Here" "TE")]
             (:unmatched-sample report))))))

(deftest left-join-report-denominates-by-position-on-the-universe
  (testing "a position matched by nothing reads as 0-of-N, not as absent"
    (let [universe (into (mapv (fn [t] {:player-name (str t " Defense")
                                        :position "DST"})
                               ["Arizona" "Buffalo"])
                         [{:player-name "Bijan Robinson" :position "RB"}])
          ;; ESPN publishes "Cardinals D/ST" where Sleeper has "Arizona
          ;; Defense" — the normalized name key cannot bridge that.
          enrichment {(match/key-for "Cardinals D/ST" "DST") {:espn/adp 100.0}
                      (match/key-for "Bijan Robinson" "RB")  {:espn/adp 3.0}}
          {:keys [report]} (merge/left-join-report universe enrichment)]
      (is (= {:n 2 :rows 1 :matched 0} (get-in report [:by-position "DST"]))
          "32 on the board, a row published, none landed — a structural break")
      (is (= {:n 1 :rows 1 :matched 1} (get-in report [:by-position "RB"]))))))

(deftest left-join-report-separates-a-thin-source-from-a-broken-join
  (let [universe (mapv (fn [i] {:player-name (str "RB" i) :position "RB"})
                       (range 10))]
    (testing "a deliberately partial source has low coverage but a clean hit rate"
      (let [{:keys [report]}
            (merge/left-join-report
             universe {(match/key-for "RB0" "RB") {:fantasypros/aav 50.0}
                       (match/key-for "RB1" "RB") {:fantasypros/aav 40.0}})]
        (is (= 1.0 (:hit-rate report)) "everything it published landed")
        (is (= 0.2 (:coverage report)) "it only ever meant to cover a few")))

    (testing "a broken join publishes rows that go nowhere"
      (let [{:keys [report]}
            (merge/left-join-report
             universe {(match/key-for "Someone Else" "RB") {:fantasypros/aav 50.0}
                       (match/key-for "Another Guy" "RB")  {:fantasypros/aav 40.0}})]
        (is (= 0.0 (:hit-rate report)))
        (is (= 0.0 (:coverage report)))))))

(deftest left-join-report-handles-the-degenerate-cases
  (testing "an empty enrichment map matches nothing but still reports shape"
    (let [{:keys [players report]}
          (merge/left-join-report [{:player-name "A" :position "RB"}] {})]
      (is (= [{:player-name "A" :position "RB"}] players))
      (is (= 0 (:rows report)))
      (is (= 0 (:matched report)))
      (is (= {:n 1 :rows 0 :matched 0} (get-in report [:by-position "RB"])))))

  (testing "an empty universe yields a zero rate rather than dividing by zero"
    (let [{:keys [report]} (merge/left-join-report [] {"k" {:x 1}})]
      (is (= 0.0 (:coverage report)))
      (is (= 0 (:matched report))))))

;; ---- alternate join keys ----

(def ^:private gsis-universe
  [{:player-name "Ja'Marr Chase" :position "WR" :ids {:gsis "00-0036900"}}
   {:player-name "Some Rookie"   :position "WR" :ids {}}])

(deftest opts-can-join-on-gsis-instead-of-a-name
  (let [{:keys [players report]}
        (merge/left-join-report gsis-universe
                                {"00-0036900" {:nflverse/prior-targets 185.0}}
                                {:key-fn       #(get-in % [:ids :gsis])
                                 :key-position {"00-0036900" "WR"}})]
    (is (= 185.0 (:nflverse/prior-targets (first players))))
    (is (not (contains? (second players) :nflverse/prior-targets))
        "a player with no gsis id simply keeps his columns — the rookie case")
    (is (= 1 (:matched report)))
    (is (= {"WR" {:n 2 :rows 1 :matched 1}} (:by-position report))
        "the position index keeps the per-position report meaningful")))

(deftest the-default-arity-still-joins-on-the-name-key
  (let [by-key {(match/key-for "Ja'Marr Chase" "WR") {:fantasypros/ecr 1}}]
    (is (= (merge/left-join-report gsis-universe by-key)
           (merge/left-join-report gsis-universe by-key {}))
        "passing no opts is the same as passing empty opts")
    (is (= 1 (:fantasypros/ecr (first (merge/left-join gsis-universe by-key)))))))
