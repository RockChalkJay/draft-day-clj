(ns draft-day.ingestion.espn-news-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [draft-day.ingestion.espn-news :as news]
            [draft-day.ingestion.pipeline :as pipeline]))

(use-fixtures :each (fn [t] (news/reset-cache!) (t) (news/reset-cache!)))

;; Shaped like the live feed, trimmed to the fields the parser reads.
(defn item [type published headline]
  {:type type :published published :headline headline
   :story (str "<p>" headline " in <b>detail</b>.</p>")})

(def feed
  {:feed (concat
          [(item "Story" "2026-09-29T00:00:00Z" "A long-form story")]
          (map #(item "Rotowire" (format "2026-09-%02dT12:00:00Z" %) (str "note " %))
               (range 1 16)))})

(def athlete
  {:athlete {:injuries [{:status "Questionable"
                         :type {:abbreviation "Q"}
                         :shortComment "Back, limited."
                         :longComment "Still dealing with the back injury."
                         :date "2026-10-01T00:46:00.000+00:00"}]}})

(deftest parse-feed-drops-stories-and-caps-the-list
  (let [items (news/parse-feed feed)]
    (is (= news/max-items (count items)))
    (is (every? #(not= "A long-form story" (:headline %)) items))
    (testing "newest first"
      (is (= "note 15" (:headline (first items)))))
    (testing "tags never reach the browser"
      (is (= "note 15 in detail ." (:story (first items)))))))

(deftest parse-status-reads-the-first-injury
  (is (= {:status "Questionable" :abbr "Q" :short "Back, limited."
          :long "Still dealing with the back injury."
          :date "2026-10-01T00:46:00.000+00:00"}
         (news/parse-status athlete))))

(deftest parse-status-of-a-healthy-athlete-is-nil
  (is (nil? (news/parse-status {:athlete {:id "1"}})))
  (is (nil? (news/parse-status {:athlete {:injuries []}}))))

(deftest one-failed-half-still-returns-the-other
  (with-redefs [pipeline/offline? (constantly false)
                news/fetch-feed (fn [_] (throw (ex-info "boom" {})))
                news/fetch-athlete (fn [_] athlete)]
    (let [r (news/player-news "1")]
      (is (= "Q" (get-in r [:status :abbr])))
      (is (= [] (:news r)))
      (is (= [:news] (map :source (:errors r))))))
  (news/reset-cache!)
  (with-redefs [pipeline/offline? (constantly false)
                news/fetch-feed (fn [_] feed)
                news/fetch-athlete (fn [_] (throw (ex-info "boom" {})))]
    (let [r (news/player-news "1")]
      (is (nil? (:status r)))
      (is (= news/max-items (count (:news r))))
      (is (= [:status] (map :source (:errors r)))))))

(deftest the-cache-serves-inside-its-ttl
  (let [calls (atom 0)]
    (with-redefs [pipeline/offline? (constantly false)
                  news/fetch-feed (fn [_] (swap! calls inc) feed)
                  news/fetch-athlete (fn [_] athlete)]
      (news/player-news "1")
      (news/player-news "1")
      (is (= 1 @calls))
      (news/player-news "2")
      (is (= 2 @calls) "keyed by ESPN id")
      (with-redefs [news/ttl-ms (constantly 0)]
        (news/player-news "1")
        (is (= 3 @calls) "refetched once the TTL has passed")))))

(deftest a-failed-reply-is-not-cached
  (let [calls (atom 0)]
    (with-redefs [pipeline/offline? (constantly false)
                  news/fetch-feed (fn [_] (swap! calls inc) (throw (ex-info "boom" {})))
                  news/fetch-athlete (fn [_] athlete)]
      (news/player-news "1")
      (news/player-news "1")
      (is (= 2 @calls)))))

(deftest offline-is-empty-and-calls-nothing
  (with-redefs [pipeline/offline? (constantly true)
                news/fetch-feed (fn [_] (throw (ex-info "no network" {})))
                news/fetch-athlete (fn [_] (throw (ex-info "no network" {})))]
    (is (= {:status nil :news [] :fetched-at nil :errors []}
           (news/player-news "1")))))
