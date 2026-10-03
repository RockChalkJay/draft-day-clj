(ns draft-day.ingestion.sleeper-defense-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.sleeper-defense :as sd]))

(def ^:private sf {:player_id "SF" :stats {:gp 17.0 :sack 20.0 :int 6.0 :blk_kick 0.0 :yds_allow 5784.0}})

(deftest an-entry-becomes-a-line-with-its-zeros
  (is (= ["SF" {:sack 20.0 :int 6.0 :blk_kick 0.0 :ff 0.0 :fum_rec 0.0
                :yds_allow 5784.0 :pts_allow 0.0}
          17.0]
         (sd/entry->line sf))
      "a stat Sleeper left out of a played season is a zero, not a dash")
  (is (nil? (sd/entry->line (assoc-in sf [:stats :gp] 0.0))) "no games, no season")
  (is (nil? (sd/entry->line {:stats {:gp 1.0}})) "nothing to join on"))

(deftest history-is-shaped-like-nflverses
  (let [h (get (sd/history {2024 [sf] 2025 [(assoc-in sf [:stats :gp] 18.0)]}) "SF")]
    (is (= [2024 2025] (mapv :season (:nflverse/history h))) "oldest first")
    (is (= {2024 17.0 2025 17.0} (:nflverse/games-by-season h)) "clamped to the season's length")
    (is (= {2024 17 2025 17} (:nflverse/games-seasons h)))))

(deftest a-lost-season-narrows-the-window
  (with-redefs [sd/fetch-season (fn [s] (when (= s 2025) [sf]))]
    (let [r (sd/fetch 2025)]
      (is (= ["SF"] (keys (:by-key r))))
      (is (= {"SF" "DST"} (:positions r)))
      (is (= [2025] (mapv :season (get-in r [:by-key "SF" :nflverse/history])))))))

(deftest every-season-lost-is-nil
  (with-redefs [sd/fetch-season (fn [_] nil)]
    (is (nil? (sd/fetch 2025)))))
