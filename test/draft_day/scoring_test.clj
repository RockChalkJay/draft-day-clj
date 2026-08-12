(ns draft-day.scoring-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.scoring :as scoring]))

(deftest presets-has-exactly-three-keys
  (is (= #{:standard :half-ppr :ppr} (set (keys scoring/presets)))))

(deftest player-points-defaults-missing-stats-to-zero
  (is (= 0.0 (scoring/player-points {:stats {}} (:ppr scoring/presets)))))

(deftest player-points-ignores-unknown-stat-keys
  (let [player {:stats {:rec 10.0 :some-unknown-stat 999.0}}]
    (is (= 10.0 (scoring/player-points player (:ppr scoring/presets))))))

(deftest stat-keys-covers-every-preset-key
  (is (= (set scoring/stat-keys) (set (keys (:ppr scoring/presets)))))
  (is (every? (set scoring/stat-keys) [:pass_2pt :rush_2pt :rec_2pt :blk_kick])))
