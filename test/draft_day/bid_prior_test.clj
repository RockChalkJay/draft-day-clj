(ns draft-day.bid-prior-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.bid-prior :as prior]
            [draft-day.db :as db]))

(deftest every-bidder-count-and-phase-has-a-cell
  (is (= (set (for [b [1 2 3 4] ph [:early :mid :late]] [b ph]))
         (set (keys prior/bid-share)))))

(deftest a-cell-is-a-probability-and-a-rising-quantile-curve
  (doseq [[k {:keys [p-zero positive]}] prior/bid-share]
    (is (<= 0.0 p-zero 1.0) (pr-str k))
    (is (= [0.05 0.1 0.25 0.5 0.75 0.9 0.95 0.99] (sort (keys positive))) (pr-str k))
    (is (apply <= (map positive (sort (keys positive)))) (str "quantiles fall in " (pr-str k)))
    (is (every? #(< 0.0 % 1.000001) (vals positive)) (str "a share outside the budget in " (pr-str k)))))

(deftest more-bidders-means-fewer-free-bids-and-bigger-paid-ones
  (doseq [ph [:early :mid :late]]
    (let [cells (map #(prior/bid-share [% ph]) [1 2 3 4])]
      (is (apply > (map :p-zero cells)) (name ph))
      (is (apply <= (map #(get-in % [:positive 0.5]) cells)) (name ph)))))

(deftest every-position-the-app-drafts-has-a-shift
  (is (= (set db/positions) (set (keys prior/position-shift)))))

(deftest other-budgets-sit-below-the-100-dollar-scale-at-every-bidder-count
  (is (= #{1 2 3 4} (set (keys prior/budget-shift))))
  (is (every? (comp neg? :log-shift) (vals prior/budget-shift))))

(deftest a-correlation-is-a-correlation
  (doseq [[_ m] prior/persistence
          k [:per-week :zero-share :aggression]]
    (is (<= -1.0 (k m) 1.0))))
