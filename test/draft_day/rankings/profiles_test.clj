(ns draft-day.rankings.profiles-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.scoring :as scoring]
            [draft-day.rankings.engine :as engine]))

(defn- team [id cash roster-pos]
  {:team-id id :bankroll cash :roster (mapv (fn [p] {:pos p :player-id nil}) roster-pos)})

(def ^:private roster (into ["RB" "RB" "WR" "WR" "QB" "TE" "FLEX"] (repeat 8 "BENCH")))

;; 18 filler RBs (default spread) + two RBs with the SAME mean projection (200
;; pts via rush_yd*0.1) but very different expert disagreement.
(defn- board []
  (concat
   (map (fn [i] {:player-id (str "rb" i) :position "RB" :stats {:rush_yd (- 3000 (* i 90))}})
        (range 18))
   [{:player-id "steady" :position "RB" :stats {:rush_yd 2000}
     :fantasypros/rank-std 2  :fantasypros/rank-ave 20}      ; rel 0.10 — safe
   {:player-id "boom" :position "RB" :stats {:rush_yd 2000}
     :fantasypros/rank-std 18 :fantasypros/rank-ave 20}]))   ; rel 0.90 — boom/bust

(defn- worth-of [profile id]
  (let [static (engine/static-rankings (board) (:ppr scoring/presets) 12 {:profile profile})
        live   (engine/live-valuation
                static
                {:teams (mapv #(team (str "t" %) 200.0 roster) (range 12))
                 :drafted-player-ids #{} :starting-bankroll 200.0})]
    (:worth (first (filter #(= id (:player-id %)) (:players live))))))

(deftest balanced-prices-equal-means-equally
  ;; Same mean projection -> same Worth when the lens ignores variance.
  (is (= (worth-of :balanced "steady") (worth-of :balanced "boom"))))

(deftest ceiling-reprices-boom-up
  ;; Under the upside lens the boom/bust player is worth more than the steady one.
  (is (> (worth-of :ceiling "boom") (worth-of :ceiling "steady"))))

(deftest floor-reprices-boom-down
  ;; Under the safe lens it flips: the steady player is worth more.
  (is (< (worth-of :floor "boom") (worth-of :floor "steady"))))
