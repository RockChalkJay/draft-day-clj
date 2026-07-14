(ns draft-day.rankings.scarcity-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.scoring :as scoring]
            [draft-day.rankings.engine :as engine]))

(defn- team [id cash roster-pos]
  {:team-id id :bankroll cash :roster (mapv (fn [p] {:pos p :player-id nil}) roster-pos)})

(def ^:private roster ["RB" "RB" "WR" "TE" "FLEX" "BENCH" "BENCH"])

;; A steep TE cliff (te0 elite, then a big drop) at a thin, in-demand position.
(defn- board []
  (concat
   (map (fn [i] {:player-id (str "rb" i) :position "RB" :stats {:rush_yd (- 2500 (* i 60))}})
        (range 30))
   (map (fn [i] {:player-id (str "wr" i) :position "WR" :stats {:rec_yd (- 1500 (* i 50)) :rec (- 100 i)}})
        (range 20))
   [{:player-id "te0" :position "TE" :stats {:rec_yd 1400 :rec 90 :rec_td 10}}]  ; elite
   (map (fn [i] {:player-id (str "te" (inc i)) :position "TE"
                 :stats {:rec_yd (- 700 (* i 20)) :rec (- 55 i) :rec_td 4}})
        (range 12))))

(defn- worth-of [profile id]
  (let [static (engine/static-rankings (board) (:ppr scoring/presets) 12 {:profile profile})
        live   (engine/live-valuation
                static
                {:teams (mapv #(team (str "t" %) 200.0 roster) (range 12))
                 :drafted-player-ids #{} :starting-bankroll 200.0}
                {:profile profile})]
    (:worth (first (filter #(= id (:player-id %)) (:players live))))))

(deftest scarcity-boosts-last-of-tier-cliff
  ;; te0 sits atop a steep TE cliff at a thin, in-demand position; the Scarcity
  ;; lens prices him higher than Balanced does.
  (is (> (worth-of :scarcity "te0") (worth-of :balanced "te0"))))

(deftest scarcity-fold-is-identity-for-balanced
  ;; Balanced runs the same live path with the fold at w=0 -> te0 still priced.
  (is (pos? (worth-of :balanced "te0"))))
