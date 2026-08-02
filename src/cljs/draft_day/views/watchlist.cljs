(ns draft-day.views.watchlist
  "Manager-curated list of players to nominate, filled from the board's ★."
  (:require [re-frame.core :as rf]
            [draft-day.views.util :as util]))

(defn- watch-row [p]
  (let [id (:player-id p)]
    [:div.w-row {:on-click #(rf/dispatch [:set-nominated id])
                 :title    "Put on the block"}
     [:span.w-name (:player-name p) [:span.w-pos (:position p)]]
     [:span.w-worth (util/money (:worth p))]
     [:span.w-mkt (util/money-rnd (:market p))]
     [:button.w-remove {:title    "Remove from watch list"
                        :on-click (fn [e]
                                    (.stopPropagation e)
                                    (rf/dispatch [:watch-remove id]))}
      "×"]]))

(defn watchlist-panel []
  (let [players @(rf/subscribe [:watchlist-players])]
    [:div.watchlist-panel
     [:div.watchlist-head
      [:h3 "👀 Watch List"]
      [:span.w-count (str (count players) " watched")]]
     (if (seq players)
       [:div.watchlist
        (map (fn [p] ^{:key (:player-id p)} [watch-row p]) players)]
       [:div.w-empty "Star players on the board to track them here."])]))
