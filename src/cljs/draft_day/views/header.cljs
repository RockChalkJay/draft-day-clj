(ns draft-day.views.header
  "The bar across the top of every tab: which half of the app is on screen, its
  tabs, the way to the other half, the league, and the numbers that half is read
  against — the auction room's on draft night, the week's and the budget's after.

  Its own namespace rather than part of `core` so the node test build can reach
  it: `core` mounts a React root when it loads."
  (:require [re-frame.core :as rf]
            [draft-day.providers :as providers]
            [draft-day.views.util :as util]))

(defn- fmt-mult [x] (str "×" (.toFixed (or x 1) 2)))

(defn league-switcher
  "Which league the whole app is about, on every tab.

  A dropdown once there is more than one, a plain label when there is one, and a
  route to Settings when there is none — because a select with a single option is
  a control that asks you to confirm the only possible answer, and one with no
  options is a control that cannot be used.

  Grouped by account, so switching hosts is the same control as switching
  leagues rather than a second one. The group label names the account and never
  its id: an ESPN user id is the SWID, which is half the credential pair."
  []
  (let [leagues @(rf/subscribe [:league-list])
        groups  @(rf/subscribe [:leagues-by-account])
        active  @(rf/subscribe [:active-league-key])
        {:keys [league-name my-team-name]} @(rf/subscribe [:account])]
    [:div.league-switcher
     (cond
       (empty? leagues)
       [:button.link {:on-click #(rf/dispatch [:set-view :settings :leagues])} "Connect a league"]

       (= 1 (count leagues))
       [:span.league-label [:b (or league-name "League")]]

       :else
       [:select {:value (str active)
                 :title "Switch league — the board, the prices and the waiver wire all follow"
                 :on-change #(rf/dispatch [:set-active-league (.. % -target -value)])}
        (for [[ak acct ls] groups
              :when (seq ls)]
          ^{:key (or ak "orphans")}
          [:optgroup {:label (if ak
                               (str (providers/label (:provider acct))
                                    (when (:username acct) (str " · " (:username acct))))
                               "No account")}
           (for [[k e] ls]
             ^{:key k} [:option {:value k} (or (:name e) (:league-id e))])])])
     (when my-team-name [:span.league-team my-team-name])]))

(def view-labels
  {:board "Board" :league "League" :team "My Team" :matchup "Matchup" :waivers "Waivers"
   :rosters "League"})

(defn mode-link
  "The way to the other half of the app. Quiet on purpose: it is used a couple
  of times a season, and a toggle beside the tabs would ask to be read every
  time the header is."
  []
  (let [mode  @(rf/subscribe [:mode])
        auto? @(rf/subscribe [:phase-auto?])
        other (if (= mode :season) :draft :season)]
    [:button.mode-link
     {:title    (str (if auto? "Chosen automatically" "Set by hand")
                     " — change it per league under Settings")
      :on-click #(rf/dispatch [:switch-mode other])}
     (if (= other :draft) "Go to draft board →" "Go to season →")]))

(defn draft-stats []
  (let [market  @(rf/subscribe [:market])
        my-team @(rf/subscribe [:my-team])
        max-bid @(rf/subscribe [:my-max-bid])
        ;; the banded figure the board prices at, straight from the server —
        ;; multiplying :inflation by :market-heat here skipped the band and read
        ;; ×0.40 where the board was pricing at 0.50
        infl    (or (:market-multiplier market)
                    (* (or (:inflation market) 1) (or (:market-heat market) 1)))]
    [:<>
     [:div.stats
      [:div.stat {:title "Market inflation × phase decay"}
       [:span.stat-label "Market"] [:span.stat-val (fmt-mult infl)]]
      [:div.stat {:title "Σ(price paid − value) — rising = room overpaying"}
       [:span.stat-label "Infl Idx"]
       [:span.stat-val {:class (cond (> (or (:inflation-index market) 0) 0) "warn"
                                     (< (or (:inflation-index market) 0) 0) "good")}
        (str "$" (js/Math.round (or (:inflation-index market) 0)))]]
      [:div.stat [:span.stat-label "Bankroll"] [:span.stat-val.good (str "$" (:bankroll my-team))]]
      [:div.stat [:span.stat-label "Max Bid"] [:span.stat-val.good (str "$" max-bid)]]]
     [:button.start-draft {:on-click #(rf/dispatch [:show-modal {:kind :start-draft}])} "Start Draft"]]))

(defn season-stats
  "The week, the budget and how old the rosters are, on every season tab.

  Re-sync lives here and reports itself here: all four tabs read the same synced
  rosters, and two of them render no status line of their own."
  []
  (let [{:keys [week faab synced-at]} @(rf/subscribe [:season-header])
        league @(rf/subscribe [:active-league])
        status @(rf/subscribe [:sync-status])]
    [:div.stats.season-meta
     (when week
       [:div.stat [:span.stat-label "Week"] [:span.stat-val week]])
     (when faab
       [:div.stat {:title "FAAB left of this season's budget"}
        [:span.stat-label "FAAB"]
        [:span.stat-val.good (str (util/faab (:left faab)) " / " (util/faab (:budget faab)))]])
     (when league
       ;; The button reports its own work: it is on all four season tabs, and
       ;; two of them render no status line of their own.
       [:button.sync-btn {:on-click #(rf/dispatch [:refresh-league
                                                   (select-keys league [:provider :league-id])])}
        "↻ Re-sync"
        [:small (or status
                    (if-let [at (util/fetched-at-label synced-at)]
                      (str "synced " at)
                      "not synced yet"))]])]))

(defn header []
  (let [mode   @(rf/subscribe [:mode])
        tabs   @(rf/subscribe [:mode-views])
        view   @(rf/subscribe [:view])
        status @(rf/subscribe [:status])]
    [:header.top
     [:div.brand "🏈 Draft Day"]
     ;; No mode yet means the week is still loading: draw neither half rather
     ;; than one that may be taken back.
     (when mode
       [:span.mode-tag (if (= mode :season) "Season" "Draft")])
     [:nav.views
      (for [v tabs]
        ^{:key v}
        [:button {:class (when (= view v) "on") :on-click #(rf/dispatch [:set-view v])}
         (view-labels v)])]
     (when mode [mode-link])
     [league-switcher]
     ;; Truncated on a crowded header, so the whole text rides on hover.
     [:div.status {:title status} status]
     (case mode
       :season [season-stats]
       :draft  [draft-stats]
       nil)
     [:button.gear {:class    (when (= view :settings) "on")
                    :title    "Settings"
                    :on-click #(rf/dispatch [:set-view :settings])}
      "⚙"]]))

