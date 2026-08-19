(ns draft-day.views.util-test
  "Headshot URL resolution for the On-the-block tile. Sleeper's CDN keys on the
  numeric Sleeper id, but the app's canonical :player-id is GSIS for most players.
  Run with `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.views.util :as util]))

(deftest headshot-url-uses-sleeper-id-from-ids
  (testing "GSIS-anchored players use [:ids :sleeper], not :player-id"
    (is (= "https://sleepercdn.com/content/nfl/players/thumb/10210.jpg"
           (util/headshot-url {:player-id "00-0038564"
                               :position  "TE"
                               :ids       {:sleeper "10210" :gsis "00-0038564"}})))))

(deftest headshot-url-dst-uses-team-logo
  (testing "DST players get the team logo from :ids :team"
    (is (= "https://sleepercdn.com/images/team_logos/nfl/bal.png"
           (util/headshot-url {:player-id "BAL"
                               :position  "DST"
                               :ids       {:sleeper "BAL" :team "BAL"}})))))

(deftest headshot-url-falls-back-to-player-id
  (testing "legacy rows without :ids still use :player-id"
    (is (= "https://sleepercdn.com/content/nfl/players/thumb/9509.jpg"
           (util/headshot-url {:player-id "9509" :position "RB"})))))
