(ns draft-day.stat-lines-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.stat-lines :as sl]))

(def ^:private bijan
  {:position "RB"
   :nflverse/games-seasons   {2023 17 2024 17 2025 17}
   :nflverse/games-by-season {2023 17.0 2024 17.0 2025 17.0}
   :nflverse/history
   [{:season 2023 :stats {:rush_yd 976.0  :rush_td 4.0  :rec 58.0 :rec_yd 487.0 :rec_td 4.0
                          ;; the raw line really does carry these, all zero
                          :pass_yd 0.0 :pass_td 0.0}}
    {:season 2024 :stats {:rush_yd 1456.0 :rush_td 14.0 :rec 61.0 :rec_yd 431.0 :rec_td 1.0}}
    {:season 2025 :stats {:rush_yd 1478.0 :rush_td 7.0  :rec 79.0 :rec_yd 820.0 :rec_td 4.0}}]
   :stats {:rush_yd 1372.0 :rush_td 9.0 :rec 64.0 :rec_yd 537.0 :rec_td 3.0}})

(defn- row [t label] (first (filter #(= label (:label %)) (:rows t))))

(deftest a-back-is-described-by-his-own-stats
  (let [t (sl/stat-table bijan 2026)]
    (is (= [2023 2024 2025] (:seasons t)))
    (is (= 2026 (:proj-season t)))
    (is (false? (:rookie? t)))
    (is (= ["Rush Yd" "Rush TD" "Rec" "Rec Yd" "Rec TD" "Games"] (mapv :label (:rows t)))
        "a back is described rushing-first, each phase's volume above its scores")
    (is (= [976.0 1456.0 1478.0] (:values (row t "Rush Yd"))))
    (is (= 1372.0 (:proj (row t "Rush Yd"))))))

(deftest touchdowns-are-split-by-phase-for-every-position
  ;; They used to share one `TD` row for everyone but quarterbacks, so the tile
  ;; answered "how did he score?" for one position and refused to for the rest.
  (let [t (sl/stat-table bijan 2026)]
    (is (= [4.0 14.0 7.0] (:values (row t "Rush TD"))))
    (is (= 9.0 (:proj (row t "Rush TD"))))
    (is (= [4.0 1.0 4.0] (:values (row t "Rec TD"))))
    (is (= 3.0 (:proj (row t "Rec TD"))))
    (is (nil? (row t "TD")) "and no combined row survives alongside them")))

(deftest each-position-leads-with-the-phase-it-is-described-by
  ;; Not one fixed sequence: a receiver's `Rec` must not sit below rushing rows
  ;; he mostly does not have. What is uniform is the shape within a phase --
  ;; volume above scores -- and that every phase present carries its own TD row.
  (is (= ["Pass Yd" "Pass TD" "Rush Yd" "Rush TD"]
         (mapv first (get sl/position-rows "QB"))))
  (is (= ["Rush Yd" "Rush TD" "Rec" "Rec Yd" "Rec TD"]
         (mapv first (get sl/position-rows "RB"))))
  (doseq [pos ["WR" "TE"]]
    (is (= ["Rec" "Rec Yd" "Rec TD" "Rush Yd" "Rush TD"]
           (mapv first (get sl/position-rows pos)))
        (str pos " leads with receiving, the phase it is described by"))))

(deftest a-cell-is-absent-or-zero-but-never-confused
  ;; Absent is not zero: a line the source has no entry in contributes nothing
  ;; rather than a confident 0.
  (is (= 4.0 (sl/stat-value {:rush_td 4.0} :rush_td)))
  (is (nil? (sl/stat-value {:rec 10.0} :rush_td)))
  (is (nil? (sl/stat-value nil :rush_td)))
  (testing "but a real zero survives — he genuinely scored none"
    (is (= 0.0 (sl/stat-value {:rush_td 0.0} :rush_td)))))

(deftest a-row-dead-across-the-whole-window-is-dropped
  ;; A quarterback carries receiving columns in the raw data and they are all
  ;; zeroes. Four dead rows would push the rows that matter off the tile.
  (let [allen {:position "QB"
               :nflverse/games-seasons   {2024 17 2025 17}
               :nflverse/games-by-season {2024 16.0 2025 16.0}
               :nflverse/history
               [{:season 2024 :stats {:pass_yd 3731.0 :pass_td 28.0 :rush_yd 531.0
                                      :rush_td 12.0 :rec 0.0 :rec_yd 7.0 :rec_td 1.0}}
                {:season 2025 :stats {:pass_yd 3668.0 :pass_td 25.0 :rush_yd 579.0
                                      :rush_td 14.0 :rec 0.0 :rec_yd 0.0 :rec_td 0.0}}]
               :stats {:pass_yd 3650.0 :pass_td 27.0 :rush_yd 535.0 :rush_td 11.0}}
        t (sl/stat-table allen 2026)]
    (is (= ["Pass Yd" "Pass TD" "Rush Yd" "Rush TD" "Games"] (mapv :label (:rows t)))
        "a QB is described by passing and rushing, never by his stray catches")
    (testing "and the stray catch is real — 7 yards and a score in 2024 — so it
              is `position-rows` withholding the receiving phase from him, not
              the dead-row rule dropping it"
      (is (= 1.0 (get-in allen [:nflverse/history 0 :stats :rec_td])))
      (is (nil? (row t "Rec TD"))))
    (testing "the rows he does have keep their real numbers"
      (is (= [28.0 25.0] (:values (row t "Pass TD")))))))

(deftest a-season-he-is-missing-from-is-a-dash-in-a-column-that-exists
  ;; The column comes from the fetched window, not from his own history.
  (let [t (sl/stat-table (update bijan :nflverse/history
                                 (fn [h] (filterv #(not= 2024 (:season %)) h)))
                         2026)]
    (is (= [2023 2024 2025] (:seasons t)))
    (is (= [976.0 nil 1478.0] (:values (row t "Rush Yd"))))))

(deftest a-season-that-was-never-fetched-gets-no-column-at-all
  ;; The other half of A MISSING SEASON IS NOT A MISSED SEASON: a season the
  ;; network lost must not show the whole league a blank year.
  (let [t (sl/stat-table (assoc bijan :nflverse/games-seasons {2024 17 2025 17}) 2026)]
    (is (= [2024 2025] (:seasons t)))
    (is (= 2 (count (:values (row t "Rush Yd")))))))

(deftest season-keys-are-read-in-either-vocabulary
  ;; The browser gets these maps through JSON, which turns the integer key 2023
  ;; into the keyword :2023. A lookup in the wrong vocabulary returns nil
  ;; silently, so the coercion is asserted directly rather than only through the
  ;; JVM's integer-keyed shape.
  (is (= 2023 (sl/season-key 2023)))
  (is (= 2023 (sl/season-key :2023)))
  (is (= 2023 (sl/season-key "2023")))
  (is (nil? (sl/season-key :not-a-season)))
  (is (= {2023 17.0 2025 16.0} (sl/by-season {:2023 17.0 :2025 16.0}))))

(deftest the-table-builds-from-the-shape-the-browser-actually-receives
  (let [browser (assoc bijan
                       :nflverse/games-seasons   {:2023 17 :2024 17 :2025 17}
                       :nflverse/games-by-season {:2023 17.0 :2025 16.0})
        t (sl/stat-table browser 2026)]
    (is (= [2023 2024 2025] (:seasons t)))
    (is (= [17.0 nil 16.0] (:values (row t "Games")))
        "a season he has no games row for is a dash, not a zero")))

(deftest a-rookie-says-so-rather-than-showing-a-row-of-dashes
  (let [rookie {:position "RB"
                :nflverse/games-seasons {2023 17 2024 17 2025 17}
                :stats {:rush_yd 897.0 :rush_td 5.0 :rec 47.0 :rec_yd 351.0 :rec_td 2.0}}
        t (sl/stat-table rookie 2026)]
    (is (true? (:rookie? t)))
    (is (= [nil nil nil] (:values (row t "Rush Yd"))))
    (is (= 897.0 (:proj (row t "Rush Yd"))))
    (testing "and with no games in any season, the Games row is left off entirely"
      (is (nil? (row t "Games"))))))

(deftest kickers-and-defenses-get-no-table
  ;; Not an empty table — the tile falls back to what it showed before.
  (is (nil? (sl/stat-table (assoc bijan :position "K") 2026)))
  (is (nil? (sl/stat-table (assoc bijan :position "DST") 2026)))
  (is (nil? (sl/stat-table (dissoc bijan :position) 2026))))

(deftest a-player-with-no-numbers-at-all-gets-no-table
  ;; Every row would be dropped, so there is nothing to draw.
  (is (nil? (sl/stat-table {:position "WR" :nflverse/games-seasons {2025 17}} 2026))))

(deftest every-row-has-one-value-per-season-column
  ;; The view zips values against :seasons positionally, so a short row would
  ;; silently shift a season's number into the wrong column.
  (let [t (sl/stat-table bijan 2026)]
    (is (every? #(= (count (:seasons t)) (count (:values %))) (:rows t)))))
