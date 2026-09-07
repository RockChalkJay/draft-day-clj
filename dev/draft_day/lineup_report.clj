(ns draft-day.lineup-report
  "What changes if `:lineup-upgrade` replaces `:upgrade` as the board's headline.

  Research, not shipped — same standing as `benchmark/` and `replay/`. It exists
  because the four decisions that follow (does the bid track it, does the drop
  become lineup-aware, does it lead the sort) should be made against a real
  league rather than against an argument.

  It needs a real synced league: with none connected everyone is a free agent,
  the manager has no roster, and the delta is undefined for every row.

      lein run -m draft-day.lineup-report -- --league-id 123 --roster-id 4

  Omit --roster-id and it reports every team in turn, which is the honest way to
  see whether an effect is real or an artifact of one roster's shape."
  (:require [clojure.string :as str]
            [draft-day.db :as db]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.sleeper]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.sleeper]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.api.routes :as routes]
            [draft-day.rankings.lineup :as lineup]
            [draft-day.rankings.ros :as ros]
            [draft-day.rankings.vendor :as vendor]
            [draft-day.rankings.waiver :as waiver]
            [draft-day.scoring :as scoring]))

(defn- spearman
  "Rank correlation between two orderings of the same ids. 1.0 is identical."
  [a b]
  (let [ra (into {} (map-indexed (fn [i x] [x i])) a)
        rb (into {} (map-indexed (fn [i x] [x i])) b)
        ks (filter rb (keys ra))
        n  (count ks)]
    (if (< n 2)
      Double/NaN
      (let [d2 (reduce + (map #(let [d (- (ra %) (rb %))] (* d d)) ks))]
        (- 1.0 (/ (* 6.0 d2) (* (double n) (dec (* n n)))))))))

(defn board-for
  "The waiver board for one league and one roster, exactly as `/api/waivers`
  builds it — via `routes/waiver-board-inputs` and `waiver/waiver-board`, so the
  report cannot drift from what the app serves."
  [league-id roster-id]
  ;; Both proxies answer in an envelope — `{:ok :config}` and `{:ok :league}` —
  ;; which is what `routes` unwraps before the browser ever sees it.
  (let [imported (:config (league-import/import-league
                           {:provider :sleeper :league-id league-id}))
        synced   (:league (league-sync/sync-league
                           {:provider :sleeper :league-id league-id}))
        scoring* (scoring/resolve-config (:scoring imported))
        roster   (:roster imported)
        ;; `routes/universe` is a private atom-backed cache over exactly this;
        ;; a one-shot report has nothing to cache, so it calls the pipeline
        ;; rather than reaching into a private var.
        {:keys [players season through-week]} (pipeline/load-universe {})
        season*  (or season (sleeper/current-season))
        week     (inc (or through-week 0))
        weekly   (pipeline/load-weekly season* week)
        ctx      {:league             synced
                  :my-roster-id       roster-id
                  :roster-size        (count (db/roster-template roster))
                  :num-teams          (or (:num-teams imported) 12)
                  :replacement-config (select-keys roster [:qb :rb :wr :te :flex])
                  :through-week       (or through-week 0)
                  :season-games       (nflverse/games-in-season season*)
                  :playoff-week-start (:playoff-week-start synced)
                  :starting-slots     (db/starting-slots roster)}
        board    (-> players
                     (vendor/for-scoring scoring*)
                     (routes/waiver-board-inputs scoring*)
                     (ros/with-ros scoring* ctx))]
    (assoc (waiver/waiver-board board ctx)
           :roster roster :season season* :through-week (or through-week 0))))

(defn- fmt-row [i {:keys [player-name position upgrade lineup-upgrade bid]}]
  (format "  %2d. %-24s %-4s  upg %+7.1f   lineup %+7.1f   bid %s"
          (inc i) (subs (str player-name) 0 (min 24 (count (str player-name))))
          position (double (or upgrade 0.0)) (double (or lineup-upgrade 0.0))
          (if bid (str "$" bid) "–")))

(defn report-one [{:keys [players my-roster roster through-week]} label]
  (let [slots (db/starting-slots roster)]
  (let [valued  (filter #(number? (:lineup-upgrade %)) players)
        by-upg  (vec (sort-by #(- (double (or (:upgrade %) 0.0))) valued))
        by-lin  (vec (sort-by (juxt #(- (double (or (:lineup-upgrade %) 0.0)))
                                    #(- (double (or (:upgrade %) 0.0))))
                              valued))
        nonzero (filter #(pos? (:lineup-upgrade %)) valued)
        negativ (filter #(neg? (:lineup-upgrade %)) valued)
        top20   (set (map :player-id (take 20 by-upg)))
        kept    (count (filter top20 (map :player-id (take 20 by-lin))))]
    (println (str "\n=== " label " ==="))
    (println (format "roster seats %d   starting %d   through week %d   free agents %d"
                     (count (db/roster-template roster))
                     (count (db/starting-slots roster))
                     through-week (count valued)))
    (println (format "my roster rows: %d" (count my-roster)))
    ;; What matters about the drop is what losing him COSTS THE LINEUP, not
    ;; whether he happens to hold a starting slot. Roster 1's drop is a starting
    ;; DST whose cost is zero, because a second DST is sitting behind him —
    ;; flagging that as "*** A STARTER ***" was a false alarm, and it is exactly
    ;; the confusion this report exists to remove.
    (when-let [d (:drop-candidate (first players))]
      (let [ros-of  #(double (or (:ros-points %) 0.0))
            ;; `my-roster` is built off :player-ids and so includes IR and
            ;; taxi, each flagged :parked?. The board chooses its drop over
            ;; :active-ids, which excludes them — seating a parked player here
            ;; would make a real starter look free to drop, and the report would
            ;; be disagreeing with the thing it exists to explain.
            seated  (filterv #(and (:ros-points %) (not (:parked? %))) my-roster)
            full    (lineup/lineup-points seated slots :ros-points)
            without (lineup/lineup-points
                     (remove #(= (:player-id %) (:player-id d)) seated)
                     slots :ros-points)
            cost    (- full without)]
        (println (format "drop candidate  : %s (%s, %.1f ros) — costs the lineup %.1f%s"
                         (:player-name d) (:position d) (ros-of d) cost
                         (if (pos? cost) "  <-- NO FREE DROP AVAILABLE" "")))))
    (println (format "\nlineup delta > 0 : %d of %d  (%.1f%%)"
                     (count nonzero) (count valued)
                     (* 100.0 (/ (count nonzero) (max 1 (count valued))))))
    (println (format "lineup delta = 0 : %d   < 0 : %d"
                     (- (count valued) (count nonzero) (count negativ))
                     (count negativ)))
    (println (format "rank correlation upgrade vs lineup: %.3f"
                     (spearman (map :player-id by-upg) (map :player-id by-lin))))
    (println (format "of the current top 20 by Upg, %d survive in the lineup top 20" kept))
    (println "\ntop 10 by CURRENT upgrade:")
    (doseq [[i p] (map-indexed vector (take 10 by-upg))] (println (fmt-row i p)))
    (println "\ntop 10 by LINEUP delta:")
    (doseq [[i p] (map-indexed vector (take 10 by-lin))] (println (fmt-row i p))))))

(defn -main [& args]
  (let [opts (apply hash-map args)
        lid  (or (get opts "--league-id") (get opts "-l"))
        rid  (get opts "--roster-id")]
    (when-not lid
      (println "usage: lein run -m draft-day.lineup-report -- --league-id ID [--roster-id N]")
      (System/exit 1))
    (let [synced (:league (league-sync/sync-league
                           {:provider :sleeper :league-id lid}))
          ids    (if rid [(Integer/parseInt rid)] (map :roster-id (:teams synced)))]
      (doseq [id ids]
        (let [b (board-for lid id)
              t (first (filter #(= id (:roster-id %)) (:teams synced)))]
          (report-one b (str "roster " id " — " (:name t)))))
      (shutdown-agents))))
