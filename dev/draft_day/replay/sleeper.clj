(ns draft-day.replay.sleeper
  "Read-only access to Sleeper's free, keyless API for the replay harness: fetch a
  completed auction draft, normalize its picks, and crawl public leagues over
  leaguemates to assemble a clean corpus.

  The corpus quality gate (`clean-auction?`) is the crux — it admits only cleanly
  recorded *live* Sleeper auctions and rejects offline back-fills (the user's own
  RaiderNation drafts fail it: a team $74 short, Ja'Marr Chase logged at $1)."
  (:require [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draft-day.json :refer [mapper]]))

(def ^:private base "https://api.sleeper.app/v1")

(defn- get-json [url]
  (let [{:keys [status body error]} @(http/get url {:timeout 30000})]
    (when (and (not error) (= 200 status) body)
      (json/read-value body mapper))))

;; ---- raw endpoints ----
(defn user          [name-or-id] (get-json (str base "/user/" name-or-id)))
(defn user-leagues  [uid season] (get-json (str base "/user/" uid "/leagues/nfl/" season)))
(defn league        [lid]        (get-json (str base "/league/" lid)))
(defn league-owners [lid]        (keep :owner_id (get-json (str base "/league/" lid "/rosters"))))
(defn draft         [did]        (get-json (str base "/draft/" did)))
(defn draft-picks   [did]        (get-json (str base "/draft/" did "/picks")))

;; ---- normalization ----
(defn- amount [pick] (some-> (get-in pick [:metadata :amount]) parse-long))

(defn normalize-draft
  "Draft object + picks -> a replay-ready draft map. Picks carry only what the
  engine needs, sorted by nomination order."
  [d picks]
  (let [s (:settings d)]
    {:draft-id  (:draft_id d)
     :season    (:season d)
     :num-teams (:teams s)
     :budget    (or (:budget s) 200)
     :league-id (:league_id d)
     :picks (->> picks
                 (keep (fn [p]
                         (when-let [a (amount p)]
                           {:player-id (:player_id p)
                            :position  (get-in p [:metadata :position])
                            :price     (double a)
                            :team-id   (str (:roster_id p))
                            :pick-no   (:pick_no p)})))
                 (sort-by :pick-no)
                 vec)}))

;; ---- corpus quality gate ----
(defn- ppr? [lg]
  (let [rec (get-in lg [:scoring_settings :rec])]
    (and (number? rec) (== 1.0 (double rec)))))

(defn clean-auction?
  "True only for a cleanly recorded live 12-team PPR $200 auction. Needs the draft
  `d`, its `picks`, and the `lg` league object (for scoring)."
  [d picks lg]
  (let [s      (:settings d)
        budget (or (:budget s) 200)
        amts   (keep amount picks)
        n-amt  (max 1 (count amts))]
    (boolean
     (and (= "auction" (:type d))
          (= "complete" (:status d))
          (= 12 (:teams s))
          (= 200 budget)
          (ppr? lg)
          (seq picks)
          (>= (/ (count amts) (double (count picks))) 0.95)            ; amounts present
          (<= (/ (count (filter #(= 1 %) amts)) (double n-amt)) 0.30)  ; not $1-defaulted
          (let [spend (->> picks (group-by :roster_id)
                           (map (fn [[_ ps]] (reduce + 0 (keep amount ps)))))]
            (and (= 12 (count spend))                                  ; all 12 teams present
                 (every? #(<= (* 0.85 budget) % budget) spend)))))))   ; each ~fully spent

;; ---- crawl ----
(def ^:private seasons ["2025" "2024" "2023"])

(defn candidate-league?
  "Cheap pre-filter from the league *summary* (no extra fetch): a 12-team PPR
  league with a draft. Cuts the vast majority of leagues before we touch
  draft/picks."
  [lg]
  (and (:draft_id lg)
       (= 12 (:total_rosters lg))
       (ppr? lg)))

(defn crawl
  "BFS from `seed-uids` over leaguemates, collecting draft_ids of clean 12-team PPR
  $200 auctions. Returns {:accepted #{draft-id...} :visited n :examined n}.

  The accept gate is strict (`clean-auction?`), but the frontier expands broadly —
  via owners of up to `:expand-per-user` of each user's leagues (12-team PPR ones
  first) — so the walk escapes an insular seed group and reaches the wider Sleeper
  population where clean public auctions live. Bounded by `:max-drafts` accepted
  and `:max-users` visited."
  [seed-uids {:keys [max-drafts max-users expand-per-user progress!]
              :or   {max-drafts 40 max-users 500 expand-per-user 8}}]
  (loop [frontier    (vec (distinct seed-uids))
         seen-users  #{}
         seen-drafts #{}
         seen-lgs    #{}
         accepted    #{}
         examined    0]
    (if (or (empty? frontier)
            (>= (count accepted) max-drafts)
            (>= (count seen-users) max-users))
      {:accepted accepted :visited (count seen-users) :examined examined}
      (let [uid (first frontier)]
        (if (seen-users uid)
          (recur (subvec frontier 1) seen-users seen-drafts seen-lgs accepted examined)
          (let [leagues (mapcat #(user-leagues uid %) seasons)
                cands  (for [lg leagues
                             :when (and (candidate-league? lg)
                                        (not (seen-drafts (:draft_id lg))))]
                         lg)
                probes (for [lg cands
                             :let [did   (:draft_id lg)
                                   d     (draft did)
                                   picks (draft-picks did)]]
                         {:did did :lg lg
                          :ok (boolean (and d picks (clean-auction? d picks lg)))})
                ;; expand via up to N leagues (candidates first) to move through
                ;; the graph without an unbounded rosters-fetch per user.
                expand-lgs (->> leagues
                                (sort-by #(if (candidate-league? %) 0 1))
                                (map :league_id)
                                (remove seen-lgs)
                                distinct
                                (take expand-per-user))
                owners    (distinct (mapcat league-owners expand-lgs))
                accepted' (into accepted (comp (filter :ok) (map :did)) probes)]
            (when progress!
              (progress! {:uid uid :visited (inc (count seen-users))
                          :accepted (count accepted') :candidates (count cands)
                          :frontier (count frontier)}))
            (recur (into (subvec frontier 1) (remove seen-users owners))
                   (conj seen-users uid)
                   (into seen-drafts (map :did) probes)
                   (into seen-lgs expand-lgs)
                   accepted'
                   (+ examined (count probes)))))))))
