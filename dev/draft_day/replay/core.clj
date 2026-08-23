(ns draft-day.replay.core
  "Replay a normalized auction draft through the real valuation engine and score
  the model's predicted Worth (just before each purchase) against the actual price.

  The replay is *exact*, not approximate: the only live inputs to Worth are the
  drafted-id set, each team's bankroll, the total count of filled slots
  (`empty-slots-by-pos` is only ever summed), and the picks log — so which roster
  slot a player fills never affects Worth, and `apply-pick` can fill any empty
  slot on the buying team."
  (:require [draft-day.rankings.engine :as engine]
            [draft-day.rankings.market :as market]
            [draft-day.rankings.league-state :as ls]
            [draft-day.replay.sleeper :as sleeper]
            [draft-day.ingestion.league-import :as li]
            [draft-day.ingestion.league-import.sleeper]  ; register :sleeper defmethods
            [draft-day.replay.universe :as universe]))

(defn roster->slots
  "League roster config {:qb :rb :wr :te :flex :k :dst :bench} -> vector of slot
  labels for one team. Labels are cosmetic (irrelevant to Worth); only the count
  matters."
  [{:keys [qb rb wr te flex k dst bench]
    :or   {qb 0 rb 0 wr 0 te 0 flex 0 k 0 dst 0 bench 0}}]
  (vec (concat (repeat qb "QB") (repeat rb "RB") (repeat wr "WR") (repeat te "TE")
               (repeat flex "FLEX") (repeat k "K") (repeat dst "DST") (repeat bench "BENCH"))))

(defn base-state [num-teams budget roster]
  (let [slots (roster->slots roster)]
    {:teams (mapv (fn [rid]
                    {:team-id (str rid) :bankroll (double budget)
                     :roster  (mapv #(hash-map :pos % :player-id nil) slots)})
                  (range 1 (inc num-teams)))            ; Sleeper roster_id = 1..N
     :drafted-player-ids #{}
     :starting-bankroll  (double budget)
     :picks []}))

(defn- fill-one-slot [team pid]
  (if-let [i (first (keep-indexed (fn [i s] (when (nil? (:player-id s)) i)) (:roster team)))]
    (assoc-in team [:roster i :player-id] pid)
    team))

(defn apply-pick
  "Fold one realized pick into the draft state."
  [state {:keys [player-id position price team-id]}]
  (-> state
      (update :drafted-player-ids conj player-id)
      (update :picks conj {:player-id player-id :position position :price price})
      (update :teams (fn [ts]
                       (mapv (fn [t]
                               (if (= (:team-id t) team-id)
                                 (-> t (update :bankroll - price) (fill-one-slot player-id))
                                 t))
                             ts)))))

(defn- filled-frac [state total-slots]
  (let [empty (reduce + 0 (vals (ls/empty-slots-by-pos state)))]
    (/ (double (- total-slots empty)) (double total-slots))))

(defn replay-draft
  "Score every pick of a normalized draft against the engine's Worth at the moment
  before it was bought. Returns a seq of per-pick rows (players missing from the
  season universe are skipped -> coverage gap)."
  [static ndraft]
  (let [{:keys [num-teams budget roster]} ndraft
        total-slots (* num-teams (count (roster->slots roster)))
        pool        (* num-teams (double budget))]
    (loop [state (base-state num-teams budget roster)
           ps    (:picks ndraft)
           rows  []]
      (if-let [pick (first ps)]
        (let [live  (engine/live-valuation static state)
              board (market/with-market (:players live) pool)
              byid  (get (into {} (map (juxt :player-id identity)) board) (:player-id pick))
              row   (when byid
                      {:draft-id    (:draft-id ndraft)
                       :season      (:season ndraft)
                       :player-id   (:player-id pick)
                       :position    (:position byid)
                       :actual      (:price pick)
                       :worth       (:worth byid)
                       :value       (:value byid)
                       :market      (:market byid)
                       :inflation   (:inflation live)
                       :filled-frac (filled-frac state total-slots)})]
          (recur (apply-pick state pick) (rest ps) (cond-> rows row (conj row))))
        rows))))

(defn league-config
  "The league's real scoring and roster, fetched through the harness's own
  throttled, retrying client.

  `league_import/import-league` would do this in one call, but its network half
  is a bare un-retried GET with no User-Agent that swallows a 429 into
  `{:ok false}` — fine for one interactive import from the app, wrong for a
  scoring run that now issues hundreds of back-to-back requests to the host the
  crawl just finished rate-limiting itself against. Under a 429 burst an
  arbitrary subset of drafts would contribute no rows, indistinguishable from a
  projection gap, and the report's MAE would be computed on a silently truncated
  sample.

  So the network half comes from `replay/sleeper`, and only the *pure* half —
  `normalize-league`, the provider seam's whole point — is reused."
  [league-id]
  (let [resp (sleeper/league league-id)]
    (cond
      (sleeper/unreadable? resp) {:ok false :reason (:reason resp)}
      (not (:ok? resp))          {:ok false :reason :not-found}
      :else (try
              {:ok true :config (li/normalize-league :sleeper (:body resp))}
              (catch Exception e {:ok false :reason :unparseable :error (ex-message e)})))))

(defn score-draft
  "Full pipeline for one normalized draft: import the league's real scoring/roster,
  build the season-vintage universe, static-rank, replay. Returns rows (empty if
  settings or projections are unavailable)."
  [{:keys [league-id season num-teams budget] :as ndraft}]
  (let [imp (league-config league-id)
        univ (universe/season-universe season)]
    (if (or (not (:ok imp)) (empty? univ))
      (do (when-not (:ok imp)
            (println (format "  no rows for %s: league unreadable (%s)"
                             league-id (name (:reason imp)))))
          [])
      (let [{:keys [scoring roster]} (:config imp)
            static (engine/static-rankings
                    univ scoring num-teams
                    {:replacement-config (select-keys roster [:qb :rb :wr :te :flex])})]
        ;; league-import roster (with :bench/:k/:dst) drives the full slot template
        (replay-draft static (assoc ndraft :roster roster :budget budget))))))
