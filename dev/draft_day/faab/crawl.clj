(ns draft-day.faab.crawl
  "Collect real FAAB waiver auctions from public Sleeper leagues: the corpus the bid
  model's Sleeper-wide backbone is measured from.

  Sleeper has no league search, so the corpus is found the way the replay
  harness finds auctions: a breadth-first walk over leaguemates, reusing
  `draft-day.replay.sleeper`'s fetch layer — its 429 handling, its user agent,
  and its rule that a throttled answer is never recorded as a verdict.

  A league-season is judged from the league object a user's league list already
  carries, so a season that is not FAAB, not finished, too small or out of range
  costs no request at all. One that passes the cheap half has its weekly logs
  fetched and normalized with the app's own `transactions/normalize-season`, so
  the corpus is exactly the auctions the shipped code would see, and it must
  then clear `min-auctions`.

  Each accepted season is written to disk as it is accepted, and the walk's
  state is checkpointed every few users, so an interrupted crawl resumes rather
  than restarts. The walk steers toward whichever league type — superflex or
  one-QB — the corpus is short of, and caps what any one user's leagues may add
  per visit, for the reasons `replay.sleeper/crawl` records."
  (:require [clojure.java.io :as io]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.transactions :as transactions]
            [draft-day.ingestion.transactions.sleeper]
            [draft-day.replay.sleeper :as sleeper]))

(def seasons
  "The seasons swept per user: the three whose preseason universes the replay
  harness already holds, which the backtest will need."
  ["2025" "2024" "2023"])

(def min-auctions
  "Fewest auctions a league-season must have run to count as a market. CHOSEN:
  an abandoned or never-bidding FAAB league runs a handful, a real one runs one
  to four hundred."
  20)

(def pace-ms
  "Pause before every request. Sequential requests already sit well under
  Sleeper's 1000-a-minute ceiling; this keeps a fast network from closing the
  gap."
  60)

(def cache-dir "data/faab_cache")

(defn season-file [league-id] (str cache-dir "/seasons/" league-id ".transit"))

(def state-file
  "The walk's resumable state. Versioned because it holds judgements, which a
  gate change makes wrong rather than stale."
  (str cache-dir "/crawl-state-v1.transit"))

(defn fetch
  "`sleeper/fetch`, paced."
  [path]
  (Thread/sleep (long pace-ms))
  (sleeper/fetch path))

(defn league-kind
  "Redraft, keeper or dynasty, from Sleeper's `settings.type`."
  [lg]
  (case (get-in lg [:settings :type])
    0 :redraft
    1 :keeper
    2 :dynasty
    :unknown))

(defn league-meta
  "What the report slices a league-season by."
  [lg]
  (let [s (:settings lg)]
    (merge {:league-id   (str (:league_id lg))
            :season      (str (:season lg))
            :num-teams   (:total_rosters lg)
            :budget      (:waiver_budget s)
            :min-bid     (or (:waiver_bid_min s) 0)
            :kind        (league-kind lg)
            :daily?      (= 1 (:daily_waivers s))
            :scoring-rec (sleeper/reception-weight lg)
            :previous-league-id (some-> (:previous_league_id lg) str)}
           (sleeper/league-type lg))))

(defn season-shape
  "The cheap half of the gate, read off a league-list entry: nil while still a
  candidate, else why not."
  [lg]
  (let [s (:settings lg)]
    (cond
      (not= 2 (:waiver_type s))                          :not-faab
      (not= "complete" (:status lg))                     :incomplete
      (not (some #{(str (:season lg))} seasons))          :out-of-range
      (< (or (:total_rosters lg) 0) sleeper/min-teams)   :too-small
      (not (pos? (or (:waiver_budget s) 0)))             :no-budget)))

(defn fetch-weeks
  "Network: every week's transaction log, or the first non-answer — a week that
  cannot be read leaves the season undecided rather than thinner."
  [league-id weeks]
  (reduce (fn [acc w]
            (let [resp (fetch (str "/league/" league-id "/transactions/" w))]
              (if (:ok? resp)
                (assoc-in acc [:weeks w] (:body resp))
                (reduced {:ok? false :reason (:reason resp)}))))
          {:ok? true :weeks (sorted-map)}
          weeks))

(defn normalize
  "One league-season through the app's own normalizer."
  [lg weeks]
  (transactions/normalized
   (transactions/normalize-season :sleeper {:league lg :weeks weeks})))

(defn saved-season
  "An accepted season already on disk, or nil. A crawl killed between saving a
  season and checkpointing would otherwise fetch it all again on resume."
  [league-id]
  (try (pipeline/read-transit (season-file league-id))
       (catch Exception _ nil)))

(defn probe-season
  "Judge one league-season: `{:league-id :decision {:ok? :reason :meta}}`, with
  `:season` the normalized auctions when accepted. A throttled week yields
  `:throttled`, which is not a verdict."
  [lg]
  (let [lid  (str (:league_id lg))
        meta (league-meta lg)
        no   (fn [reason] {:league-id lid :decision {:ok? false :reason reason :meta meta}})]
    (if-let [shape (season-shape lg)]
      (no shape)
      (if-let [{saved-meta :meta season :season} (saved-season lid)]
        {:league-id lid :decision {:ok? true :reason :accepted :meta saved-meta} :season season}
        (let [legs (range 1 (inc (long (or (get-in lg [:settings :leg]) 0))))
              got  (fetch-weeks lid legs)]
          (if-not (:ok? got)
            (no :throttled)
            (let [season (normalize lg (:weeks got))
                  n      (count (:auctions season))]
              (if (< n min-auctions)
                (no :too-few-auctions)
                {:league-id lid
                 :decision  {:ok? true :reason :accepted
                             :meta (assoc meta :auctions n)}
                 :season    season}))))))))

(defn undecided? [p] (= :throttled (get-in p [:decision :reason])))

(defn user-leagues
  "Network: a user's league-list entries across `seasons`, or nil when any list
  could not be read — a partial list would mark the missing seasons' leagues as
  never seen, and a resumed walk would not come back for them."
  [uid]
  (reduce (fn [acc season]
            (let [resp (fetch (str "/user/" uid "/leagues/nfl/" season))]
              (cond
                (:ok? resp)                (into acc (:body resp))
                (= :not-found (:reason resp)) acc
                :else                      (reduced nil))))
          []
          seasons))

(defn wanted-first
  "League-list entries, the league type the corpus is short of first."
  [lgs accepted]
  (let [want (sleeper/wanted-superflex? accepted)]
    (sort-by #(if (= want (:superflex? (sleeper/league-type %))) 0 1) lgs)))

(defn visit
  "One user's contribution: probe up to `max-seasons-per-user` unseen
  candidates, the wanted type first, and name the leagues whose owners to walk
  to next. Leagues past the cap are neither probed nor marked seen, so a later
  visit from someone else in them can still take them."
  [lgs {:keys [seen-seasons accepted]} {:keys [max-seasons-per-user expand-per-user]}]
  (let [fresh      (->> lgs
                        (remove #(seen-seasons (str (:league_id %))))
                        (#(wanted-first % accepted))
                        (sort-by #(if (season-shape %) 1 0)))
        candidates (filter #(nil? (season-shape %)) fresh)
        to-probe   (take max-seasons-per-user candidates)
        rejected   (filter season-shape fresh)
        probes     (concat (map probe-season to-probe)
                           (map probe-season rejected))
        expand     (->> (wanted-first (filter #(= 2 (get-in % [:settings :waiver_type])) lgs)
                                      accepted)
                        (map #(str (:league_id %)))
                        distinct
                        (take expand-per-user))]
    {:probes probes :expand expand}))

(defn crawl
  "Walk from `seed-uids` until `max-seasons` are accepted, `max-users` visited or
  the frontier drains. Returns the resumable state
  `{:accepted {league-id meta} :reasons {reason n} :frontier [uid] :seen-users
  #{} :seen-seasons #{} :examined n}`; hand it back as `:state` to resume.

  `on-accept!` is called with each accepted probe as it is accepted and
  `checkpoint!` with the state every `checkpoint-every` users. A user whose
  league list will not load goes to the back of the queue, and after
  `max-retries` is left for a later run rather than looping a throttled walk."
  [seed-uids {:keys [max-seasons max-users max-seasons-per-user expand-per-user
                     checkpoint-every max-retries on-accept! checkpoint! progress! state]
              :or   {max-seasons 400 max-users 1500 max-seasons-per-user 12
                     expand-per-user 6 checkpoint-every 5 max-retries 3}}]
  (let [opts {:max-seasons-per-user max-seasons-per-user :expand-per-user expand-per-user}]
    (loop [st (merge {:accepted {} :reasons {} :frontier (vec (distinct seed-uids))
                      :seen-users #{} :seen-seasons #{} :examined 0}
                     state)]
      (let [{:keys [accepted frontier seen-users]} st]
        (if (or (empty? frontier)
                (>= (count accepted) max-seasons)
                (>= (count seen-users) max-users))
          st
          (let [uid (first frontier)
                st  (update st :frontier subvec 1)]
            (if (seen-users uid)
              (recur st)
              (if-let [lgs (user-leagues uid)]
                (let [{:keys [probes expand]} (visit lgs st opts)
                      probes   (vec probes)
                      decided  (remove undecided? probes)
                      ok       (filter #(get-in % [:decision :ok?]) decided)
                      _        (run! #(when on-accept! (on-accept! %)) ok)
                      owners   (->> expand
                                    (mapcat #(when-let [os (sleeper/body (fetch (str "/league/" % "/rosters")))]
                                               (keep :owner_id os)))
                                    (map str)
                                    distinct)
                      queued   (set frontier)
                      st'      (-> st
                                   (update :accepted into (map (juxt :league-id #(get-in % [:decision :meta]))) ok)
                                   (update :reasons #(reduce (fn [m p] (update m (get-in p [:decision :reason]) (fnil inc 0))) % probes))
                                   (update :seen-seasons into (map :league-id) decided)
                                   (update :seen-users conj uid)
                                   (update :frontier into (remove #(or (seen-users %) (queued %)) owners))
                                   (update :examined + (count probes)))]
                  (when progress! (progress! st'))
                  (when (and checkpoint! (zero? (mod (count (:seen-users st')) checkpoint-every)))
                    (checkpoint! st'))
                  (recur st'))
                (let [tries (inc (get-in st [:retries uid] 0))]
                  (recur (if (< tries max-retries)
                           (-> st (assoc-in [:retries uid] tries) (update :frontier conj uid))
                           (update-in st [:reasons :user-unreadable] (fnil inc 0)))))))))))))

(defn save-season!
  "Persist one accepted probe's auctions and meta."
  [{:keys [league-id decision season]}]
  (let [path (season-file league-id)
        tmp  (str path ".tmp")]
    (io/make-parents path)
    (pipeline/write-transit! tmp {:meta (:meta decision) :season season})
    (.renameTo (io/file tmp) (io/file path))))

(defn load-state []
  (or (try (pipeline/read-transit state-file) (catch Exception _ nil)) {}))

(defn save-state! [st]
  (let [tmp (str state-file ".tmp")]
    (io/make-parents state-file)
    (pipeline/write-transit! tmp st)
    (.renameTo (io/file tmp) (io/file state-file))))
