(ns draft-day.replay.sleeper
  "Read-only access to Sleeper's free, keyless API for the replay harness: find
  completed auction drafts, normalize their picks, and crawl public leagues over
  leaguemates to assemble a corpus.

  Sleeper publishes no search or discovery endpoint, so the leaguemate graph is
  the only crawl there is. That was never the binding constraint — the corpus
  stayed at two usable drafts because this namespace threw away most of what it
  found:

  - the accept gate demanded *exactly* 12 teams, *exactly* a $200 budget and
    *exactly* full PPR, which rejects most real auctions (the 2023 draft sitting
    in the cache has a $250 budget and could never have passed its own gate);
  - only the league's most recent `draft_id` was ever read, so a league that has
    auctioned every year since 2021 yielded one draft;
  - every failure — including a 429 — came back as nil and read as `no auction
    here`, so throttling was indistinguishable from absence.

  All three are fixed here. The gate returns a *decision* with a reason, so a
  crawl can report why it rejected what it rejected instead of leaving the
  operator to guess."
  (:require [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draft-day.json :refer [mapper]])
  (:import [java.util.concurrent Semaphore]))

(def ^:private base "https://api.sleeper.app/v1")

(def user-agent
  "Identify ourselves rather than crawling anonymously. Same reasoning as
  `benchmark/fetch.clj`: a default client string is what gets throttled first,
  and a throttled crawl that cannot tell it is throttled reports missing data."
  "draft-day-replay/0.1 (fantasy football research; +https://github.com/RockChalkJay/draft-day-clj)")

(def max-in-flight
  "How many requests Sleeper may see from us at once.

  Their docs put the ceiling at 1000 calls/minute before an IP block. Six
  concurrent requests with a 30s timeout cannot approach that, and the cap is
  what keeps a widened crawl — which now probes every league rather than only
  12-team PPR ones — from looking like an attack."
  6)

(defonce ^:private throttle (Semaphore. max-in-flight true))

(defn- attempt
  "One HTTP attempt. Returns {:done resp} for a settled answer or {:retry reason}
  for something worth asking again."
  [url]
  (.acquire throttle)
  (try
    (let [{:keys [status body error]} @(http/get url {:timeout 30000
                                                      :headers {"User-Agent" user-agent}})]
      (cond
        error             {:retry :error}
        (= 404 status)    {:done {:ok? false :reason :not-found}}
        (= 429 status)    {:retry :throttled}
        (not= 200 status) {:retry :error}
        :else
        (let [v (try (json/read-value body mapper) (catch Exception _ ::unparseable))]
          (cond
            ;; Sleeper answers 200 with a JSON `null` body for ids it does not
            ;; know. Left alone that parses to nil and flows on as though it were
            ;; data — the trap `ingestion/league_import/sleeper.clj` calls out.
            (= ::unparseable v) {:done {:ok? false :reason :error}}
            (nil? v)            {:done {:ok? false :reason :not-found}}
            :else               {:done {:ok? true :body v}}))))
    (finally (.release throttle))))

(defn fetch
  "GET a Sleeper path. Returns {:ok? true :body v}, or {:ok? false :reason r}
  where r is :not-found, :throttled or :error.

  A 429 is retried with linear backoff and, if it never clears, surfaces as
  :throttled rather than as an empty result. That distinction is the whole point:
  a crawl that cannot separate `this league has no auction` from `Sleeper asked
  me to slow down` silently under-reports and looks like a data drought."
  ([path] (fetch path {}))
  ([path {:keys [retries backoff-ms] :or {retries 4 backoff-ms 2000}}]
   (let [url (str base path)]
     (loop [n 1 last-reason :error]
       (let [{:keys [done retry]} (attempt url)]
         (cond
           done           done
           (>= n retries) {:ok? false :reason (or retry last-reason)}
           :else          (do (Thread/sleep (* n backoff-ms))
                              (recur (inc n) retry))))))))

(defn body
  "The parsed body of a `fetch` result, or nil. Use where a miss and a throttle
  are genuinely equivalent — never on the draft path."
  [resp]
  (when (:ok? resp) (:body resp)))

;; ---- raw endpoints ----
(defn user           [name-or-id] (fetch (str "/user/" name-or-id)))
(defn user-leagues   [uid season] (fetch (str "/user/" uid "/leagues/nfl/" season)))
(defn league         [lid]        (fetch (str "/league/" lid)))
(defn league-drafts  [lid]        (fetch (str "/league/" lid "/drafts")))
(defn league-rosters [lid]        (fetch (str "/league/" lid "/rosters")))
(defn draft          [did]        (fetch (str "/draft/" did)))
(defn draft-picks    [did]        (fetch (str "/draft/" did "/picks")))

(defn league-owners [lid]
  (keep :owner_id (body (league-rosters lid))))

;; ---- normalization ----
(defn- amount [pick] (some-> (get-in pick [:metadata :amount]) parse-long))

(defn reception-weight
  "The league's points-per-reception, which is what separates standard from
  half-PPR from PPR. Recorded rather than required — the corpus is deliberately
  heterogeneous now, but only useful if the shape stays visible."
  [lg]
  (let [rec (get-in lg [:scoring_settings :rec])]
    (when (number? rec) (double rec))))

(defn normalize-draft
  "Draft object + picks -> a replay-ready draft map. Picks carry only what the
  engine needs, sorted by nomination order.

  `lg` is optional and only supplies the scoring weight for slicing; the real
  scoring config comes from `league_import` at score time."
  ([d picks] (normalize-draft d picks nil))
  ([d picks lg]
   (let [s (:settings d)]
     {:draft-id    (:draft_id d)
      :season      (:season d)
      :num-teams   (:teams s)
      :budget      (or (:budget s) 200)
      :league-id   (:league_id d)
      :scoring-rec (reception-weight lg)
      :picks (->> picks
                  (keep (fn [p]
                          (when-let [a (amount p)]
                            {:player-id (:player_id p)
                             :position  (get-in p [:metadata :position])
                             :price     (double a)
                             :team-id   (str (:roster_id p))
                             :pick-no   (:pick_no p)})))
                  (sort-by :pick-no)
                  vec)})))

;; ---- corpus quality gate ----

(def min-priced-share
  "How much of a draft must carry a price before it is worth replaying. Below
  this the record is partial, and the missing picks are not missing at random —
  they are the ones nobody bothered to enter."
  0.95)

(def max-dollar-share
  "Above this share of $1 picks the draft was almost certainly back-filled
  offline rather than bid live. The user's own RaiderNation drafts fail here:
  a team $74 short, Ja'Marr Chase logged at $1."
  0.30)

(def min-teams
  "Below this many teams the draft is a mock or a test league, not a market.

  This is the one *shape* requirement that survives, and it is here on quality
  grounds rather than uniformity grounds — the distinction that matters, because
  demanding a single shape is what held this corpus at two drafts. The first
  widened crawl accepted a four-team league with a $1000 budget, and a room with
  four bidders does not price like a room with twelve: the same player is
  simultaneously scarcer and cheaper, so its realized prices would be noise in a
  corpus meant to calibrate against real auctions.

  Six admits small-but-real leagues and excludes mocks. It is reported as
  `:too-small` in the crawl histogram rather than dropped silently, so the cost
  of the floor is always visible."
  6)

(defn auction-decision
  "Is this draft usable corpus, and if not, why not?

  Returns `{:ok? bool :reason kw :meta {...}}`. The reason is the point: a crawl
  that only counts acceptances cannot tell a narrow gate from a thin population,
  which is exactly how this gate stayed at 12-team/$200/PPR while reporting a
  data shortage.

  What is *not* checked is as deliberate as what is. Budget and scoring are
  recorded, never required — `replay/core.clj` already configures the engine from
  each league's own settings via `league_import`, so a $250 half-PPR auction is as
  replayable as a $200 PPR one, and refusing it only shrinks the corpus. The old
  `every team spent at least 85% of budget` rule is gone too: leaving money on the
  table is a legitimate way to lose an auction, not evidence of a bad record. Team
  count survives only as a floor against mocks (`min-teams`), not as a fixed
  shape."
  [d picks lg]
  (let [s    (:settings d)
        amts (keep amount picks)
        meta {:num-teams   (:teams s)
              :budget      (or (:budget s) 200)
              :scoring-rec (reception-weight lg)
              :picks       (count picks)}
        no   (fn [reason] {:ok? false :reason reason :meta meta})]
    (cond
      (nil? d)                      (no :no-draft)
      (not= "auction" (:type d))    (no :not-auction)
      (not= "complete" (:status d)) (no :incomplete)
      (< (or (:teams s) 0) min-teams) (no :too-small)
      (empty? picks)                (no :no-picks)

      (< (/ (count amts) (double (count picks))) min-priced-share)
      (no :few-amounts)

      ;; every roster in the league has to show up, or we are replaying a draft
      ;; against a league the engine will size differently
      (not= (:teams s) (count (distinct (keep :roster_id picks))))
      (no :missing-teams)

      (> (/ (count (filter #(= 1 %) amts)) (double (max 1 (count amts)))) max-dollar-share)
      (no :dollar-defaulted)

      :else {:ok? true :reason :accepted :meta meta})))

(defn draft-shape
  "The cheap half of the gate — what the draft object alone rules out, before
  spending a request on its picks. nil means still a candidate.

  Worth separating because the widened gate probes every league now, and most
  drafts in the wild are snake. Fetching picks for all of them to learn that
  would multiply the crawl's request count for no information."
  [d]
  (cond
    (nil? d)                      :no-draft
    (not= "auction" (:type d))    :not-auction
    (not= "complete" (:status d)) :incomplete
    (< (or (:teams (:settings d)) 0) min-teams) :too-small))

;; ---- crawl ----

(def seasons
  "Seasons swept per user. 2021 is the earliest whose Sleeper projections are a
  genuine preseason freeze — 2019 and 2020 are contaminated (see
  `replay/universe.clj`) and replaying them would score the engine against a
  universe that already knew the answers."
  ["2025" "2024" "2023" "2022" "2021"])

(defn candidate-league?
  "Does this league summary have a draft at all?

  That is deliberately the whole filter. It used to also require 12 teams and
  full PPR, which is what kept half-PPR and non-$200 auctions out of the corpus
  before the gate ever saw them."
  [lg]
  (boolean (:draft_id lg)))

(defn league-chain
  "This league and its predecessors, following `previous_league_id` backwards.

  A keeper or dynasty league keeps its history in a linked list, one node per
  season, and reading only the newest node throws away every auction the league
  ever ran before this year. Bounded by `depth`, and it refuses to revisit an id
  it already has, so a cycle cannot run away.

  `seen?` stops the walk at any league already collected. Because a user's own
  league list already contains last season's node *and* this season's node chains
  back to it, without this the same league is walked — and its drafts probed —
  once per season it appears in."
  ([lg depth] (league-chain lg depth (constantly false)))
  ([lg depth seen?]
   (loop [cur lg n 0 acc []]
     (if (or (nil? cur) (>= n depth) (seen? (:league_id cur)))
       acc
       (let [acc'    (conj acc cur)
             prev-id (:previous_league_id cur)]
         (if (or (nil? prev-id) (some #(= prev-id (:league_id %)) acc'))
           acc'
           (recur (body (league prev-id)) (inc n) acc')))))))

(defn league-histories
  "Every distinct league in the combined history of `lgs`.

  Deduped as it goes rather than afterwards, so a predecessor already collected
  costs neither a fetch nor a second round of draft probes."
  [lgs depth]
  (loop [remaining lgs seen #{} acc []]
    (if-let [lg (first remaining)]
      (let [chain (league-chain lg depth seen)]
        (recur (rest remaining)
               (into seen (map :league_id) chain)
               (into acc chain)))
      acc)))

(defn probe-drafts
  "Every draft this league ran, each with its decision.

  Uses `/league/{id}/drafts` rather than the league's single `draft_id`: one
  request returns every draft that league ran, already carrying type, status and
  settings, so the cheap rejections cost nothing extra.

  `skip?` is asked before any per-draft request. A resumed crawl passes the ids
  it has already decided, so re-running costs a listing per league instead of a
  picks fetch per draft."
  [l {:keys [skip?]}]
  (for [d  (or (body (league-drafts (:league_id l))) [])
        :let  [did (:draft_id d)]
        :when (not (and skip? (skip? did)))
        :let  [shape (draft-shape d)]]
    (if shape
      {:draft-id did :decision {:ok? false :reason shape :meta {}}}
      (let [picks (body (draft-picks did))]
        {:draft-id did
         :decision (auction-decision d (or picks []) l)
         :draft    d
         :picks    picks
         :league   l}))))

(defn crawl
  "BFS from `seed-uids` over leaguemates, collecting the draft ids of usable
  auctions across every season and every league in a league's history.

  Returns a resumable state map: `{:accepted {draft-id meta} :reasons {reason n}
  :frontier [uid...] :seen-users #{} :seen-drafts #{} :seen-lgs #{} :examined n}`.
  Hand that map back as `:state` to continue a crawl that was interrupted or
  capped — a full sweep takes a while, and restarting it from the seed each time
  is how a corpus stays small.

  `:reasons` is the diagnostic that was missing. A crawl that reports only its
  acceptances cannot distinguish a narrow gate from a thin population; the
  histogram says which, and so says whether more sources are actually needed.

  `checkpoint!` is handed the same state map every `checkpoint-every` users. A
  full sweep runs for hours and will be interrupted; saving only on a clean
  finish means an interrupted crawl loses every request it made, which is the
  same way this corpus stayed small the first time."
  [seed-uids {:keys [max-drafts max-users expand-per-user chain-depth
                     progress! checkpoint! checkpoint-every state]
              :or   {max-drafts 500 max-users 3000 expand-per-user 8 chain-depth 4
                     checkpoint-every 20}}]
  (let [snapshot (fn [frontier seen-users seen-drafts seen-lgs accepted reasons examined]
                   {:accepted accepted :reasons reasons :visited (count seen-users)
                    :examined examined :frontier frontier :seen-users seen-users
                    :seen-drafts seen-drafts :seen-lgs seen-lgs})]
   (loop [frontier    (or (not-empty (:frontier state)) (vec (distinct seed-uids)))
          seen-users  (or (:seen-users state) #{})
          seen-drafts (or (:seen-drafts state) #{})
          seen-lgs    (or (:seen-lgs state) #{})
          accepted    (or (:accepted state) {})
          reasons     (or (:reasons state) {})
          examined    (or (:examined state) 0)]
    (if (or (empty? frontier)
            (>= (count accepted) max-drafts)
            (>= (count seen-users) max-users))
      (snapshot frontier seen-users seen-drafts seen-lgs accepted reasons examined)
      (let [uid (first frontier)]
        (if (seen-users uid)
          (recur (subvec frontier 1) seen-users seen-drafts seen-lgs accepted reasons examined)
          (let [leagues (mapcat #(body (user-leagues uid %)) seasons)
                cands   (filter candidate-league? leagues)
                ;; one visit per league, however many seasons and chains reach it
                probes  (->> (league-histories cands chain-depth)
                             (mapcat #(probe-drafts % {:skip? seen-drafts})))
                ;; expand via up to N leagues to move through the graph without an
                ;; unbounded rosters-fetch per user
                expand-lgs (->> leagues
                                (map :league_id)
                                (remove seen-lgs)
                                distinct
                                (take expand-per-user))
                owners     (distinct (mapcat league-owners expand-lgs))
                accepted'  (into accepted
                                 (comp (filter #(get-in % [:decision :ok?]))
                                       (map (juxt :draft-id #(get-in % [:decision :meta]))))
                                 probes)
                reasons'   (reduce (fn [m p] (update m (get-in p [:decision :reason]) (fnil inc 0)))
                                   reasons probes)]
            (when progress!
              (progress! {:uid uid :visited (inc (count seen-users))
                          :accepted (count accepted') :candidates (count cands)
                          :frontier (count frontier) :reasons reasons'}))
            (let [frontier'    (into (subvec frontier 1) (remove seen-users owners))
                  seen-users'  (conj seen-users uid)
                  seen-drafts' (into seen-drafts (map :draft-id) probes)
                  seen-lgs'    (into seen-lgs expand-lgs)
                  examined'    (+ examined (count probes))]
              (when (and checkpoint! (zero? (mod (count seen-users') checkpoint-every)))
                (checkpoint! (snapshot frontier' seen-users' seen-drafts' seen-lgs'
                                       accepted' reasons' examined')))
              (recur frontier' seen-users' seen-drafts' seen-lgs'
                     accepted' reasons' examined')))))))))
