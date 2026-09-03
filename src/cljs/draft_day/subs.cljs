(ns draft-day.subs
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [draft-day.db :as db]
            [draft-day.scoring :as scoring]))

;; ---- simple extracts ----
(doseq [k [:view :status :config :teams :my-team-id :players
           :nominated-id :sort :pos-filter :search :columns :drafted :ranked :modal
           :watchlist :import-report :universe
           :league-sync :my-roster-id :waivers :waiver-sort :waiver-status
           :waiver-columns]]
  (rf/reg-sub k (fn [dbv _] (get dbv k))))

;; :custom when :scoring is a full {stat weight} map (hand-edited or imported),
;; otherwise the active preset keyword itself (:standard/:half-ppr/:ppr).
(rf/reg-sub :scoring-mode :<- [:config]
  (fn [cfg _] (let [s (:scoring cfg)] (cond (map? s) :custom (keyword? s) s :else :ppr))))

;; The format whose vendor columns this league actually reads. Resolution is
;; shared with the server (`routes/resolve-scoring`) so the format warned about
;; here is always the one `rankings.vendor` flattened.
(rf/reg-sub :scoring-format :<- [:config]
  (fn [cfg _] (scoring/format-of (scoring/resolve-config (:scoring cfg)))))

(defn source-gap?
  "Did this `:sources` report actually deliver anything usable?

  `:ok? false` is only the loud half. A scrape that answers 200 and parses to
  nothing reports `{:ok? true :rows 0}` — which is what a vendor renaming a JSON
  key looks like — and a join whose match keys have drifted reports plenty of
  rows and `:matched 0`. Both leave every column blank, and keying the warning
  off `:ok?` alone let both through silently."
  [{:keys [ok? rows matched]}]
  (or (false? ok?)
      (not (pos? (or rows 0)))
      (not (pos? (or matched 0)))))

(def vendor-gap-sources
  "The format-scoped FantasyPros halves, in the order the notice names them."
  [:fantasypros/ecr :fantasypros/aav])

(rf/reg-sub :vendor-gaps
  :<- [:scoring-format]
  :<- [:universe]
  ;; FantasyPros is scraped once per scoring format, and each scrape is
  ;; independently best-effort — so the standard cheatsheet can fail while PPR
  ;; succeeds, and the cache is then served for a day with market prices for
  ;; some leagues and not others. Only this league's format matters here, which
  ;; is why the check cannot live server-side: the universe is shared.
  ;;
  ;; A label the universe does not mention at all is not judged: the bundled
  ;; sample predates provenance, and "we never looked" is not "it is missing".
  (fn [[fmt universe] _]
    (let [sources (:sources universe)]
      (into []
            (keep (fn [source]
                    (when-let [report (get sources (scoring/format-label source fmt))]
                      (when (source-gap? report) source))))
            vendor-gap-sources))))

(rf/reg-sub :ranked-players :<- [:ranked] (fn [r _] (:players r)))

(rf/reg-sub :players-by-id :<- [:ranked]
  (fn [r _] (into {} (map (juxt :player-id identity)) (:players r))))

;; The raw universe, not the ranked board. `/api/rankings` deliberately strips
;; :nflverse/history before it ranks (see `routes/without-history`) because that
;; response is re-sent after every pick, so the season lines are only ever on the
;; universe `/api/players` fetched once. Static facts come from here; live
;; valuation comes from :players-by-id.
(rf/reg-sub :universe-by-id :<- [:players]
  (fn [ps _] (into {} (map (juxt :player-id identity)) ps)))

(rf/reg-sub :market :<- [:ranked]
  (fn [r _] (select-keys r [:inflation :inflation-index :market-heat :market-multiplier])))

(rf/reg-sub :visible-columns :<- [:columns]
  (fn [cols _] (filterv :visible? cols)))

;; ---- board: filter to undrafted, apply pos/search, rank by worth, sort ----

(defn- matches-search? [p q]
  (or (str/blank? q)
      (str/includes? (str/lower-case (or (:player-name p) "")) q)
      (str/includes? (str/lower-case (or (:team p) "")) q)))

(defn sort-players
  "Sort by the active column, breaking ties on `rank-key` — the board's own total
  order.

  Ties are the common case, not the edge: Worth is whole dollars and the whole
  minimum-bid tail sits on the same one, so sorting on Worth alone left a
  96-player block in whatever order the server emitted while the `#` column kept
  showing each row's real rank, which reads as a broken board rather than as a
  tie.

  The fallback is `db/rank-key` rather than the `:rank` field because `:rank` is
  assoc'd by `:board-players` a few lines below and nothing here could enforce
  that: `(compare nil nil)` is 0, so any other caller — the watch list's
  one-shot re-sort, My Roster — would silently get the arbitrary server order
  back with no error. `rank-key` is self-contained and cannot be called wrong,
  which is also why it now lives in `db.cljc` alongside its other callers.

  Ties deliberately do *not* invert with `dir`: a tie is not an ordering, so both
  directions show the better player first. Only the sorted column reverses."
  [players key dir]
  (let [acc (get db/sort-accessors key :worth)]
    (sort (fn [a b]
            (let [va (acc a) vb (acc b)]
              (cond
                (and (nil? va) (nil? vb)) (compare (db/rank-key a) (db/rank-key b))
                (nil? va) 1
                (nil? vb) -1
                :else (let [c (* dir (compare va vb))]
                        (if (zero? c) (compare (db/rank-key a) (db/rank-key b)) c)))))
          players)))

(defn sort-waiver-players
  "`sort-players` over the waiver catalog: same nil-last rule, same
  tie-into-total-order discipline, different accessors and a different total
  order (`db/waiver-rank-key`, which leads with Upgrade rather than Worth).

  Not folded into `sort-players` with a catalog argument, because the two differ
  in *both* halves — accessors and fallback — and a shared function taking both
  would be threading two parameters through every call site to save six lines."
  [players key dir]
  (let [acc (get db/waiver-sort-accessors key :upgrade)]
    (sort (fn [a b]
            (let [va (acc a) vb (acc b)]
              (cond
                (and (nil? va) (nil? vb)) (compare (db/waiver-rank-key a) (db/waiver-rank-key b))
                (nil? va) 1
                (nil? vb) -1
                :else (let [c (* dir (compare va vb))]
                        (if (zero? c)
                          (compare (db/waiver-rank-key a) (db/waiver-rank-key b))
                          c)))))
          players)))

;; undrafted, unfiltered by position/search — the pool the board and watch list
;; draw from, so they don't collapse when the board is filtered by pos/search.
(rf/reg-sub :undrafted-players
  :<- [:ranked-players]
  :<- [:drafted]
  (fn [[players drafted] _]
    (let [drafted-ids (set (keys drafted))]
      (remove #(contains? drafted-ids (:player-id %)) players))))

(rf/reg-sub :board-players
  :<- [:undrafted-players]
  :<- [:sort]
  :<- [:pos-filter]
  :<- [:search]
  (fn [[players sort pos-filter search] _]
    (let [q           (str/lower-case (or search ""))
          filtered    (->> players
                           (filter #(or (nil? pos-filter) (= (:position %) pos-filter)))
                           (filter #(matches-search? % q)))
          ;; live overall rank by Worth then VORP then points (see `rank-key`),
          ;; independent of the active sort column
          rank-map    (into {} (map-indexed (fn [i p] [(:player-id p) (inc i)])
                                            (sort-by db/rank-key filtered)))
          scale       (db/tier-scale pos-filter)
          ;; :tier is resolved here, shadowing the flat alias the server ships,
          ;; so every consumer downstream — row striping, the legend, the Tier
          ;; column, sorting — keeps reading one key while the scale moves under
          ;; it. The server sends both scales, so a position filter switches
          ;; scale with no refetch and no :recompute.
          ranked      (map #(assoc % :rank (rank-map (:player-id %))
                                     :tier (db/player-tier % scale)
                                     ;; FantasyPros publishes both scales too, so
                                     ;; the FP T column tracks ours rather than
                                     ;; answering a different question beside it
                                     :fantasypros/ecr-tier (db/fp-tier % scale))
                           filtered)]
      (vec (sort-players ranked (:key sort) (:dir sort))))))

;; ---- my team / roster ----

(rf/reg-sub :my-team :<- [:teams] :<- [:my-team-id]
  (fn [[teams id] _] (first (filter #(= (:team-id %) id) teams))))

;; My roster's bye exposure (starters/bench/open non-bench slot count). Drives the
;; board's red pulse and the roster's amber marker. Recomputes after every
;; pick/undo since it derives from :teams and :ranked.
(rf/reg-sub :my-bye-exposure :<- [:my-team] :<- [:players-by-id]
  (fn [[team by-id] _] (db/roster-exposure team by-id)))

;; Starter player-ids whose bye week has no bench cover, once the lineup is full
;; (empty before that). Colors the My Roster Bye column amber.
(rf/reg-sub :my-uncovered-starters :<- [:my-bye-exposure]
  (fn [exposure _] (db/uncovered-starter-ids exposure)))

(defn- open-slots [team] (count (filter #(nil? (:player-id %)) (:roster team))))

(rf/reg-sub :my-max-bid :<- [:my-team]
  (fn [team _] (when team (max 1 (- (:bankroll team) (dec (open-slots team)))))))

;; Pooled per-bucket availability for MY ROSTER open slots: each open slot in a
;; budget bucket shows (plan − spent) ÷ open slots, floored. Spend counts against
;; the bucket of the slot a player actually fills (a WR parked in FLEX charges
;; FLEX). Buckets with no plan set get no entry, so the column stays blank.
(rf/reg-sub :budget-avail
  :<- [:my-team]
  :<- [:drafted]
  :<- [:config]
  (fn [[team drafted cfg] _]
    (let [plan          (:budget-plan cfg)
          price-of      (fn [player-id] (get-in drafted [player-id :price] 0))
          ;; tally spent + open slots per budget bucket
          bucket-totals (reduce (fn [acc {:keys [pos player-id]}]
                                  (let [budget (db/slot->budget-key pos)]
                                    (if player-id
                                      (update-in acc [budget :spent] (fnil + 0) (price-of player-id))
                                      (update-in acc [budget :open]  (fnil inc 0)))))
                                {} (:roster team))]
      ;; (planned − spent) ÷ open, per bucket that has a plan and open slots
      (reduce-kv (fn [acc budget {:keys [spent open]}]
                   (let [planned (get plan budget 0)]
                     (if (and (pos? planned) 
                              (pos? (or open 0)))
                       (assoc acc budget (js/Math.floor (/ (- planned (or spent 0)) open)))
                       acc)))
                 {} bucket-totals))))

;; ---- watch list ----

;; The ids as a set, for the board's star membership test.
(rf/reg-sub :watch-set :<- [:watchlist] (fn [w _] (set w)))

;; Watched players in the manager's own order — the vector's. Nothing sorts them:
;; the list says who he means to nominate next, which is not a fact about Worth,
;; and a row that moved on its own after a pick would make the order untrustable.
;;
;; Drafted players fall out here rather than through an event, so a pick (or its
;; undo) is reflected automatically without disturbing the order of the rest.
(rf/reg-sub :watchlist-players
  :<- [:players-by-id]
  :<- [:watchlist]
  :<- [:drafted]
  (fn [[by-id watchlist drafted] _]
    (->> watchlist
         (remove #(contains? drafted %))
         (keep by-id)
         vec)))

;; ---- waiver board ----

(rf/reg-sub :league-synced? :<- [:league-sync]
  (fn [ls _] (boolean (seq (:teams ls)))))

(rf/reg-sub :sync-teams :<- [:league-sync]
  (fn [ls _] (vec (:teams ls))))

;; Which league the persisted rosters came from, so a re-sync is one click. It
;; rides on the sync reply rather than being stored separately, because the two
;; must not be able to disagree about which league is on screen.
(rf/reg-sub :synced-league-id :<- [:league-sync]
  (fn [ls _] (:league-id ls)))

;; Guarded because it is persisted and `sync-panel` binds it straight into an
;; input's `:value`, where a non-string is a render error rather than a bad
;; value. Every other persisted key with a shape gets a `db/reconcile-*` at
;; boot; these two are scalars, and one line here is cheaper than a repair
;; function that #42 would delete along with the rest of that approach.
;;
;; No `:sleeper-user-id` sub: its only reader is `:league-synced`, which takes it
;; off the map directly because it is an event handler, not a view.
(rf/reg-sub :sleeper-username
  (fn [db _] (let [n (:sleeper-username db)] (when (string? n) n))))

;; nil means "never looked up"; [] means "looked up, plays in none this season".
;; The panel says different things for the two, so this does not normalize them.
(rf/reg-sub :league-choices (fn [db _] (:league-choices db)))

(rf/reg-sub :visible-waiver-columns :<- [:waiver-columns]
  (fn [cols _] (filterv :visible? cols)))

;; What the manager has left to spend, and what it would take to be sure of a
;; claim. Straight from the server rather than recomposed here: `:faab-left`
;; needs the league's budget and the roster's spend, which arrive in different
;; documents, and `rankings.waiver` already joined them once.
(rf/reg-sub :my-faab :<- [:waivers]
  (fn [w _] (:faab w)))

(rf/reg-sub :waiver-meta :<- [:waivers]
  (fn [w _] (select-keys w [:through-week :season-games :claims-left])))

;; The manager's own seats. nil and [] mean different things here and the panel
;; draws them differently — nil is "no team picked yet", [] is "this team holds
;; nobody" — so this deliberately does not normalize one into the other.
(rf/reg-sub :my-waiver-roster :<- [:waivers]
  (fn [w _] (:my-roster w)))

;; Which week the board is showing, as one of three answers — and three, not
;; two, is the point. `:waivers` is nil before the first reply and stays nil
;; after a failed one, so a boolean `(pos? (or through-week 0))` reports
;; *preseason* whenever the board simply has not loaded. In week 10 that put an
;; accented banner reading "no games played yet" over a loading screen, and left
;; it there permanently if the request errored — which is the exact misreading
;; the banner exists to prevent.
;;
;; :unknown = no board yet; 0 = genuinely preseason; a week = in season.
(rf/reg-sub :season-phase :<- [:waivers]
  (fn [w _]
    (let [wk (:through-week w)]
      (cond (nil? wk)  :unknown
            (pos? wk)  :in-season
            :else      :preseason))))

(rf/reg-sub :waiver-players
  :<- [:waivers]
  :<- [:waiver-sort]
  :<- [:pos-filter]
  :<- [:search]
  (fn [[w sort pos-filter search] _]
    (let [q        (str/lower-case (or search ""))
          filtered (->> (:players w)
                        (filter #(or (nil? pos-filter) (= (:position %) pos-filter)))
                        (filter #(matches-search? % q)))
          ;; The board's own order, independent of the active sort column —
          ;; exactly as `:board-players` does it, so the `#` column keeps
          ;; meaning "where he ranks" and not "which row he is on".
          rank-map (into {} (map-indexed (fn [i p] [(:player-id p) (inc i)])
                                         (sort-by db/waiver-rank-key filtered)))
          ranked   (map #(assoc % :rank (rank-map (:player-id %))) filtered)]
      (vec (sort-waiver-players ranked (:key sort) (:dir sort))))))

;; Rostered players matching the current search, with who holds them.
;;
;; This is what `:rostered` is *for*: search "Chase" while he is on somebody's
;; roster and the free-agent table is simply empty, which teaches the manager
;; nothing. Names come from the universe the browser already has rather than
;; from the wire, so answering "who has him" costs no payload at all.
(rf/reg-sub :rostered-matches
  :<- [:waivers]
  :<- [:universe-by-id]
  :<- [:search]
  (fn [[w by-id search] _]
    (let [q (str/lower-case (or search ""))]
      (when-not (str/blank? q)
        (->> (:rostered w)
             (keep (fn [[id team]]
                     (when-let [p (get by-id id)]
                       (when (matches-search? p q)
                         {:player-name (:player-name p)
                          :position    (:position p)
                          :team        team}))))
             (sort-by :player-name)
             vec)))))
