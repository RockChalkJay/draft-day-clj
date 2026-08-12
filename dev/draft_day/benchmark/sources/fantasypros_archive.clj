(ns draft-day.benchmark.sources.fantasypros-archive
  "Vintage preseason projections scraped from archived FantasyPros draft pages.

  Why this exists: Sleeper's frozen projections only reach 2021, and they are a
  single provider's numbers (every entry reads `company: rotowire`). FantasyPros
  publishes a consensus of many analysts and the Internet Archive has captured it
  since 2015, which both extends the benchmark window and — more valuably — gives
  a second, independent projection to check the first against.

  TWO HAZARDS, both handled here rather than left to the caller:

  1. THE PAGE IS NOT FROZEN. FantasyPros recomputes the draft consensus during
     the season. Only captures strictly before that season's Week 1 kickoff are
     accepted; see `openers`. Everything else is discarded, however tempting.

  2. THE COLUMN ORDER CHANGED. In 2015 a WR row ran rushing-then-receiving; by
     2023 it ran receiving-then-rushing. Anything keyed on column position would
     silently read rushing yards as receiving yards for half the corpus. So the
     header is parsed instead: the group row (RECEIVING/RUSHING/PASSING/MISC,
     with colspans) is expanded and zipped against the sub-header row (REC/YDS/
     TDS/...), and the pair identifies the stat. Order becomes irrelevant.

  A third caveat is reported rather than fixed: capture dates vary in quality.
  2023 was captured Sept 6 (excellent), but 2024's WR page was captured April 7 —
  before the NFL draft, so no rookies and stale depth charts. `coverage` surfaces
  the date per season/position so a weak vintage is visible instead of silently
  dragging a season's numbers down."
  (:require [clojure.string :as str]
            [draft-day.benchmark.fetch :as fetch]
            [draft-day.benchmark.sources.wayback :as wayback])
  (:import [org.jsoup Jsoup]))

(def openers
  "Week 1 Thursday kickoff per season. A capture on or after this date may carry
  in-season revisions and is rejected.

  Covers 2010 onward because the archived ECR cheatsheets reach back to 2011 —
  a season absent here is refused outright by `preseason?` rather than guessed,
  which is correct but silently cost 2011-2014 until the table was extended."
  {2010 "0909" 2011 "0908" 2012 "0905" 2013 "0905" 2014 "0904"
   2015 "0910" 2016 "0908" 2017 "0907" 2018 "0906" 2019 "0905" 2020 "0910"
   2021 "0909" 2022 "0908" 2023 "0907" 2024 "0905" 2025 "0904" 2026 "0910"})

(def positions ["qb" "rb" "wr" "te"])

(def ^:private page-prefix "fantasypros.com/nfl/projections/")

(defn page-url [pos]
  (str "https://www.fantasypros.com/nfl/projections/" pos ".php?week=draft"))

(defn preseason?
  "Is this capture timestamp strictly before season S's kickoff?"
  [season timestamp]
  (boolean
   (when-let [opener (get openers season)]
     (and (= (str season) (subs timestamp 0 4))
          (neg? (compare (subs timestamp 4 8) opener))))))

(defn best-capture
  "The LATEST pre-kickoff capture of a position's draft page for a season, or nil.
  Latest because a projection published in August has seen free agency, the draft
  and camp; one from April has seen none of them."
  [season pos]
  (let [want (str "/nfl/projections/" pos ".php")]
    (->> (wayback/captures page-prefix (- season 1) season)
         (filter (fn [{:keys [original timestamp]}]
                   (and (str/includes? (str/lower-case original) want)
                        (str/includes? (str/lower-case original) "week=draft")
                        (preseason? season timestamp))))
         (map :timestamp)
         sort
         last)))

;; ---- parsing ----

(def stat-for
  "(column group, sub-header) -> the Sleeper stat key `draft-day.scoring` speaks.
  Pairs with no entry (ATT, CMP, FPTS) are simply not scored: FPTS especially is
  FantasyPros' own scoring, and the whole point is to re-score the raw line under
  the league's rules."
  {["PASSING" "YDS"]  :pass_yd
   ["PASSING" "TDS"]  :pass_td
   ["PASSING" "INTS"] :pass_int
   ["RUSHING" "YDS"]  :rush_yd
   ["RUSHING" "TDS"]  :rush_td
   ["RECEIVING" "REC"] :rec
   ["RECEIVING" "YDS"] :rec_yd
   ["RECEIVING" "TDS"] :rec_td
   ["MISC" "FL"]      :fum_lost})

(defn expand-groups
  "Group-header cells -> one label per underlying column, honouring colspan."
  [cells]
  (into []
        (mapcat (fn [el]
                  (let [span (try (Integer/parseInt (.attr el "colspan"))
                                  (catch Exception _ 1))]
                    (repeat (max 1 span) (str/upper-case (str/trim (.text el)))))))
        cells))

(defn column-keys
  "Pure-ish: a projections table -> a vector of stat keys aligned to its <td>
  columns, nil where the column is not a scored stat. Returns nil when the header
  cannot be read, so a layout change fails loudly instead of mis-mapping."
  [table]
  (let [rows (.select table "thead tr")]
    (when (>= (.size rows) 2)
      (let [groups (expand-groups (.select (.get rows 0) "td, th"))
            subs*  (mapv #(str/upper-case (str/trim (.text %)))
                         (.select (.get rows 1) "th, td"))]
        (when (= (count groups) (count subs*))
          (mapv (fn [g s] (get stat-for [g s])) groups subs*))))))

(defn parse-number [s]
  (let [t (str/replace (str s) #"[,\s]" "")]
    (when (re-matches #"-?\d+(\.\d+)?" t)
      (Double/parseDouble t))))

(defn row-name
  "Player name from a row, tolerating both markup eras.

  2023 wraps the name in `<a class=\"player-name\">`; 2015 uses a bare anchor with
  no class, so a selector-only approach NPEs on half the corpus. The
  `fp-player-name` attribute is present in both and is tried first."
  [tr]
  (or (some-> (.selectFirst tr "[fp-player-name]") (.attr "fp-player-name")
              str/trim not-empty)
      (some-> (.selectFirst tr "a.player-name") .text str/trim not-empty)
      (some-> (.selectFirst tr "td a") .text str/trim not-empty)))

(defn row-fp-id
  "FantasyPros player id, from the row class in either era."
  [tr]
  (let [cls (str (.attr tr "class"))]
    (or (second (re-find #"mpb-player-(\d+)" cls))
        (second (re-find #"fp-id-(\d+)" (str (.html tr)))))))

(defn parse-row
  "One <tr> -> {:fp-id :player-name :stats}, or nil if it is not a player row."
  [tr col-keys]
  (let [fp-id (row-fp-id tr)
        name* (row-name tr)
        tds   (.select tr "td")]
    (when (and name* (pos? (.size tds)))
      {:fp-id       fp-id
       :player-name (str/trim name*)
       :stats       (into {}
                          (keep (fn [i]
                                  (when-let [k (nth col-keys i nil)]
                                    (when-let [v (parse-number (.text (.get tds i)))]
                                      [k v]))))
                          (range (min (.size tds) (count col-keys))))})))

(defn parse-page
  "Archived FantasyPros projections HTML -> [{:fp-id :player-name :stats}].
  Empty when the table or its header cannot be found."
  [html]
  (if (str/blank? html)
    []
    (let [doc   (Jsoup/parse html)
          table (or (.selectFirst doc "table#data") (.selectFirst doc "table"))]
      (if-not table
        []
        (if-let [col-keys (column-keys table)]
          (into [] (keep #(parse-row % col-keys)) (.select table "tbody tr"))
          [])))))

;; ---- assembly ----

(defn season-position
  "{:timestamp :players [...]} for one season/position, or nil if no usable
  pre-kickoff capture exists.

  A failed archive fetch degrades this one position to unavailable rather than
  aborting the run: archive.org resets connections under load, and a five-season
  backfill that dies on the nineteenth page — discarding eighteen good fetches —
  is unusable. Successful fetches are already cached, so re-running resumes.
  The season is then reported as an incomplete capture set, never as data."
  [season pos]
  (try
    (when-let [ts (best-capture season pos)]
      (let [{:keys [html]} (wayback/fetch-snapshot ts (page-url pos))
            players        (parse-page html)]
        (when (seq players)
          {:timestamp ts
           :position  (str/upper-case pos)
           :players   (mapv #(assoc % :position (str/upper-case pos)) players)})))
    (catch Exception e
      (binding [*out* *err*]
        (println (format "  [fp-archive] %d %s unavailable: %s"
                         season (str/upper-case pos) (ex-message e))))
      nil)))

(defn season-data
  "All four skill positions for a season:
  {:season :captures {pos timestamp} :players [...]}.

  A season missing any skill position is still returned — the caller decides
  whether a partial board is usable, and `coverage` makes the gap visible."
  [season]
  (fetch/cached
   (fetch/cache-path "fp-archive" "v1" season)
   (fn []
     (let [parts (keep #(season-position season %) positions)]
       {:season   season
        :captures (into (sorted-map) (map (juxt :position :timestamp)) parts)
        :players  (into [] (mapcat :players) parts)}))))

(defn coverage
  "Per-season capture dates and row counts, for --source-report. The capture date
  is the quality signal: August beats April by a wide margin."
  [seasons]
  (mapv (fn [s]
          (let [{:keys [captures players]} (season-data s)]
            {:season   s
             :captures captures
             :n        (count players)
             :complete? (= 4 (count captures))}))
        seasons))
