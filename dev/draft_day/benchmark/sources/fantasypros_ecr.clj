(ns draft-day.benchmark.sources.fantasypros-ecr
  "Vintage expert-consensus RANKINGS (ECR) from archived FantasyPros cheatsheets.

  Why this exists when `fantasypros-archive` already pulls projections: the
  cheatsheet page is archived far more densely. Pre-kickoff captures exist for
  every season 2011-2025, against eight for the projections pages. And for a
  *ranking* benchmark a rank is a first-class input — we do not need a stat line
  to ask 'did this ordering hold up'.

  It also supplies its own universe. The cheatsheet IS the draftable pool, in
  order, roughly 270-370 players — so unlike the projection sources this needs no
  ADP list to say who was worth drafting, which is what lets it reach 2011 (six
  years before Fantasy Football Calculator's ADP begins).

  THE COST: a rank cannot be re-scored under a league's custom rules. Every other
  source here yields a stat line that `draft-day.scoring` re-prices; this one does
  not. So `:ecr` is a scoring-agnostic baseline, useful for the long view and for
  testing rank-based ideas over fifteen seasons, and never a substitute for the
  projection path on the seasons where that exists.

  SIX NAME FORMATS, ONE TABLE SHAPE. Every era is a server-rendered table with a
  name cell and a position-rank cell; only the name formatting moves:

    2011  'Arian Foster'                + separate 'RB' and 'HOU' cells
    2012  'Arian Foster (RB1, HOU, 8)'
    2013  'Adrian Peterson (MIN/5)'     + 'RB1'
    2014  'LeSean McCoy (PHI/7) P'      + 'RB1'
    2015  'Antonio Brown PIT, 11'       + 'WR1'
    2018  'Todd Gurley T. Gurley LAR'   + 'RB1'

  So the parser finds the position-rank cell, takes the shortest plausible name
  cell, and normalizes. Measured 96.4% GSIS join across 2011-2018, 95-97% per
  season; the residue is players DynastyProcess has no gsis for, not parse
  failures.

  THREE ERAS, ONE INTERFACE: the server-rendered table (2011-2020, six name
  formats) and the JavaScript payload (2021+, `var ecrData`). `parse-page` handles
  the former, `parse-ecr-json` the latter, and `season-data` tries them in order."
  (:require [clojure.string :as str]
            [draft-day.benchmark.fetch :as fetch]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]
            [draft-day.benchmark.sources.fantasypros-archive :as fp-archive]
            [draft-day.benchmark.sources.wayback :as wayback])
  (:import [org.jsoup Jsoup]
           [org.jsoup.parser Parser]))

(defn cell-text
  "Text of a table cell with tag boundaries turned into spaces.

  Jsoup's `.text()` concatenates inline elements without a separator, so
  `<a>Todd Gurley</a><span>T. Gurley</span>` becomes \"Todd GurleyT. Gurley\" —
  and the abbreviated-duplicate rule below, which keys on whitespace before the
  initial, then fails to fire. Replacing tags with a space matches how the parse
  was validated and avoids relaxing that rule into one that would mangle a
  genuine name like \"A.J. Brown\"."
  [el]
  (-> (.html el)
      (str/replace #"<[^>]+>" " ")
      (Parser/unescapeEntities false)
      (str/replace #"\s+" " ")
      str/trim))

(def page-url
  "https://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php")

(def ^:private page-prefix "fantasypros.com/nfl/rankings/")

(def scoring-positions #{"QB" "RB" "WR" "TE"})

(def pos-rank-pattern #"^(QB|RB|WR|TE|K|DST|DEF)\d+$")

(defn best-capture
  "Latest pre-kickoff capture of the cheatsheet for a season, or nil. Reuses the
  projection source's `openers` table — the kickoff date is a fact about the
  season, not about the page."
  [season]
  (->> (wayback/captures page-prefix (dec season) season)
       (filter (fn [{:keys [original timestamp]}]
                 (and (str/includes? (str/lower-case original)
                                     "/nfl/rankings/consensus-cheatsheets.php")
                      (fp-archive/preseason? season timestamp))))
       (map :timestamp)
       sort
       last))

(defn clean-name
  "Strip the era-specific decoration off a name cell.

  Order matters: parentheses first (they can contain commas and slashes), then
  the abbreviated duplicate ('Todd Gurley T. Gurley' truncates at 'T.'), then a
  trailing bye number, team abbreviation, and single-letter note flag."
  [s]
  (-> (str s)
      (str/replace #"\([^)]*\)" " ")
      (str/split #"\s+[A-Z]\.\s") first
      (str/replace #"\s*,\s*\d+\s*$" "")
      (str/replace #"\s+[A-Z]{2,3}\s*$" "")
      (str/replace #"\s+[A-Z]\s*$" "")
      (str/replace #"\s+" " ")
      str/trim))

(defn cell-position
  "Position implied by a row's cells: a 'RB1' style rank, a bare 'RB', or the
  parenthesised '(RB1, HOU, 8)' form."
  [cells]
  (or (some (fn [c] (second (re-matches pos-rank-pattern c))) cells)
      (some (fn [c] (when (scoring-positions c) c)) cells)
      (second (re-find #"\((QB|RB|WR|TE)\d+," (str/join " " cells)))))

(defn name-cell
  "The most name-like cell.

  A player name is SHORT. Cheatsheet rows also carry analyst commentary hundreds
  of characters long, and picking the cell with the most letters selects those
  instead — which silently dropped 2017 from 97% to 41% before the length cap."
  [cells]
  (->> cells
       (remove #(re-matches pos-rank-pattern %))
       (filter #(re-find #"[A-Za-z]{3,}" %))
       (filter #(and (<= (count %) 40)
                     (<= (count (str/split % #"\s+")) 6)))
       (sort-by #(- (count (re-seq #"[A-Za-z]" %))))
       first))

(defn parse-page
  "Archived cheatsheet HTML -> [{:player-name :position :ecr}] in rank order.
  Empty for the JavaScript-rendered era, which has no player rows to find."
  [html]
  (if (str/blank? html)
    []
    (let [doc  (Jsoup/parse html)
          rows (.select doc "tr")
          parsed (keep (fn [tr]
                         (let [cells (mapv cell-text (.select tr "td"))]
                           (when (>= (count cells) 3)
                             (let [pos (cell-position cells)]
                               (when (scoring-positions pos)
                                 (when-let [nm (some-> (name-cell cells) clean-name not-empty)]
                                   (when (> (count nm) 2)
                                     {:player-name nm :position pos})))))))
                       rows)]
      ;; Rank is row order within the cheatsheet, which is the ECR ordering
      ;; itself. Reading the printed rank cell would be era-specific and buys
      ;; nothing. De-duplicate: some pages repeat a player across sub-tables.
      (->> parsed
           (reduce (fn [{:keys [seen out]} p]
                     (let [k [(str/lower-case (:player-name p)) (:position p)]]
                       (if (seen k)
                         {:seen seen :out out}
                         {:seen (conj seen k)
                          :out  (conj out (assoc p :ecr (inc (count out))))})))
                   {:seen #{} :out []})
           :out))))

(defn parse-ecr-json
  "The 2021+ era: the cheatsheet is rendered client-side from `var ecrData = {…}`,
  so the HTML has no player rows at all and the table parser correctly finds
  nothing.

  The payload is richer than any HTML era — alongside the rank it carries
  `rank_std` (the spread of expert opinion, exactly the input
  `rankings/projections.clj` uses for floor/ceiling bands) and `tier` (what
  `rankings/tiers.clj` computes) — both now available as of draft day."
  [html]
  (when-let [m (re-find #"var\s+ecrData\s*=\s*(\{.*?\});\s*\n" (str html))]
    (try
      (let [d (json/read-value (second m) mapper)]
        (into []
              (keep (fn [p]
                      (let [pos (:player_position_id p)
                            r   (:rank_ecr p)]
                        (when (and (scoring-positions pos) (number? r))
                          (cond-> {:player-name (str/trim (str (:player_name p)))
                                   :position    pos
                                   :ecr         (long r)
                                   :fp-id       (some-> (:player_id p) str)}
                            (:rank_std p) (assoc :rank-std (fetch/num-or-nil (str (:rank_std p))))
                            (:tier p)     (assoc :tier (long (:tier p))))))))
              (:players d)))
      (catch Exception _ nil))))

(defn season-data
  "{:season :timestamp :players [...]} for a season, disk-cached."
  [season]
  (fetch/cached
   (fetch/cache-path "fp-ecr" "v1" season)
   (fn []
     (if-let [ts (best-capture season)]
       (let [{:keys [html]} (wayback/fetch-snapshot ts page-url)
             ;; Server-rendered eras first; 2021+ falls through to the JSON blob.
             players (or (seq (parse-page html)) (parse-ecr-json html) [])]
         {:season season :timestamp ts :players (vec players)})
       {:season season :timestamp nil :players []}))))

(defn coverage
  "Per-season capture date and row count, for --source-report."
  [seasons]
  (mapv (fn [s]
          (let [{:keys [timestamp players]} (season-data s)]
            {:season s
             :capture (some-> timestamp (subs 0 8))
             :n (count players)
             :by-position (into (sorted-map) (frequencies (map :position players)))}))
        seasons))
