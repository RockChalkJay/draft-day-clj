(ns draft-day.benchmark.sources.fftoday
  "Vintage preseason projections from FFToday — the deepest projection source
  found, and the only one that needs no archive.

  `fftoday.com/rankings/playerproj.php?Season=Y&PosID=P&LeagueID=1` serves past
  seasons from the live site, 2008-2025, two pages of 50 per position, with full
  stat lines that re-score under a custom league. Eighteen seasons against
  Sleeper's five and the Internet Archive's eight, with none of the archive's
  rate limiting or markup drift.

  VINTAGE EVIDENCE. FFToday publishes no capture date, so the check is
  behavioural and the same in spirit as Sleeper's flat-`gp` test: a preseason
  board still contains players who went on to miss the season. The 2018 running
  back board is led by Todd Gurley with Le'Veon Bell third — Bell held out the
  entire 2018 season and scored nothing — and both 2021 and 2024 open with
  Christian McCaffrey, who played 7 and 4 games. A backfilled board could not
  look like that. `gate` scores this automatically by correlating projected
  points against realized games played; a source that knew about injuries would
  show a strong positive relationship.

  COLUMN ORDER FLIPS BY POSITION. Running backs run Rushing-then-Receiving,
  wide receivers Receiving-then-Rushing. Anything keyed on column index would
  read rushing yards as receiving yards for one of them, so the group header row
  (with colspans) is expanded and zipped against the column header row, exactly
  as in `fantasypros-archive`."
  (:require [clojure.string :as str]
            [draft-day.benchmark.fetch :as fetch]
            [draft-day.benchmark.sources.fantasypros-archive :as fp-archive])
  (:import [org.jsoup Jsoup]))

(def position-ids
  "FFToday PosID -> our position label."
  {10 "QB" 20 "RB" 30 "WR" 40 "TE"})

(def pages
  "Two pages of 50 covers every draftable player at a position."
  [0 1])

(def first-season 2008)
(def last-season 2025)

(defn page-url [season pos-id page]
  (str "https://www.fftoday.com/rankings/playerproj.php?Season=" season
       "&PosID=" pos-id "&LeagueID=1&cur_page=" page))

(def stat-for
  "(column group, column header) -> the Sleeper stat key `draft-day.scoring`
  speaks. Cmp and Att are not scored; FPts is FFToday's own scoring and is
  deliberately dropped so the line re-prices under the league's rules."
  {["PASSING" "YDS"]   :pass_yd
   ["PASSING" "TD"]    :pass_td
   ["PASSING" "INT"]   :pass_int
   ["RUSHING" "YDS"]   :rush_yd
   ["RUSHING" "TD"]    :rush_td
   ["RECEIVING" "REC"] :rec
   ["RECEIVING" "YDS"] :rec_yd
   ["RECEIVING" "TD"]  :rec_td})

(defn row-cells
  "DIRECT child <td>s of a row.

  Not `.select \"td\"`: Jsoup's select descends into nested tables, and FFToday
  wraps the whole projections table inside an outer layout table. The wrapper row
  then reports 578 cells instead of 11, every stat index lands on page chrome,
  and the parse yields rows with correct names and empty stat lines — a failure
  that looks like missing data rather than a bug."
  [tr]
  (into [] (filter #(= "td" (.tagName %))) (.children tr)))

(defn header-label
  "Column header text. The Player cell carries a nest of sort links, so prefer
  the bold label when present rather than the cell's whole text."
  [el]
  (let [b (.selectFirst el "b")]
    (-> (if b (.text b) (.text el))
        (str/replace #"\s+" " ")
        str/trim
        str/upper-case)))

(defn column-keys
  "Stat key per column, nil where the column is not a scored stat. nil overall
  when the header cannot be aligned, so a layout change fails loudly."
  [table]
  (let [grp (.selectFirst table "tr.tablehdr")
        hdr (.selectFirst table "tr.tableclmhdr")]
    (when (and grp hdr)
      (let [groups (fp-archive/expand-groups (row-cells grp))
            cols   (mapv header-label (row-cells hdr))]
        (when (= (count groups) (count cols))
          (mapv (fn [g c] (get stat-for [g c])) groups cols))))))

(defn parse-row
  "One data row -> {:fftoday-id :player-name :team :stats}, or nil."
  [tr col-keys]
  (let [tds (row-cells tr)]
    (when (= (count tds) (count col-keys))
      (when-let [a (.selectFirst tr "a[href*=/stats/players/]")]
        {:fftoday-id  (second (re-find #"/players/(\d+)/" (.attr a "href")))
         :player-name (str/trim (.text a))
         :team        (when (> (count tds) 2) (str/trim (.text (nth tds 2))))
         :stats       (into {}
                            (keep (fn [i]
                                    (when-let [k (nth col-keys i nil)]
                                      (when-let [v (fetch/num-or-nil
                                                    (str/replace (.text (nth tds i)) "," ""))]
                                        [k v]))))
                            (range (count tds)))}))))

(defn parse-page
  "Projections HTML -> [{:fftoday-id :player-name :team :stats}]."
  [html]
  (if (str/blank? html)
    []
    (let [doc   (Jsoup/parse html)
          table (->> (.select doc "table")
                     (filter #(and (.selectFirst % "tr.tableclmhdr")
                                   (.selectFirst % "a[href*=/stats/players/]")))
                     first)]
      (if-not table
        []
        (if-let [ks (column-keys table)]
          (into [] (keep #(parse-row % ks)) (.select table "tr"))
          [])))))

(defn season-position
  "All pages for one season/position."
  [season pos-id]
  (let [pos (get position-ids pos-id)]
    (into []
          (comp (mapcat (fn [page]
                          (let [{:keys [html]}
                                (fetch/cached
                                 (fetch/cache-path "fftoday" "v1" season pos-id page)
                                 (fn [] {:html (fetch/http-get-string (page-url season pos-id page))}))]
                            (parse-page html))))
                (map #(assoc % :position pos)))
          pages)))

(defn season-data
  "{:season :players [...]} for a season, disk-cached per page."
  [season]
  (fetch/cached
   (fetch/cache-path "fftoday" "season" "v1" season)
   (fn []
     {:season  season
      :players (into [] (mapcat #(season-position season %)) (keys position-ids))})))

(defn players [season] (:players (season-data season)))

(defn coverage
  "Per-season row counts by position, for --source-report."
  [seasons]
  (mapv (fn [s]
          (let [ps (players s)]
            {:season s :n (count ps)
             :by-position (into (sorted-map) (frequencies (map :position ps)))}))
        seasons))
