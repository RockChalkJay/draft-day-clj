(ns draft-day.ingestion.fantasypros
  "Enrichment: FantasyPros. Two independent best-effort scrapes off the same
  vendor — a failure of either degrades the board, never empties it.

   - ECR (`parse-ecr`): scrapes the `var ecrData = {…}` JSON blob out of the
     cheatsheet page, supplying expert tier, positional rank, and (crucially) the
     rank spread (rank_std) that powers the Floor/Ceiling model.
   - AAV (`parse-aav`): scrapes FantasyPros' auction-value calculator (the
     draftwizard `#OverallTable`) for a raw market price per player."
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draft-day.ingestion.match :as match]
            [draft-day.json :refer [mapper]])
  (:import [org.jsoup Jsoup]))

(def cheatsheet-urls
  {:ppr      "https://www.fantasypros.com/nfl/rankings/ppr-cheatsheets.php"
   :half-ppr "https://www.fantasypros.com/nfl/rankings/half-point-ppr-cheatsheets.php"
   :standard "https://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php"})


(defn- ->int [x] (cond (number? x) (int x)
                        (string? x) (Integer/parseInt x)))
(defn- ->double [x] (cond (number? x) (double x)
                          (string? x) (parse-double x)))

(defn- normalize-player [p]
  (let [name (:player_name p) pos (:player_position_id p)]
    {:key                  (match/key-for name pos)
     :fantasypros/ecr      (->int (:rank_ecr p))
     :fantasypros/pos-rank (:pos_rank p)
     :fantasypros/ecr-tier (->int (:tier p))
     :fantasypros/rank-std (->double (:rank_std p))
     :fantasypros/rank-ave (->double (:rank_ave p))
     :fantasypros/rank-min (->int (:rank_min p))
     :fantasypros/rank-max (->int (:rank_max p))}))

(defn parse-ecr
  "Pure: cheatsheet HTML -> seq of enrichment maps (each with a :key)."
  [html]
  (try
    (let [doc (Jsoup/parse html)
          scripts (.select doc "script")
          script (->> scripts
                      (filter #(str/includes? (.html %) "var ecrData"))
                      first)]
      (when script
        (let [content (.html script)
              json-match (re-find #"var ecrData = (\{.*)" content)]
          (when (second json-match)
            (let [json-str (second json-match)]
              (->> (:players (json/read-value json-str mapper))
                   (map normalize-player)
                   (filter :key)))))))
    (catch Exception _ nil)))

(defn fetch-ecr
  "Network: fetch + parse the cheatsheet for a scoring format. nil on failure."
  ([] (fetch-ecr :ppr))
  ([scoring]
   (let [url (get cheatsheet-urls scoring (:ppr cheatsheet-urls))
         {:keys [status body error]} @(http/get url {:timeout 30000})]
     (when (and (not error) (= 200 status))
       (parse-ecr body)))))

;; --- AAV (auction values) ---

(def aav-scoring-params
  "The calculator's `scoring` parameter per format. Exactly these three spellings
  work: anything it does not recognize (\"HALF-PPR\", \"0.5\") silently serves
  Standard, which is how a typo here would become a wrong market price rather
  than an error."
  {:standard "STD" :half-ppr "HALF" :ppr "PPR"})

(defn aav-url
  "`teams`/`tb` fix the baseline pool (12 * $200 = $2400) that rankings.market
  normalizes against."
  [fmt]
  (str "https://draftwizard.fantasypros.com/auction/fp_nfl.jsp?scoring="
       (get aav-scoring-params fmt "PPR") "&teams=12&tb=200"))

;; "Josh Allen (BUF - QB)" / "Houston Texans (HOU - DST)" -> name + position.
(def ^:private aav-name-re #"^(.*?)\s*\([A-Z]{2,3}\s*-\s*([A-Z]{1,3})\)\s*$")

(defn parse-aav
  "Pure: auction-calculator HTML -> seq of {:key :fantasypros/aav}. Each
  #OverallTable row carries the dollar value in its `v` attribute and the name in
  its lone class-less <td>; rows without a positive value or a parseable name are
  dropped."
  [html]
  (try
    (seq (->> (.select (Jsoup/parse html) "table#OverallTable tr[pid]")
              (keep (fn [row]
                      (let [v    (->double (.attr row "v"))
                            cell (some-> (.select row "td:not([class])") .first .text)
                            [_ name pos] (some->> cell (re-matches aav-name-re))]
                        (when (and v (pos? v) name pos)
                          {:key (match/key-for name pos) :fantasypros/aav v}))))))
    (catch Exception _ nil)))

(defn fetch-aav
  "Network: fetch + parse the auction-value calculator for a scoring format.
  nil on failure."
  ([] (fetch-aav :ppr))
  ([fmt]
   (let [{:keys [status body error]} @(http/get (aav-url fmt) {:timeout 30000})]
     (when (and (not error) (= 200 status))
       (parse-aav body)))))

;; --- Sleepers (a per-position boolean list, no numeric value) ---
;; Scoring-agnostic: FantasyPros publishes one sleeper list per position. We just
;; mark players who appear on any of them. Position is implied by the page.
(def sleeper-urls
  {"QB" "https://www.fantasypros.com/nfl/rankings/qb-sleepers.php"
   "RB" "https://www.fantasypros.com/nfl/rankings/rb-sleepers.php"
   "WR" "https://www.fantasypros.com/nfl/rankings/wr-sleepers.php"
   "TE" "https://www.fantasypros.com/nfl/rankings/te-sleepers.php"})

(defn parse-sleepers
  "Pure: sleeper-list HTML for one position -> seq of {:key :fantasypros/sleeper?}.
  Each `tr.player-row` carries the name in an `a.fp-player-link`'s `fp-player-name`
  attribute; rows without a name (ad/filler rows) are dropped."
  [html position]
  (try
    (seq (->> (.select (Jsoup/parse html) "tr.player-row a.fp-player-link")
              (keep (fn [a]
                      (let [name (.attr a "fp-player-name")]
                        (when-not (str/blank? name)
                          {:key (match/key-for name position)
                           :fantasypros/sleeper? true}))))))
    (catch Exception _ nil)))

(defn fetch-sleepers
  "Network: fetch + parse every position's sleeper list, concatenated. Each page is
  best-effort; a failing page contributes nothing. nil when none succeed."
  []
  (seq (mapcat (fn [[pos url]]
                 (let [{:keys [status body error]} @(http/get url {:timeout 30000})]
                   (when (and (not error) (= 200 status))
                     (parse-sleepers body pos))))
               sleeper-urls)))
