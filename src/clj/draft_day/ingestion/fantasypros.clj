(ns draft-day.ingestion.fantasypros
  "Enrichment: FantasyPros. Two independent best-effort scrapes off the same
  vendor — a failure of either degrades the board, never empties it.

   - ECR (`parse-ecr`): scrapes the `var ecrData = {…}` JSON blob out of the
     cheatsheet page, supplying expert tier, positional rank, bye week, and
     (crucially) the rank spread (rank_std) that powers the Floor/Ceiling model.
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
     :fantasypros/rank-max (->int (:rank_max p))
     :bye                  (->int (:player_bye_week p))}))

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
;; `scoring=PPR` matches the rest of the app's PPR baseline (the ECR scrape above
;; and ESPN's PPR fallback); the page would otherwise default to Standard. See the
;; README TODO on making the market sources scoring-aware. `teams`/`tb` fix the
;; baseline pool (12 * $200 = $2400) that rankings.market normalizes against.
(def aav-url
  "https://draftwizard.fantasypros.com/auction/fp_nfl.jsp?scoring=PPR&teams=12&tb=200")

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
  "Network: fetch + parse the auction-value calculator. nil on failure."
  []
  (let [{:keys [status body error]} @(http/get aav-url {:timeout 30000})]
    (when (and (not error) (= 200 status))
      (parse-aav body))))
