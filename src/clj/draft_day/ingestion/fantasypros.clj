(ns draft-day.ingestion.fantasypros
  "Enrichment: FantasyPros. Independent best-effort scrapes off one vendor — a
  failure of any of them degrades the board, never empties it.

  Ten requests land here on a cold load (ECR and AAV for each of three scoring
  formats, plus four sleeper pages). They are started together by callers, so
  `fetch-page` owns the one thing a caller cannot: how many of them this vendor
  is willing to see at once.

   - ECR (`parse-ecr`): scrapes the `var ecrData = {…}` JSON blob out of the
     cheatsheet page, supplying expert tier, positional rank, and (crucially) the
     rank spread (rank_std) that powers the Floor/Ceiling model.
   - AAV (`parse-aav`): scrapes FantasyPros' auction-value calculator (the
     draftwizard `#OverallTable`) for a raw market price per player."
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draft-day.ingestion.match :as match]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.json :refer [mapper]])
  (:import [java.util.concurrent Semaphore]
           [org.jsoup Jsoup]))

(def max-in-flight
  "How many requests this vendor may see from us at once.

  Ten scrapes (ECR and AAV for three formats, plus four sleeper pages) fired
  simultaneously is ten connections from one IP to one host, and a
  scraping-averse vendor answers that with a 429. A 429 is not an exception
  here — it is `nil` from every fetch, a cache written with no FantasyPros data
  in it at all, and that cache served for the next 24 hours. So the cap is the
  point: three at a time is most of the speedup without looking like a crawl."
  3)

(defonce ^:private throttle (Semaphore. max-in-flight true))

(defn fetch-page
  "GET one FantasyPros page, returning its body, or nil on any transport error
  or non-200.

  Every scrape in this namespace goes through here, so however many are started
  at once the vendor still sees at most `max-in-flight` of them. The permit is
  released in a `finally`: a scrape that throws must not retire a permit
  permanently, or a later cold load deadlocks on a vendor that is perfectly
  healthy."
  [url]
  (.acquire throttle)
  (try
    (let [{:keys [status body error]} @(http/get url {:timeout 30000})]
      (when (and (not error) (= 200 status)) body))
    (finally (.release throttle))))

(def cheatsheet-urls
  {:ppr      "https://www.fantasypros.com/nfl/rankings/ppr-cheatsheets.php"
   :half-ppr "https://www.fantasypros.com/nfl/rankings/half-point-ppr-cheatsheets.php"
   :standard "https://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php"})

(defn cheatsheet-url
  "The cheatsheet for a scoring format. An unknown format throws.

  Falling back to PPR here is the bug the per-format split removed, wearing a
  different hat: the fetch succeeds, the join succeeds, the source reports a
  full row count, and a standard league reads PPR expert ranks with nothing
  anywhere saying so. Ingestion wraps this in `best-effort`, so a throw costs
  that one column and says so on the board — which is the whole point."
  [scoring]
  (or (get cheatsheet-urls scoring)
      (throw (ex-info "no FantasyPros cheatsheet for that scoring format"
                      {:scoring scoring :known (vec (keys cheatsheet-urls))}))))


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
  "Network: fetch + parse the cheatsheet for a scoring format. nil on failure.
  The format is required, and an unrecognized one throws rather than quietly
  becoming PPR — see `cheatsheet-url`."
  [scoring]
  (some-> (fetch-page (cheatsheet-url scoring)) parse-ecr))

;; --- AAV (auction values) ---

(def aav-scoring-params
  "The calculator's `scoring` parameter per format. Exactly these three spellings
  work: anything it does not recognize (\"HALF-PPR\", \"0.5\") silently serves
  Standard, which is how a typo here would become a wrong market price rather
  than an error."
  {:standard "STD" :half-ppr "HALF" :ppr "PPR"})

(defn aav-url
  "`teams`/`tb` fix the baseline pool (12 * $200 = $2400) that rankings.market
  normalizes against. An unknown format throws, for the reason `cheatsheet-url`
  gives — and doubly so here, because the calculator itself silently serves
  Standard for a parameter it does not recognize."
  [fmt]
  (let [param (or (get aav-scoring-params fmt)
                  (throw (ex-info "no FantasyPros auction scoring param for that format"
                                  {:format fmt :known (vec (keys aav-scoring-params))})))]
    (str "https://draftwizard.fantasypros.com/auction/fp_nfl.jsp?scoring="
         param "&teams=12&tb=200")))

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
  nil on failure. Format required, for the reason `fetch-ecr` gives."
  [fmt]
  (some-> (fetch-page (aav-url fmt)) parse-aav))

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
  "Network: fetch + parse every position's sleeper list, concatenated. Each page
  is best-effort; a failing page contributes nothing. nil when none succeed.

  The four pages go out together. Walked in turn they were four more 30-second
  timeouts on the request thread — 120 seconds that no amount of parallelism
  elsewhere in ingestion could hide, and all of it against the same vendor that
  had just gone quiet. `fetch-page` still holds them to `max-in-flight`."
  []
  (let [bodies (parallel/all
                (update-vals sleeper-urls
                             (fn [url] #(try (fetch-page url)
                                             (catch Exception _ nil)))))]
    (seq (mapcat (fn [[pos _]] (some-> (get bodies pos) (parse-sleepers pos)))
                 sleeper-urls))))
