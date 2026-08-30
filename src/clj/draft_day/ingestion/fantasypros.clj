(ns draft-day.ingestion.fantasypros
  "Enrichment: FantasyPros. Independent best-effort scrapes off one vendor — a
  failure of any of them degrades the board, never empties it.

  Twenty-two requests land here on a cold load (ECR and AAV for each of three
  scoring formats, four sleeper pages, and twelve per-position cheatsheets).
  They are started together by callers, so `fetch-page` owns the one thing a
  caller cannot: how many of them this vendor is willing to see at once.

   - ECR (`parse-ecr`): scrapes the `var ecrData = {…}` JSON blob out of the
     cheatsheet page, supplying expert tier, positional rank, and (crucially) the
     rank spread (rank_std) that powers the Floor/Ceiling model.
   - Per-position tier (`parse-pos-ecr`): the same blob off each position's own
     cheatsheet, for the finer tier that page cuts within the position.
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

  Twenty-two scrapes fired simultaneously is twenty-two connections from one IP
  to one host, and a
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

;; --- per-position expert tier ---
;; The overall cheatsheet above publishes an *overall* tier. FantasyPros also
;; publishes a cheatsheet per position, whose `tier` is cut within that position
;; — a genuinely different, finer scale (12 tiers across 171 RBs, where those
;; same RBs share a handful of overall tiers). The board shows it beside its own
;; tier at whichever scale is active, so both are ingested.

(def pos-formats
  "Whether a position's cheatsheet varies by scoring format.

  RB/WR/TE have ppr / half-point-ppr / bare (standard) variants. QB, K and DST
  have only the bare page — the prefixed URLs 302 to the *overall* cheatsheet,
  because reception scoring cannot reorder them. Following that redirect would
  parse a whole-board page as if it were one position, so the false entries here
  are load-bearing, not an optimization: those three are fetched once and joined
  unscoped, the same call `pipeline` already makes for ESPN."
  {"RB" true "WR" true "TE" true "QB" false "K" false "DST" false})

(def pos-format-prefixes
  {:ppr "ppr-" :half-ppr "half-point-ppr-" :standard ""})

(defn pos-cheatsheet-url
  "The per-position cheatsheet for `pos` at scoring format `fmt`.

  An unknown position or format throws, for the reason `cheatsheet-url` gives.
  A format-invariant position ignores `fmt` and returns its bare page."
  [pos fmt]
  (let [varies? (get pos-formats pos ::missing)]
    (when (= ::missing varies?)
      (throw (ex-info "no FantasyPros positional cheatsheet for that position"
                      {:position pos :known (vec (sort (keys pos-formats)))})))
    (let [prefix (if varies?
                   (or (get pos-format-prefixes fmt)
                       (throw (ex-info "no FantasyPros cheatsheet for that scoring format"
                                       {:scoring fmt
                                        :known (vec (keys pos-format-prefixes))})))
                   "")]
      (str "https://www.fantasypros.com/nfl/rankings/"
           prefix (str/lower-case pos) "-cheatsheets.php"))))

(defn parse-pos-ecr
  "Pure: a positional cheatsheet -> seq of {:key :fantasypros/ecr-pos-tier}.

  Deliberately narrow. The page also carries rank_ecr, rank_std and the rest,
  but those already arrive from the overall cheatsheet; re-emitting them here
  would put two scrapes in a race to write the same columns through `deep-merge`,
  and the positional page's ranks are position-relative, so the winner would
  sometimes be an ECR of 4 meaning 'RB4'. Only the tier is taken — the one thing
  the overall page cannot give per position."
  [html]
  (some->> (parse-ecr html)
           (keep (fn [{:keys [key] :fantasypros/keys [ecr-tier]}]
                   (when (and key ecr-tier (pos? ecr-tier))
                     {:key key :fantasypros/ecr-pos-tier ecr-tier})))
           seq))

(defn fetch-pos-ecr
  "Network: fetch + parse one position's cheatsheet at one format. nil on failure."
  [pos fmt]
  (some-> (fetch-page (pos-cheatsheet-url pos fmt)) parse-pos-ecr))

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
;; Unanchored at the tail on purpose; `parse-aav` says what follows the position.
(def ^:private aav-name-re #"^(.*?)\s*\([A-Z]{2,3}\s*-\s*([A-Z]{1,3})\)")

(defn parse-aav
  "Pure: auction-calculator HTML -> seq of {:key :fantasypros/aav}. Each
  #OverallTable row carries the dollar value in its `v` attribute and the name in
  its lone class-less <td>; rows without a positive value or a parseable name are
  dropped.

  That cell holds a name *and*, for anyone carrying a knock, an injury badge:
  `<td>Puka Nacua (LAR - WR)<span class='injury-tag'>DTD</span></td>`. Two
  guards keep the badge out, and they are both here because they fail in
  opposite directions: the cell is read by its *own* text, so a child element
  contributes nothing wherever it sits, and the name is matched rather than
  fully matched, so a trailing badge is ignored even if it arrives as bare text
  in the cell rather than as an element.

  Reading the whole cell text against an end-anchored pattern is what this
  replaces, and it failed in the worst available way. On the PPR page of
  2026-08-29, 33 of 179 priced rows simply never parsed — roughly a fifth of
  the board and $532 of a $2400 pool, weighted to the expensive end, since a
  $60 receiver's knock is the one that gets reported. The badge set turns over
  weekly, so read that as a magnitude and not a constant. Meanwhile the
  `:sources` report went on calling the join 99% healthy: it counts the rows
  that reach it, and a row dropped here never does."
  [html]
  (try
    (seq (->> (.select (Jsoup/parse html) "table#OverallTable tr[pid]")
              (keep (fn [row]
                      (let [v    (->double (.attr row "v"))
                            cell (some-> (.select row "td:not([class])") .first .ownText)
                            [_ name pos] (some->> cell (re-find aav-name-re))]
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
