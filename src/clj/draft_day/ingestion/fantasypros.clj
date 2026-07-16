(ns draft-day.ingestion.fantasypros
  "Enrichment: FantasyPros expert consensus (ECR). Scrapes the `var ecrData = {…}`
  JSON blob out of the cheatsheet page — this supplies the expert tier, positional
  rank, bye week, and (crucially) the rank spread (rank_std) that powers the
  Floor/Ceiling risk model. Best-effort: a failure degrades the board, never
  empties it."
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draft-day.ingestion.match :as match])
  (:import [org.jsoup Jsoup]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))

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

(defn ecr-by-key
  "Index enrichment maps by :key (dropping the key from the value)."
  [players]
  (into {} (map (juxt :key #(dissoc % :key))) players))

(defn fetch-ecr
  "Network: fetch + parse the cheatsheet for a scoring format. nil on failure."
  ([] (fetch-ecr :ppr))
  ([scoring]
   (let [url (get cheatsheet-urls scoring (:ppr cheatsheet-urls))
         {:keys [status body error]} @(http/get url {:timeout 30000})]
     (when (and (not error) (= 200 status))
       (parse-ecr body)))))
