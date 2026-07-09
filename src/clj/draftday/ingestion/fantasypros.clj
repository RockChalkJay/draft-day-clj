(ns draftday.ingestion.fantasypros
  "Enrichment: FantasyPros expert consensus (ECR). Scrapes the `var ecrData = {…}`
  JSON blob out of the cheatsheet page — this supplies the expert tier, positional
  rank, bye week, and (crucially) the rank spread (rank_std) that powers the
  Floor/Ceiling risk model. Best-effort: a failure degrades the board, never
  empties it."
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draftday.ingestion.match :as match]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))

(def cheatsheet-urls
  {:ppr      "https://www.fantasypros.com/nfl/rankings/ppr-cheatsheets.php"
   :half-ppr "https://www.fantasypros.com/nfl/rankings/half-point-ppr-cheatsheets.php"
   :standard "https://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php"})

(def ^:private user-agent
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36")

(defn balanced-object
  "Return the first brace-balanced {…} substring of s, respecting JSON strings and
  escapes (so a } inside a string value doesn't end it early)."
  [s]
  (when-let [start (str/index-of s "{")]
    (loop [i start, depth 0, in-str false, esc false]
      (when (< i (count s))
        (let [c (.charAt ^String s i)]
          (cond
            esc      (recur (inc i) depth in-str false)
            (= c \\) (recur (inc i) depth in-str true)
            (= c \") (recur (inc i) depth (not in-str) false)
            in-str   (recur (inc i) depth in-str false)
            (= c \{) (recur (inc i) (inc depth) in-str false)
            (= c \}) (if (= depth 1)
                       (subs s start (inc i))
                       (recur (inc i) (dec depth) in-str false))
            :else    (recur (inc i) depth in-str false)))))))

(defn- ->double [x] (cond (number? x) (double x) 
                          (string? x) (parse-double x)))
(defn- ->long   [x] (cond (number? x) (long x)   
                          (string? x) (parse-long x)))

(defn- normalize-player [p]
  (let [name (:player_name p) pos (:player_position_id p)]
    {:key                  (match/key-for name pos)
     :fantasypros/ecr      (->long (:rank_ecr p))
     :fantasypros/pos-rank (:pos_rank p)
     :fantasypros/ecr-tier (->long (:tier p))
     :fantasypros/rank-std (->double (:rank_std p))
     :fantasypros/rank-ave (->double (:rank_ave p))
     :fantasypros/rank-min (->long (:rank_min p))
     :fantasypros/rank-max (->long (:rank_max p))
     :bye                  (->long (:player_bye_week p))}))

(defn parse-ecr
  "Pure: cheatsheet HTML -> seq of enrichment maps (each with a :key)."
  [html]
  (when-let [idx (str/index-of html "var ecrData = ")]
    (when-let [obj (balanced-object (subs html idx))]
      (->> (:players (json/read-value obj mapper))
           (map normalize-player)
           (filter :key)))))

(defn ecr-by-key
  "Index enrichment maps by :key (dropping the key from the value)."
  [players]
  (into {} (map (juxt :key #(dissoc % :key))) players))

(defn fetch-ecr
  "Network: fetch + parse the cheatsheet for a scoring format. nil on failure."
  ([] (fetch-ecr :ppr))
  ([scoring]
   (let [url (get cheatsheet-urls scoring (:ppr cheatsheet-urls))
         {:keys [status body error]} @(http/get url {:timeout 30000
                                                      :headers {"User-Agent" user-agent}})]
     (when (and (not error) (= 200 status))
       (parse-ecr body)))))
