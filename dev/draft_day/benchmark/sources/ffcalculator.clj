(ns draft-day.benchmark.sources.ffcalculator
  "Fantasy Football Calculator: vintage consensus ADP, 2010-2025.

  Free and keyless, and the deepest draft-time input available — six seasons more
  than Sleeper's projections reach. Its provenance is also the strongest of any
  source here: each response's `meta` carries the actual draft window the ADP was
  sampled over (2015: start_date 2015-09-06, end_date 2015-09-09) and the number
  of drafts behind it, so \"was this really known before the season?\" is answered
  by the payload rather than inferred.

  Publishes names, not ids, so the join goes through the DynastyProcess name
  crosswalk. `stdev`/`high`/`low` come along free — genuine market disagreement,
  which is the quantity `rankings.projections` currently approximates from
  FantasyPros rank spread."
  (:require [draft-day.benchmark.fetch :as fetch]
            [draft-day.ingestion.match :as match]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]))

(def formats
  "Scoring format -> FFC path segment. FFC has no half-ppr before ~2018; callers
  should treat an empty result as 'not published' rather than an error."
  {:ppr "ppr" :standard "standard" :half-ppr "half-ppr"})

(defn adp-url [format season teams]
  (str "https://fantasyfootballcalculator.com/api/v1/adp/"
       (get formats format "ppr") "?teams=" teams "&year=" season))

(defn parse-response
  "Pure: FFC payload -> {:meta {...} :players [{:match-key :name :position :team
  :adp :stdev :bye}]}."
  [payload]
  {:meta    (:meta payload)
   :players (mapv (fn [p]
                    {:match-key (match/key-for (:name p) (:position p))
                     :name      (:name p)
                     :position  (:position p)
                     :team      (:team p)
                     :adp       (some-> (:adp p) double)
                     :stdev     (some-> (:stdev p) double)
                     :bye       (:bye p)})
                  (:players payload))})

(defn season-data
  "{:meta {...} :players [...]} for a season/format, disk-cached."
  ([season] (season-data season :ppr 12))
  ([season format teams]
   (fetch/cached
    (fetch/cache-path "ffc" (name format) teams season)
    #(if-let [body (fetch/http-get-string (adp-url format season teams))]
       (let [parsed (parse-response (json/read-value body mapper))]
         (if (seq (:players parsed)) parsed {}))
       {}))))

(defn adp-by-gsis
  "{gsis-id adp} for a season, joined through the DynastyProcess name crosswalk.
  Unmatched names are dropped — they are overwhelmingly players with no nflverse
  outcome row either (practice-squad flyers late in the ADP list)."
  [season resolve-name]
  (into {}
        (keep (fn [{:keys [match-key adp]}]
                (when-let [g (resolve-name match-key)]
                  (when adp [g adp]))))
        (:players (season-data season))))

(defn draft-window
  "The sampling window behind a season's ADP, for --source-report. This is real
  provenance: ADP sampled in early September is unambiguously draft-time."
  [season]
  (let [m (:meta (season-data season))]
    (select-keys m [:start_date :end_date :total_drafts :type :teams])))
