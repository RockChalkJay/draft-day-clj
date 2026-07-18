(ns draft-day.ingestion.match
  "Canonical match key for joining name-only enrichment sources (FantasyPros) onto
  the Sleeper universe. A low-stakes fallback — Sleeper ids handle exact joins for
  ESPN/nflverse; this covers scrape sources that only give a name."
  (:require [clojure.string :as str]))

(def ^:private suffixes #{"jr" "sr" "ii" "iii" "iv" "v"})

(defn normalize-name
  "lowercase, drop generational suffixes, strip everything non-alphanumeric, and
  concatenate — so \"T.J. Hockenson\" and \"TJ Hockenson\" collapse to the same key."
  [name]
  (let [tokens (-> (or name "")
                   str/lower-case
                   (str/replace #"[^a-z0-9 ]" " ")
                   (str/split #"\s+"))]
    (str/join "" (remove suffixes tokens))))

(defn key-for [name position]
  (str (normalize-name name) "_" (str/lower-case (or position ""))))

(defn by-key
  "Index enrichment maps by :key (dropping the key from the value)."
  [rows]
  (into {} (map (juxt :key #(dissoc % :key))) rows))
