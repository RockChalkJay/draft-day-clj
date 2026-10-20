(ns draft-day.markdown
  "The small subset of Markdown Sensei answers in, as data: paragraphs, bullet
  and numbered lists, one pipe table, `**bold**` and `` `code` ``. Nothing else
  is recognised, so nothing the model writes can become markup: the view turns
  these blocks into hiccup and never into HTML.

  Shared with the JVM so the parse is tested there. It is read while an answer
  is still arriving, so a half-finished construct has to degrade to plain text
  rather than throw: an unclosed `**` stays as two asterisks, and a table whose
  separator row has not arrived is a paragraph."
  (:require [clojure.string :as str]))

(defn inline
  "`s` as a vector of strings, `[:b text]` and `[:code text]`."
  [s]
  (mapv (fn [token]
          (cond
            (and (str/starts-with? token "**") (str/ends-with? token "**") (> (count token) 4))
            [:b (subs token 2 (- (count token) 2))]

            (and (str/starts-with? token "`") (str/ends-with? token "`") (> (count token) 2))
            [:code (subs token 1 (dec (count token)))]

            :else token))
        (re-seq #"\*\*.+?\*\*|`[^`]+`|[^*`]+|[*`]" s)))

(defn line-kind
  "What one line starts: `:blank`, `:bullet`, `:ordered`, `:table`, `:heading` or
  `:text`."
  [line]
  (cond
    (str/blank? line)                    :blank
    (re-find #"^\s*[-*•]\s+\S" line)     :bullet
    (re-find #"^\s*\d+[.)]\s+\S" line)   :ordered
    (re-find #"^\s*\|" line)             :table
    (re-find #"^#{1,6}\s+\S" line)       :heading
    :else                                :text))

(defn cells
  "The cells of one pipe-table row, trimmed."
  [line]
  (mapv str/trim (str/split (str/replace (str/trim line) #"^\||\|$" "") #"\|" -1)))

(defn separator?
  "Is this the `|---|---|` row under a table's header?"
  [line]
  (boolean (re-matches #"\s*\|?[\s|:-]*-[\s|:-]*" line)))

(defn strip-marker
  "A list item without its `-` or `1.`."
  [line]
  (str/replace line #"^\s*([-*•]|\d+[.)])\s+" ""))

(defn table
  "A table block from its lines, or a paragraph when there is no separator row
  yet: the header of a table still arriving is a line of text, not a table."
  [lines]
  (if (and (> (count lines) 1) (separator? (second lines)))
    {:type :table :head (mapv inline (cells (first lines)))
     :rows (mapv #(mapv inline (cells %)) (drop 2 lines))}
    {:type :p :inline (inline (str/join " " (map str/trim lines)))}))

(defn block
  "One block from a run of lines of one kind, or nil for a blank run."
  [[kind lines]]
  (case kind
    :blank   nil
    :bullet  {:type :ul :items (mapv (comp inline strip-marker) lines)}
    :ordered {:type :ol :items (mapv (comp inline strip-marker) lines)}
    :table   (table lines)
    :heading {:type :p :inline [[:b (str/replace (first lines) #"^#{1,6}\s+" "")]]}
    :text    {:type :p :inline (inline (str/join " " (map str/trim lines)))}))

(defn parse
  "`text` as a vector of blocks: `{:type :p :inline}`, `{:type :ul|:ol :items}`
  and `{:type :table :head :rows}`."
  [text]
  (->> (str/split-lines (or text ""))
       (partition-by line-kind)
       (map (fn [run] [(line-kind (first run)) run]))
       (keep block)
       vec))
