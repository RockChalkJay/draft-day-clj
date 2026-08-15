(ns draft-day.rankings.vendor
  "Pick each vendor column's scoring-format variant, per request.

  FantasyPros publishes ECR (expert rank, tier, rank spread) per format and its
  auction calculator takes a scoring parameter; Sleeper publishes ADP per format.
  Ingestion used to bake one format — PPR — into the shared universe, so a
  standard league read PPR expert ranks, PPR rank spread (which sets the
  Floor/Ceiling band), PPR market prices and PPR ADP. Those columns simply did
  not move when the league's scoring did.

  The universe cache is shared across leagues, so the choice cannot be made at
  ingestion. Ingestion now stores every format under `:vendor/by-format` and this
  namespace flattens the matching one onto the flat keys the rest of the pipeline
  and the board already read.

  ESPN is absent on purpose: it publishes the same auction value under both its
  PPR and STANDARD rank types, so there is no variant to pick."
  (:require [draft-day.scoring :as scoring]))

(defn with-format
  "Flatten the `fmt` variant of `:vendor/by-format` onto each player and drop the
  bundle. Dropping it matters — the response carries the whole board, and
  shipping three formats of every vendor column would triple that for data the
  client cannot use."
  [players fmt]
  (mapv (fn [p]
          (-> (merge p (get-in p [:vendor/by-format fmt]))
              (dissoc :vendor/by-format)))
        players))

(defn for-scoring
  "`with-format` for the format nearest this league's scoring config."
  [players scoring]
  (with-format players (scoring/format-of scoring)))
