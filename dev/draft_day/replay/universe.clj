(ns draft-day.replay.universe
  "Per-season player universe for the replay harness (vintage handling).

  A past draft must be scored against *that season's* projections, not today's, or
  the model gets hindsight. We fetch each draft season's Sleeper projections and
  cache them. Sleeper-only is sufficient: Value/Worth derive purely from
  :stats -> :points -> VORP and never touch the FantasyPros/ESPN enrichments
  (those drive display tiers, floor/ceiling, and MKT only).

  These ARE frozen preseason projections for 2021 and later, contrary to what
  this docstring used to claim. Sleeper bulk re-stamps every archived season on
  the Monday after week 18 — all ~680 records written inside a 10-20 second
  window — so `last_modified` reads as January and looks near-final. The values
  are not: projected games stay flat at 17/18 for everyone, and players who
  missed most of a season still carry full-season projections. See
  `draft-day.benchmark.vintage`, which gates on that behaviour. 2019 and 2020 are
  genuinely contaminated (flatness 0.22/0.29) and should not be replayed."
  (:require [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.pipeline :as pipeline]))

(defn- cache-path [season] (str "data/replay_cache/universe-" season ".transit"))

(defn season-universe
  "Season-S Sleeper universe (projections), cached to disk. `season` is a string
  or int. Returns a vector of player rows (possibly empty if Sleeper has no
  projections for that season)."
  [season]
  (let [season (str season)
        path   (cache-path season)]
    (or (pipeline/read-transit path)
        (let [u (vec (sleeper/fetch-universe (Integer/parseInt season)))]
          (when (seq u) (pipeline/write-transit! path u))
          u))))
