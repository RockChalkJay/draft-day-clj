(ns draft-day.ingestion.paths
  "Where the server keeps what it can fetch again. The harvesters' archives
  (`data/trends`, `data/projections`) are beside this directory, not in it, so
  the caches can be wiped without touching them.")

(def cache-root "data/cache")
