(ns draft-day.ingestion.parallel
  "Run independent enrichment fetches at once instead of one after another.

  Every enrichment source is independent I/O behind a 30-second timeout, and the
  whole chain runs synchronously inside the `GET /api/players` that missed the
  cache. Awaited in turn they stack: ten FantasyPros scrapes (ECR and AAV for
  three formats, plus four sleeper pages) alone are five minutes of a thread
  doing nothing, before Sleeper's byes and ESPN's feed.

  How many connections a single *vendor* sees at once is that vendor's business,
  not this namespace's — the per-host cap belongs with the host, so it lives in
  `ingestion.fantasypros`. This just starts things together and makes sure
  nothing is left running when the caller goes away.")

(defn all
  "Evaluate every thunk in `tasks` ({key thunk}) concurrently; return {key value}
  with the same keys.

  Thunks are expected to be best-effort already (see `pipeline/best-effort`) —
  swallowing here as well would hide a real bug behind a nil column. What this
  does own is the escape path: on *any* throw from the deref, including an
  `Error` that no best-effort wrapper catches, the still-pending tasks are
  cancelled rather than left running with nothing to consume them. A cold load
  can OOM on ESPN's ~37MB feed, and six orphaned scrapes holding their response
  buffers is the worst possible moment to add heap."
  [tasks]
  (let [futs (update-vals tasks (fn [thunk] (future (thunk))))]
    (try
      (update-vals futs deref)
      (finally
        (run! future-cancel (vals futs))))))
