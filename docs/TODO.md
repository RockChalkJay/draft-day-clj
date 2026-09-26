# TODO

Working list for Draft Day. Everything here is open; entries are deleted when
they are done rather than struck through. See the [README](../README.md) for
what the app is and how it works, and [scoring-coverage.md](scoring-coverage.md)
for the known gaps between a league's real rules and what the board can score.

- **Remove the 🚨 for tier cliffs. Postion views with tier coloring accomplish the same thing in a clean way.**

- **Warning when a nominated player would cause 3 or more shared bye weeks at the same position**

- **Add stats to the on the block card.** Target percentage, targets,
  receptions. The player picture is done (`views/controls.cljs` renders a
  headshot with a silhouette fallback); the stats half is what is left.

- **Per-league draft state.** `:teams`, `:drafted`, `:picks` and `:my-team-id`
  are still one draft for one team, sitting beside a `:leagues` map that holds
  everything else per league. Switching leagues therefore re-prices the board
  but keeps the draft that is on it. Fine while drafts are done — a manager who
  drafts in two leagues does so months apart — but it must be settled before the
  next preseason, and moving them into the league entry is another
  `fx/storage-version` bump when it happens. It reaches the phase too, though
  only for a league whose host has not said whether it has drafted:
  `db/derived-phase` falls back to the tracker's draft there, so finishing one
  auction opens every such league in season until week 1 is played.

- **Neither board is reachable from a keyboard.** Rows on the draft board
  (click to nominate) and the waiver board (click to compare) have no
  `tabIndex` and no key handler, so both features are mouse-only. The cheap fix
  makes it worse — `tabIndex 0` on six hundred rows means tabbing off the search
  box walks the whole table — so it wants the roving-tabindex grid pattern with
  arrow-key movement and a single tab stop, applied to both boards at once.
  Raised and deliberately deferred reviewing #52.

- **Multi-source projections: a blended mean.** Every forward-looking number on
  both boards comes from one vendor — Rotowire, via Sleeper. A mean across
  vendors is the most robust result in the forecasting literature, and
  `market.clj` already does exactly this for auction dollars: normalize each
  source to a common basis, `keep` what the player has, average. The difference
  is that `market` is display-only while a blended projection would be
  load-bearing — `:points` feeds VORP, tiers, Value and Worth; `:ros-points`
  feeds `:upgrade` and `:bid`. So it is a `rankings/model.clj` registry
  candidate validated in `dev/`, not a display layer, and it does not ship on
  the strength of the argument: `model/blend.clj` holds two candidates that
  looked like improvements over two seasons and lost over five. Note the harness
  scores **preseason draft** rankings only — there is nothing equivalent for
  `:ros-points` or a weekly line, so an in-season blend needs one built before
  it can be judged. One convenience: scoring is linear, so averaging the stat
  lines and averaging the scored points give the same answer — which stops being
  true for any non-linear rule (FG distance buckets, DST points-allowed tiers),
  the same set `scoring-coverage.md` already tracks.

- **The calibration is one vendor, one season, half-PPR.** `confidence/win-rates`
  was measured from 2025 Rotowire weekly projections against 2025 actuals. The
  *shape* travels — a rank gap needs no error model and is scoring-invariant —
  but the percentages may not, and nothing re-derives them. They live in one
  named table with its provenance in the ns docstring so that re-measuring is a
  data change. Two gaps worth closing: no position outside QB/RB/WR/TE/K was
  measured at all (DST gets no verdict, deliberately), and the deep-pool split
  that matters most for a waiver board — where discrimination is *worst* — is
  recorded in prose rather than in the table the code reads.

- **`:market-multiplier` never reaches the wire.** `engine/live-valuation`
  computes and returns it (`src/clj/draft_day/rankings/engine.clj:89`)
  precisely so the client does not recompose `inflation × market-heat` itself
  and skip the band. But `rankings-handler` selects only
  `[:inflation :inflation-index :market-heat]`
  (`src/clj/draft_day/api/routes.clj:199`), so the key never ships, and the
  header's fallback branch in `src/cljs/draft_day/core.cljs:71` always wins —
  displaying the un-banded product, which is the exact defect the comment above
  it says was fixed. `subs.cljs:97` also selects a key that never arrives. One
  line to fix: add `:market-multiplier` to the `select-keys` vector.

- **The ESPN tables are documented numbering, not a payload this repo has
  read.** Three of them: the defensive entries in
  `league-import.espn/stat-ids` (93-106), `lineup-slots`, and
  `league-sync.espn/pro-team-abbrev`. All three fail *silently* — a wrong stat
  id prices a rule nobody set, a wrong slot mis-sizes the roster that decides
  whether a claim costs a drop, a wrong team id drops a defense onto the
  free-agent board while its owner holds it.
  `test/draft_day/integration/espn_league_test.clj` checks all three against a
  live league and skips out loud without `DRAFTDAY_ESPN_SWID`/`_S2`/`_LEAGUE`
  in the environment. Run it once against a real league and the guesses stop
  being guesses. Run against a real league it turns up one gap that is not a
  guess: `unsupported-scoring` reports two dozen of that league's rules as bare
  `ESPN stat 123`, so a manager comparing the import against his settings page
  is handed numbers rather than names. `stat-labels` is what needs the entries —
  the ids cluster in the kicking and defensive-points bands (63, 77, 85,
  123-136, 198, 201, 206, 209).

- **ESPN's `:playoff-week-start` is deliberately unread.** `nil` is already the
  legal answer for a host that says nothing, and `waiver/claims-left` degrades
  honestly on it — while a wrong week mis-sizes every bid on the board and says
  nothing. `settings.scheduleSettings` is where it lives; confirm the spelling
  against a live league before reading it. `:waiver-position` is read from
  `waiverRank` on the same evidence, i.e. none.

- **ESPN leagues have no bid history.** `ingestion/transactions.clj` is
  provider-agnostic and Sleeper is its only provider, so an ESPN sync carries no
  `:bid-history` (`providers/bid-history?` is false) and its waiver board will
  price rival bids from Sleeper-wide data alone. The ESPN half is one new file,
  `ingestion/transactions/espn.clj`, with the season-level pair:
  `fetch-raw-season` reads `view=mTransactions2&scoringPeriodId=N` for each
  week with an `x-fantasy-filter` asking for `WAIVER` and `WAIVER_ERROR` — the
  failed claims, which is the whole point — and `normalize-season` reads
  `bidAmount`, `teamId`, the `ADD` item's player (a D/ST by its team
  abbreviation, as `league-sync.espn/entry-player-id` keys it), `status`
  (`EXECUTED` won) and `processDate` under the Sleeper normalizer's structural
  rule. An ESPN league keeps its id across seasons, so `:previous` is the same
  id a year earlier, which ESPN serves from its separate `leagueHistory`
  endpoint. `import-espn/get-json` needs an optional filter header. None of
  that shape has been read off a live payload, so it wants assertions in
  `test/draft_day/integration/espn_league_test.clj` before it is trusted, like
  the other ESPN tables.

- **Optional: rename the bid history's `:auctions` key.** In an auction-draft
  app, "auction" on the waiver side reads like the draft room, so the docstrings
  say "waiver auction" and `ingestion/transactions.clj` defines the term. A
  name that cannot be misread (`:waiver-runs`, `:contests`) would retire the
  qualifier. It touches the season contract in `transactions.clj`, the Sleeper
  normalizer, `transactions/summary`, the `faab.*` harness and their tests, and
  the cached history files — `transactions/schema-version` must move, or an old
  file loads with the key missing.

- **A rival new to this league can bring his habits from his others.** Sleeper
  user ids are global, so a manager's bids in his other public FAAB leagues are
  readable, and `draft-day.bid-prior/persistence` says they carry: across
  1,973 managers in two $100 leagues in the same season, claims a week
  correlate at 0.43, $0 share at 0.56 and aggression at 0.37 — weaker than the
  same league a season apart (0.57, 0.62, 0.43), stronger than knowing nothing. It would give
  a brand-new league per-rival habits from week one, where it now has only the
  Sleeper-wide typical manager. The cost is the reason it waits: a
  sync would read tens to a couple of hundred more transaction logs, which wants
  the history cache to keep them and `sleeper-http`'s limit to pace them.

- **ESPN league discovery is undocumented and will break.** `fan.api.espn.com`
  is the only endpoint in the app with a credential in its *path*, and the only
  one whose shape nobody publishes. `league-sync.espn/league-entries` is written
  to find nothing rather than to throw, and `find-leagues` reports that as a gap
  beside the account rather than as a failed connection, so the fallback is
  pasting a league ID. That is the designed behaviour, not a bug to fix — but if
  ESPN ever publishes a supported listing, this is the thing to replace.

- **Credentials sit in `localStorage`.** An `espn_s2` is a live session token
  and it is persisted in the browser next to the rest of the app's state,
  because the server holds nothing between requests and there is no session
  store to put it in. Accepted, and written down here so it is a decision
  rather than an oversight. Nothing keeps it out of another script running on
  the same origin — and, per the entry below, nothing currently keeps it out of
  a log line either.

- **`providers/redact` is the rule nobody applies.** CLAUDE.md and
  `league_import/espn.clj`'s docstring both name it as the only sanctioned way a
  credential may reach a log line, an `ex-data` or an error body, and the
  function is correct and tested — but it has **no production caller**. Every
  throw site today happens to carry `{:status n}` and nothing more, so nothing
  leaks; the discipline is maintained by hand at each site rather than by the
  guard, which is exactly the arrangement that holds until the first person adds
  a helpful `{:credentials creds}` to an `ex-info` while debugging. Either route
  the provider throws through `redact`, or stop documenting it as mandatory.

- **Audit error handling across the application.** The app runs three error
  protocols at once and converts between them ad hoc:

  - **throw `ex-info` with `:status`** — `league-sync.sleeper/fetch-json`,
    `league-import/fetch-raw-league`, `fetch-raw-rosters`
  - **result maps** `{:ok true …}` / `{:ok false :status :error}` —
    `import-league`, `sync-league`, which exist to convert from the first
  - **nil on failure** — `pipeline/best-effort` (logs, returns nil),
    `espn/http-get-string` (nil on any non-200)

  `league-sync/unwrap-execution` is what made this visible. It is not fixing a
  bug so much as undoing damage the design does to itself: the status travels in
  the *exception* channel, a future's deref wraps the throw and destroys the
  ex-data, so a function has to exist to put it back. Carried as a value,
  `{:ok false :status 404}` would cross a future boundary untouched and there
  would be nothing to unwrap. The audit should decide where the throw→value
  boundary belongs and make it consistent rather than adding another adapter.

- **ESPN `My Roster` sorts wrong** When a ESPN league is active, the `My Roster`
  sort order is not in the standard sort order by position. e.g. QB, RB, WR, TE, 
  FLEX, K, DEF, BENCH, IR

  Specific things already spotted:

  - `espn/http-get-string` returns nil on any non-200, producing exactly the
    silently empty column CLAUDE.md names as the worst ingestion failure — the
    same shape as the FantasyPros 429 it warns about.
  - `parallel/all`'s docstring notes `best-effort` does not catch `Error`, so
    the escape path and the swallow path disagree about what a failure is.
  - The frontend `:http` effect routes every non-2xx to `on-failure` with
    `(:error body)`, which assumes every handler returns `{:error msg}`. Worth
    confirming that holds everywhere. It now appends the HTTP status alongside
    the message, which is what lets a 401 be told from a 502 — but the *message*
    is still the only thing most handlers read.
  - `:waivers-failed` and `:recompute-failed` deliberately keep stale data
    readable rather than blanking it. That is the good pattern; it should be
    stated as the convention rather than left as two coincidences.
