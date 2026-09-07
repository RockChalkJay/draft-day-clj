# TODO

Working list for Draft Day. Struck-through entries are done; bold ones are
still open. See the [README](../README.md) for what the app is and how it
works, and [scoring-coverage.md](scoring-coverage.md) for the known gaps
between a league's real rules and what the board can score.


- ~~Player budgeting~~

- **Persistence**

- ~~Don't position filter on suggestion cards~~

- ~~Remove strategy tabs - they don't add value~~

- ~~Injury history not just current health. Color code for serious injury or suspension~~

- **Remove the 🚨 for tier cliffs. Postion views with tier coloring accomplish the same thing in a clean way.**

- **Fix tier row color bug**

- **Warning when a nominated player would cause 3 or more shared bye weeks at the same position**

- **Add stats and player pic to the on the block card** Include additional stats like target percentage, number of targets, number of receptions.

- **Per-league draft state.** `:teams`, `:drafted`, `:picks` and `:my-team-id`
  are still one draft for one team, sitting beside a `:leagues` map that holds
  everything else per league. Switching leagues therefore re-prices the board
  but keeps the draft that is on it. Fine while drafts are done — a manager who
  drafts in two leagues does so months apart — but it must be settled before the
  next preseason, and moving them into the league entry is another
  `fx/storage-version` bump when it happens.

- ~~Points for the current week alongside rest-of-season~~ Shipped. Sleeper's
  weekly endpoint returns the same entry shape the season line already parses
  and names the same vendor behind both, and it carries the opponent, so this
  cost one fetch rather than the ingestion project it looks like. Cached apart
  from the universe on a far shorter TTL — it is the only column in the app that
  goes stale in under a day. Open follow-up: **mid-Sunday the board projects the
  wrong week.** `:through-week` is the newest week nflverse has published and it
  publishes as games finish, so partial week-6 rows push it to 6 and the board
  asks for week 7 while week 6 is being played. Honest rather than mislabelled —
  the banner names the week — but the fix is a slate-completeness check ("a week
  is reached once its full slate has rows") rather than a calendar.

- ~~Comparison UI — free agent against your roster, free agent against free
  agent~~ Shipped, and the "large LOE" estimate was wrong: it was the
  weekly/rest-of-season split it depended on that looked expensive, and that
  turned out to be one fetch. Rows lean toward whoever leads so a split between
  the two horizons reads as a zigzag, and the tile names the split without
  picking for you.

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

- ~~**A vendor-disagreement column.**~~ Superseded rather than shelved. It was
  reaching for "is this difference real", and vendor agreement turns out to be a
  poor proxy for that: the ESPN/Sleeper spread is 0.78 points against a real
  median error of 4.04, so two vendors agreeing says far more about their shared
  method than about the player. `draft-day.confidence` answers the same question
  against **actual outcomes** instead, and in rank gaps rather than points, so it
  needs no error model and is identical under any scoring config. The findings
  that stopped the spread stand and are kept below, because they are facts about
  the ESPN feed that the next idea to reach for it will need.

  The original entry: the idea was a `±`
  beside each projection showing how far ESPN and Sleeper disagree — display
  only, so it needs no validation, and two players projecting the same number
  with different agreement is not a tie. ESPN's full projected line is already
  downloaded and its stat ids verified (3 pass yd, 4 pass td, 20 int, 19/26/44
  two-point, 23/24/25 rush, 42/43/53/58 receiving, 72 fumbles lost, 83/86
  kicking). What stopped it, measured against the live 2026 feed over 400
  name-matched players:
  - **No second weekly source.** ESPN publishes a season projection, so only
    rest-of-season could ever carry a spread. A weekly one would be a second
    37MB fetch per week.
  - **A house bias that would read as an opinion.** Median ESPN/Sleeper is 1.13
    for RB and 0.96 for TE, so a raw gap says "ESPN likes him" about every back
    on the board. It needs per-position bias correction to mean anything.
  - **Team defenses are unmappable.** Their ids sit in a 93-106 band whose
    members cannot be told apart by magnitude, and guessing produces a confident
    wrong number.
  The residual disagreement is real once the bias is removed (IQR of the ratio
  is 0.19-0.26 for RB/WR/TE) — but it is a fifth of the error it would be
  standing in for, which is what settled it.

- **The calibration is one vendor, one season, half-PPR.** `confidence/win-rates`
  was measured from 2025 Rotowire weekly projections against 2025 actuals. The
  *shape* travels — a rank gap needs no error model and is scoring-invariant —
  but the percentages may not, and nothing re-derives them. They live in one
  named table with its provenance in the ns docstring so that re-measuring is a
  data change. Two gaps worth closing: no position outside QB/RB/WR/TE/K was
  measured at all (DST gets no verdict, deliberately), and the deep-pool split
  that matters most for a waiver board — where discrimination is *worst* — is
  recorded in prose rather than in the table the code reads.

- ~~Drag-and-drop column bugs found reviewing #12: droppable `text/plain` payload,
  picker drag dead in Firefox, missing `preventDefault`, insertion line flicker~~

- ~~In-season waiver wire: sync real rosters, project rest-of-season, price a
  claim against FAAB~~ Shipped. See [The waiver wire](../README.md#the-waiver-wire).
  Open follow-ups it deliberately left:
  - `ros/PRIOR-GAMES` and `nflverse-weekly/recent-window` are **chosen, not
    measured**. `dev/draft_day/benchmark/` is where they would earn numbers —
    the harness already replays historical seasons, which is exactly the shape
    of evidence the blend needs.
  - The rest-of-season projection reads no injury designation, so a player who
    has been out since week 2 still carries a full share of the games remaining.
    `:injury-risk` and the Inj column cover it on the board; folding it into the
    projection would be the double-charging `rankings.injury` argues against, so
    it needs a real argument before it happens.
  - Only Sleeper syncs. ESPN and Yahoo need server-side auth, which is why the
    sync is backend-proxied — adding one is two `defmethod`s and a `:require`.
    The browser is now ready for them too: accounts are keyed by provider and
    leagues by `db/league-key` (`provider:league-id`), so a second provider is a
    new entry rather than a second shape.
  - **The bundled sample predates the in-season columns.** It stamps
    `:schema-version 5`, carries no `:through-week` and no
    `:nflverse/season-to-date`, so `DRAFTDAY_OFFLINE=1` can only ever show the
    preseason board. That is honest rather than wrong — a preseason capture read
    back as preseason — but it means the in-season half cannot be exercised
    offline at all, and `snapshot/missing-sources` now flags `:nflverse/weekly`.
    Fixed by re-running `draft-day.tools.snapshot` once a season is under way.

- **The watch list comes back in hash order after the set-to-vector migration.**
  `db/reconcile-watchlist` is `(into [] (distinct) stored)`, and over the `#{}`
  the app used to persist that is hash-iteration order. `:watchlist-players`
  used to end in a `sort-by rank-key` which hid it; that sort is gone now the
  order is the manager's. An upgrading manager opens the app to a scrambled
  list with nothing saying anything moved. `(set? stored)` is detectable at
  exactly the point the repair happens, so `:boot` could re-sort once.
  Predates the waiver work — noted here rather than fixed inside it.

- **`:market-multiplier` never reaches the wire.** `engine/live-valuation`
  computes and returns it (`src/clj/draft_day/rankings/engine.clj:87`)
  precisely so the client does not recompose `inflation × market-heat` itself
  and skip the band. But `rankings-handler` selects only
  `[:inflation :inflation-index :market-heat]`
  (`src/clj/draft_day/api/routes.clj:124`), so the key never ships, and the
  header's fallback branch in `src/cljs/draft_day/core.cljs:26` always wins —
  displaying the un-banded product, which is the exact defect the comment above
  it says was fixed. `subs.cljs:66` also selects a key that never arrives. One
  line to fix: add `:market-multiplier` to the `select-keys` vector.

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

  Specific things already spotted:

  - `league-import-handler` has no outer `try/catch`, so a malformed JSON body
    500s where `rankings-handler` would have answered cleanly. The two should
    agree.
  - `espn/http-get-string` returns nil on any non-200, producing exactly the
    silently empty column CLAUDE.md names as the worst ingestion failure — the
    same shape as the FantasyPros 429 it warns about.
  - `parallel/all`'s docstring notes `best-effort` does not catch `Error`, so
    the escape path and the swallow path disagree about what a failure is.
  - The frontend `:http` effect routes every non-2xx to `on-failure` with
    `(:error body)`, which assumes every handler returns `{:error msg}`. Worth
    confirming that holds everywhere.
  - `:waivers-failed` and `:recompute-failed` deliberately keep stale data
    readable rather than blanking it. That is the good pattern; it should be
    stated as the convention rather than left as two coincidences.
