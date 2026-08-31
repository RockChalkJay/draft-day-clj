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
