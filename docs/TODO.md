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
