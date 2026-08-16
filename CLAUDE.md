# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A fantasy football auction-draft assistant (auction-focused, VBD). Clojure backend serves a stateless JSON API; a ClojureScript/re-frame SPA renders a live draft board and drives all draft state from the browser.

## Commands

Backend (Leiningen, `project.clj`):
- `lein repl` — nREPL for backend dev
- `lein test` — run all Clojure tests
- `lein test draft-day.rankings.value-test` — run one test namespace
- `lein test :only draft-day.rankings.value-test/value-conserves-to-budget` — run one test
- `lein run` — start the http-kit server (`draft-day.server/-main`), reads `PORT` (default 8080)

Frontend (shadow-cljs, `:lein true` so it shells to `lein` for the JVM/classpath):
- `npx shadow-cljs watch app` (or `npm run watch`) — dev build with hot reload, served alongside `resources/public` on port 8280
- `npx shadow-cljs release app` (or `npm run release`) — production build
- `npm test` (`shadow-cljs compile test && node out/node-tests.js`) — the `:node-test` build, covering the cljs-only namespaces (`events`/`fx`/`subs`) that `lein test` cannot reach. `src/cljc` is covered by `lein test` instead, so cljs tests are only worth writing for genuinely browser-side behaviour.

**JAVA_HOME gotcha**: Homebrew's JDK is keg-only and not on PATH. `lein` finds Java fine, but `shadow-cljs` needs it exported first:
```
export JAVA_HOME=/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home
```

Env vars the ingestion pipeline reads: `DRAFTDAY_OFFLINE=1` forces the bundled sample universe (no network calls — useful for tests/dev); `DRAFTDAY_CACHE_TTL_HOURS` controls the disk-cache freshness window (default 24).

Research harnesses (Leiningen `:dev` profile, `dev/` on the source path — active by default for `repl`/`test`/`run`, so no `with-profile` needed):
- `lein run -m draft-day.benchmark.report -- --help` — score a `rankings.model` against real historical outcomes; see `dev/draft_day/benchmark/report.clj` for flags (`--models`, `--seasons`, `--compare`, `--power-report`, `--simulate`, etc.)
- `lein run -m draft-day.replay.report` — replay a real historical auction draft and compare Worth's picks against what was actually paid

## Not the shipped app

`dev/` (`draft_day.benchmark.*`, `draft_day.replay.*`) and their tests under `test/draft_day/benchmark/` are a research harness for validating ranking formulas against real outcomes before they ship — not part of the deployed API or SPA. Local caches live under `data/benchmark_cache/` and `data/replay_cache/` (gitignored, re-fetchable). See `dev/draft_day/benchmark/core.clj`, `.../report.clj`, and `.../vintage.clj` docstrings for the harness's own architecture (vintage/leakage gating, paired season-block-bootstrapped statistics, draft-simulation metric); it's involved enough to warrant reading those directly rather than duplicating here.

## Architecture

### Split and statelessness

`src/clj` is the backend, `src/cljs` is the frontend SPA; `src/cljs` is also on the backend's classpath (see `project.clj` comment) purely so shadow-cljs can find it via `:lein true`.

The server is intentionally stateless about the draft: the only server-side state is a shared, in-memory `players` universe cache (an atom in `api/routes.clj`). All draft-in-progress state (teams, picks, bankrolls) lives in the browser's re-frame db and is round-tripped as a `league-state` map on every `POST /api/rankings` call. This means the rankings engine is a pure function of `(players, scoring, roster-config, league-state)` — no session state to reconcile server-side.

### Backend: ingestion -> rankings engine -> API

**Ingestion** (`draft_day/ingestion/`) resolves the player universe with a fallout chain: `offline-sample -> fresh-cache -> live -> stale-cache -> bundled-sample` (`pipeline.clj`). The base universe comes from Sleeper (`sleeper.clj`, free/keyless); FantasyPros (`fantasypros.clj`, scraped via Jsoup for ECR/tier/floor-ceiling inputs) and ESPN (`espn.clj`, live auction values, fetched via raw `java.net.http` because http-kit's client chokes on the ~37MB response) are best-effort enrichments left-joined onto it by `(name, position)` key (`merge.clj`, `match.clj`).

Every enrichment fetch goes out at once: `pipeline/enrichment-tasks` names them all as best-effort thunks keyed by their `:sources` label and `parallel.clj` runs them concurrently (cancelling the stragglers if the deref escapes), while the *joins* stay sequential and in `scoring/formats` order so the `:sources` report reads the same every run. Per-vendor politeness lives with the vendor, not with `parallel.clj` — `fantasypros/fetch-page` holds that host to `max-in-flight` connections, because ten simultaneous scrapes from one IP earn a 429, and a 429 here is not an exception but a silently empty column cached for a day.

Columns that vendors publish *per scoring format* — FantasyPros ECR, auction values and RB/WR/TE positional tiers, Sleeper ADP — are fetched for all three formats and stored side by side under `:vendor/by-format`; the format is required on every FantasyPros fetch and an unknown one throws (`fantasypros/cheatsheet-url`), because falling back to PPR is silent in the worst way — the join succeeds and reports a full row count while a standard league reads PPR prices. `rankings/vendor.clj` flattens the matching one onto the flat keys at request time (`scoring/format-of` maps a custom config to its nearest published format by reception weight). The universe cache is shared across leagues, so the choice cannot be made at ingestion — baking in PPR is what made a standard league read PPR tiers, rank spread, market prices and ADP. ESPN is deliberately *not* format-scoped: it publishes the same auction value under both its PPR and STANDARD rank types. FantasyPros' QB/K/DST positional cheatsheets are unscoped for the same reason — the `ppr-`/`half-point-ppr-` URLs 302 to the *overall* cheatsheet for those three, so following the redirect would parse a whole-board page as one position (`fantasypros/pos-formats` is what encodes this, and it is load-bearing rather than an optimization). Cache is Transit on disk (`data/players_cache.transit`); the bundled fallback is EDN on the classpath (`resources/sample_players.edn`).

**League import** (`draft_day/ingestion/league_import.clj` + one namespace per provider, e.g. `league_import/sleeper.clj`) pulls a manager's real scoring/roster settings from a fantasy host, backend-proxied so a future provider needing server-side auth (ESPN, Yahoo) is a drop-in. It's a multimethod dispatch on `:provider` — `fetch-raw-league` (network) and `normalize-league` (pure) — rather than a protocol, matching the "one namespace per provider, called by name" convention `pipeline.clj` already uses for `sleeper.clj`/`espn.clj`/`fantasypros.clj`. Adding a provider is a new file with two `defmethod`s plus a `:require` in `routes.clj` for side-effecting registration; no change to the dispatcher itself.

**Rankings engine** (`draft_day/rankings/`) is a numbered pipeline, split into a static half (computed once per scoring/roster config) and a live half (recomputed after every pick), orchestrated by `engine.clj`:

- `static-rankings`: `model` (stat line -> `:points`, via `rankings/model.clj`) -> `projections` (floor/ceiling band from expert-rank disagreement) -> `replacement` (replacement level) -> VORP -> `tiers` (every registered tier strategy, at both scales). VORP before tiering is the one ordering constraint: the overall tier scale is cut on `:vorp`.
- `live-valuation`: `value` (VBD -> stable salary-cap dollars) -> `inflation` / `inflation-index` (conserving inflation + per-position live market + phase decay) -> `worth`/`bargain` (Value scaled by live inflation, minus Worth). It also assocs a per-player `tcm` (tier-cliff multiplier, live/undrafted-only) — a display-only board signal, *not* an input to Value or Worth.

The scoring step is a multimethod dispatch (`rankings/model.clj`'s `score-board`, keyed on a `:model` keyword — `:points` by default, plain `draft-day.scoring/with-points`) rather than a hardcoded call, so a candidate formula validated in the benchmark harness (see below) ships by passing a different keyword into `static-rankings`, not by porting code out of the harness. `rankings/model/blend.clj` registers the ADP/ECR-blended and rookie-capital variants used there; none are wired to the live API today.

Valuation is hardwired to the Balanced weighting; there is no user-selectable strategy profile (the feature was removed — effective points equal raw points, VORP is not scarcity-adjusted, and inflation-sensitivity is fixed at 1.0). The positional-demand multiplier (PDM) that once rode alongside was removed too: it was computed on every pick but never fed Value or Worth.

**Tier strategies** (`rankings/tiers.clj` + `rankings/tiers/*.clj`) are a seam shaped exactly like `rankings/model.clj`: `tier-board` is a multimethod on a strategy keyword, the baseline lives in `tiers.clj` and variants register by being `:require`d for side effects. A strategy returns `{:overall {id tier} :position {id tier}}` — id-keyed maps, not a board, so the orchestrator owns key placement. `static-rankings` runs *every* registered strategy and ships them side by side as `:tiers {strategy {:overall n :position n}}`, which is what lets the board switch technique with no round trip; `:tier` remains a back-compat alias for the default strategy's positional tier.

Two strategies ship. `:cliffs` cuts a *configured number* of tiers at the pool's biggest gaps (`tiers/cut-points`), rather than wherever a drop passes a threshold. `tiers/tier-counts` is that configuration — `{:overall 16 :position 8}`, code-level, no user-facing setting — and `tiers/MIN-TIER-SIZE` (2) forbids the singleton tiers a forced count otherwise produces, since a group of one cannot support the claim "these are interchangeable". Counts are targets: a pool too small to hold that many tiers at the minimum size gets as many as fit, which is why QB lands on 6 and K on 4 while RB and WR reach 9. The overall scale is cut on `:vorp`, the only score comparable across positions; everything at or below replacement shares a tail tier (VORP 0.0 is that boundary), and K/DST get a floor from `tiers/tier-floor` since they are absent from the replacement map.

Two reversals are load-bearing there. Gaps are ranked **absolutely**, not by `relative-drop`: a relative drop is measured against the falling player, so it grows without bound as the metric decays toward zero and drags nearly every cut into the tail — on the sample board that produced one 13-player top tier above four 2-player tiers at the bottom, exactly backwards. The old objection to absolute gaps (a fat gap deep in the tail outranking a real one up top) is answered by *where* the cut happens rather than by the metric, since the pool is already truncated at replacement. And the count no longer follows the data, so a perfectly smooth position gets cut anyway — deliberate, because a fixed count is what makes tier 3 mean the same thing at every position and in every scoring format. `relative-drop` survives in `tiers.clj` as `tcm`'s input; `tcm/DROP-THRESHOLD` is now its own constant with no counterpart to drift from.

`:ecr` is FantasyPros' published expert tier, *both* scales looked up rather than derived: ingestion scrapes the overall consensus cheatsheet (`:fantasypros/ecr-tier`) and each position's own cheatsheet (`:fantasypros/ecr-pos-tier`), which is a genuinely finer cut — 12 tiers across 177 RBs, where those RBs share only a handful of overall tiers. It is a peer strategy rather than an override, which is what fixes the two problems that got the override removed: partial coverage now reads as an explicit untiered bucket (absent key -> grey, dashed, "Unranked"; never a borrowed cliff tier), and the two scales are separate keys instead of one dense-ranked number meaning neither.

**API** (`api/routes.clj`): `GET /api/players` returns the cached universe; `POST /api/rankings` takes scoring/roster config + `league-state` and returns the fully valued board (400 if the scoring config has no non-zero weights — an all-zero board is a lie, not a board); `POST /api/league/import` takes `{:provider :league-id}` and proxies to `league-import/import-league`, whose reply carries `:unsupported-scoring` — the league's own rules that a flat stat-line model cannot score (FG distance buckets, DST points-allowed tiers, yardage bonuses), which Settings shows rather than reporting a bare success. Also serves the compiled SPA from `resources/public`.

### Frontend: re-frame

Standard re-frame split: `db.cljc` (app-db shape, roster/team helpers, the column catalog — shared with the JVM so `lein test` reaches it), `events.cljs`, `subs.cljs` (not read in detail — check before editing), `fx.cljs` (effect handlers: a small `fetch`-based `:http` effect, no extra HTTP deps; `:persist!` to localStorage; `:debounce` to coalesce a burst of the same event), `views/` (`board`, `roster`, `controls`, `columns`, `modal`, `settings`).

Flow: `:boot` merges persisted localStorage state into `default-db` -> `:fetch-players` (`GET /api/players`) -> `:recompute` (`POST /api/rankings` with the current `league-state`, built from `:teams`/`:drafted`/`:picks`) -> `:ranked-loaded` stashes the response under `:ranked`. Every mutating event (draft picks, config changes, column edits) re-dispatches `:recompute` so the board reflects live valuation immediately.

Each `:recompute` stamps a monotonic `:recompute-seq` that `:ranked-loaded` checks before writing: a full re-rank takes long enough that overlapping requests answer out of order, and without the check a reply computed under the *previous* scoring config could win and stick. `db/reconcile-config` repairs a persisted `:config` at boot the same way `reconcile-columns` repairs `:columns` — localStorage carries no schema stamp, so every shape the app has ever written has to be repairable in place.

A `persist` interceptor (`events.cljs`) writes a whitelisted slice of db (`db/persist-keys`) to localStorage after any event that includes it — this is the only persistence; there's no server-side session.

The board is data-driven: `:columns` is an ordered vector of `{:key :visible?}` against `db/column-catalog`, so columns can be toggled/reordered without touching render code; `db/reconcile-columns` handles migrating a persisted column list when the catalog changes.

Which tier technique the board groups by is `(:config :tier-strategy)` against `db/tier-strategy-catalog`, switched from a segmented control beside the position filter. It persists but deliberately skips `:recompute` — the server already sent every strategy — and `:board-players` is the single place that resolves it, assoc'ing each row's `:tier` from `[:tiers strategy scale]` where the scale is positional iff a position filter is active. Everything downstream (row striping, the legend, the `:tier` column, sorting) keeps reading one `:tier` key and knows nothing about strategies. The catalog keys must match `rankings.tiers/registered`; a `lein test` assertion pins the two together, because a button dispatching an unregistered keyword does not error — it silently renders the whole board as unranked.

`(:config :scoring)` is either a preset keyword (`:standard`/`:half-ppr`/`:ppr`) or, once a manager customizes or imports, a full `{stat-key weight}` map — mode is derived (`:scoring-mode` sub), never stored separately, so an imported scoring map opens directly in Custom mode. Coercing that field to a weight map is `scoring/resolve-config`, used by both `routes/resolve-scoring` and the `:scoring-format` sub: the browser derives which vendor format its league reads from the same field the server does, and hand-written copies of that `cond` had already drifted on the string spellings. The preset table and `stat-keys` live in shared `src/cljc/draft_day/scoring.cljc` so the editor can seed a custom map synchronously; there is deliberately no endpoint serving them, because seeding from an async fetch is what used to write a nil scoring config. `db/scoring-catalog` groups the stat keys (Passing/Rushing/Receiving/Misc/Kicking/Defense) for the custom editor in `views/settings.cljs`; it's presentational metadata only, not the same shape as `column-catalog` (flat, no grouping). League import (`:import-league` in `events.cljs`) POSTs to `/api/league/import` rather than calling a provider's API directly from the browser — the shared `fetch`-based `:http` effect (`fx.cljs`) checks `resp.ok` and routes any non-2xx to `on-failure` with the body's `:error`.
