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

**Ingestion** (`draft_day/ingestion/`) resolves the player universe with a fallout chain: `offline-sample -> fresh-cache -> live -> stale-cache -> bundled-sample` (`pipeline.clj`). The base universe comes from Sleeper (`sleeper.clj`, free/keyless); FantasyPros (`fantasypros.clj`, scraped via Jsoup for ECR/tier/floor-ceiling inputs) and ESPN (`espn.clj`, live auction values, fetched via raw `java.net.http` because http-kit's client chokes on the ~37MB response) are best-effort enrichments left-joined onto it by `(name, position)` key (`merge.clj`, `match.clj`). Cache is Transit on disk (`data/players_cache.transit`); the bundled fallback is EDN on the classpath (`resources/sample_players.edn`).

**League import** (`draft_day/ingestion/league_import.clj` + one namespace per provider, e.g. `league_import/sleeper.clj`) pulls a manager's real scoring/roster settings from a fantasy host, backend-proxied so a future provider needing server-side auth (ESPN, Yahoo) is a drop-in. It's a multimethod dispatch on `:provider` — `fetch-raw-league` (network) and `normalize-league` (pure) — rather than a protocol, matching the "one namespace per provider, called by name" convention `pipeline.clj` already uses for `sleeper.clj`/`espn.clj`/`fantasypros.clj`. Adding a provider is a new file with two `defmethod`s plus a `:require` in `routes.clj` for side-effecting registration; no change to the dispatcher itself.

**Rankings engine** (`draft_day/rankings/`) is a numbered pipeline, split into a static half (computed once per scoring/roster config) and a live half (recomputed after every pick), orchestrated by `engine.clj`:

- `static-rankings`: `model` (stat line -> `:points`, via `rankings/model.clj`) -> `projections` (floor/ceiling band from expert-rank disagreement) -> `tiers` (cliff detection per position) -> `replacement` (replacement level + VORP)
- `live-valuation`: `value` (VBD -> stable salary-cap dollars) -> `inflation` / `inflation-index` (conserving inflation + per-position live market + phase decay) -> `worth`/`bargain` (Value scaled by live inflation, minus Worth). It also assocs a per-player `tcm` (tier-cliff multiplier, live/undrafted-only) — a display-only board signal, *not* an input to Value or Worth.

The scoring step is a multimethod dispatch (`rankings/model.clj`'s `score-board`, keyed on a `:model` keyword — `:points` by default, plain `scoring/with-points`) rather than a hardcoded call, so a candidate formula validated in the benchmark harness (see below) ships by passing a different keyword into `static-rankings`, not by porting code out of the harness. `rankings/model/blend.clj` registers the ADP/ECR-blended and rookie-capital variants used there; none are wired to the live API today.

Valuation is hardwired to the Balanced weighting; there is no user-selectable strategy profile (the feature was removed — effective points equal raw points, VORP is not scarcity-adjusted, and inflation-sensitivity is fixed at 1.0). The positional-demand multiplier (PDM) that once rode alongside was removed too: it was computed on every pick but never fed Value or Worth.

**API** (`api/routes.clj`): `GET /api/players` returns the cached universe; `POST /api/rankings` takes scoring/roster config + `league-state` and returns the fully valued board; `GET /api/scoring/presets` returns the named scoring presets plus `scoring/stat-keys` (the full set of stat keys the custom scoring editor and league import may touch); `POST /api/league/import` takes `{:provider :league-id}` and proxies to `league-import/import-league`. Also serves the compiled SPA from `resources/public`.

### Frontend: re-frame

Standard re-frame split: `db.cljs` (app-db shape, roster/team helpers, the column catalog), `events.cljs`, `subs.cljs` (not read in detail — check before editing), `fx.cljs` (effect handlers: a small `fetch`-based `:http` effect, no extra HTTP deps; `:persist!` to localStorage), `views/` (`board`, `roster`, `controls`, `columns`, `modal`, `settings`).

Flow: `:boot` merges persisted localStorage state into `default-db` -> `:fetch-players` (`GET /api/players`) -> `:recompute` (`POST /api/rankings` with the current `league-state`, built from `:teams`/`:drafted`/`:picks`) -> `:ranked-loaded` stashes the response under `:ranked`. Every mutating event (draft picks, config changes, column edits) re-dispatches `:recompute` so the board reflects live valuation immediately.

A `persist` interceptor (`events.cljs`) writes a whitelisted slice of db (`db/persist-keys`) to localStorage after any event that includes it — this is the only persistence; there's no server-side session.

The board is data-driven: `:columns` is an ordered vector of `{:key :visible?}` against `db/column-catalog`, so columns can be toggled/reordered without touching render code; `db/reconcile-columns` handles migrating a persisted column list when the catalog changes.

`(:config :scoring)` is either a preset keyword (`:standard`/`:half-ppr`/`:ppr`) or, once a manager customizes or imports, a full `{stat-key weight}` map — mode is derived (`:scoring-mode` sub: `(if (map? scoring) :custom scoring)`), never stored separately, so an imported scoring map opens directly in Custom mode. `db/scoring-catalog` groups the stat keys (Passing/Rushing/Receiving/Misc/Kicking/Defense) for the custom editor in `views/settings.cljs`; it's presentational metadata only, not the same shape as `column-catalog` (flat, no grouping). League import (`:import-league` in `events.cljs`) POSTs to `/api/league/import` rather than calling a provider's API directly from the browser — the shared `fetch`-based `:http` effect (`fx.cljs`) treats any parseable JSON as success regardless of HTTP status, so import failures are detected by checking for an `:error` key in the response body, not via `on-failure`.
