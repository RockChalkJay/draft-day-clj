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

Env vars the ingestion pipeline reads: `DRAFTDAY_OFFLINE=1` forces the bundled sample universe (no network calls — useful for tests/dev); `DRAFTDAY_CACHE_TTL_HOURS` controls the disk-cache freshness window (default 8).

## Architecture

### Split and statelessness

`src/clj` is the backend, `src/cljs` is the frontend SPA; `src/cljs` is also on the backend's classpath (see `project.clj` comment) purely so shadow-cljs can find it via `:lein true`.

The server is intentionally stateless about the draft: the only server-side state is a shared, in-memory `players` universe cache (an atom in `api/routes.clj`). All draft-in-progress state (teams, picks, bankrolls) lives in the browser's re-frame db and is round-tripped as a `league-state` map on every `POST /api/rankings` call. This means the rankings engine is a pure function of `(players, scoring, roster-config, league-state, profile)` — no session state to reconcile server-side.

### Backend: ingestion -> rankings engine -> API

**Ingestion** (`draft_day/ingestion/`) resolves the player universe with a fallout chain: `offline-sample -> fresh-cache -> live -> stale-cache -> bundled-sample` (`pipeline.clj`). The base universe comes from Sleeper (`sleeper.clj`, free/keyless); FantasyPros (`fantasypros.clj`, scraped via Jsoup for ECR/tier/floor-ceiling inputs) and ESPN (`espn.clj`, live auction values, fetched via raw `java.net.http` because http-kit's client chokes on the ~37MB response) are best-effort enrichments left-joined onto it by `(name, position)` key (`merge.clj`, `match.clj`). Cache is Transit on disk (`data/players_cache.transit`); the bundled fallback is EDN on the classpath (`resources/sample_players.edn`).

**Rankings engine** (`draft_day/rankings/`) is a numbered pipeline, split into a static half (computed once per scoring/roster config) and a live half (recomputed after every pick), orchestrated by `engine.clj`:

- `static-rankings`: `scoring` (stat line -> points) -> `projections` (floor/ceiling band from expert-rank disagreement) -> `profiles` (blend effective points toward floor/ceiling per active strategy profile) -> `tiers` (cliff detection per position) -> `replacement` (replacement level + VORP)
- `live-valuation`: `tcm` (tier-cliff multiplier, live/undrafted-only) + `pdm` (positional demand multiplier) -> scarcity-adjust VORP (Scarcity profile only) -> `value` (VBD -> stable salary-cap dollars) -> `inflation` / `inflation-index` (conserving inflation + per-position live market + phase decay) -> `worth`/`bargain` (Value scaled by live inflation, minus Worth)

Strategy **profiles** (`profiles.clj`) are named `{:risk :ceiling :scarcity :inflation-sensitivity}` weight vectors (e.g. Balanced, Floor, Ceiling, Scarcity) — the engine is profile-agnostic; adding a profile is "add a vector," not an engine change. `api/routes.clj` additionally computes Floor/Ceiling worths per player (`lens-worths`) so the client can badge players whose valuation is sensitive to the active lens.

**API** (`api/routes.clj`): `GET /api/players` returns the cached universe; `POST /api/rankings` takes scoring/roster config + `league-state` and returns the fully valued board. Also serves the compiled SPA from `resources/public`.

### Frontend: re-frame

Standard re-frame split: `db.cljs` (app-db shape, roster/team helpers, the column catalog), `events.cljs`, `subs.cljs` (not read in detail — check before editing), `fx.cljs` (effect handlers: a small `fetch`-based `:http` effect, no extra HTTP deps; `:persist!` to localStorage), `views/` (`board`, `roster`, `controls`, `columns`, `modal`, `settings`).

Flow: `:boot` merges persisted localStorage state into `default-db` -> `:fetch-players` (`GET /api/players`) -> `:recompute` (`POST /api/rankings` with the current `league-state`, built from `:teams`/`:drafted`/`:picks`) -> `:ranked-loaded` stashes the response under `:ranked`. Every mutating event (draft picks, config changes, column edits) re-dispatches `:recompute` so the board reflects live valuation immediately.

A `persist` interceptor (`events.cljs`) writes a whitelisted slice of db (`db/persist-keys`) to localStorage after any event that includes it — this is the only persistence; there's no server-side session.

The board is data-driven: `:columns` is an ordered vector of `{:key :visible?}` against `db/column-catalog`, so columns can be toggled/reordered without touching render code; `db/reconcile-columns` handles migrating a persisted column list when the catalog changes.
