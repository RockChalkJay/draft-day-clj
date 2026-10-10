# Development

```bash
lein test                                        # all Clojure tests
lein test draft-day.rankings.value-test          # one namespace
lein test :only draft-day.rankings.value-test/value-conserves-to-budget
npm test                                         # the ClojureScript node-test build
lein test :integration                           # hits live vendors; skips without DRAFTDAY_ESPN_* env vars
```

`src/cljc` is on the JVM classpath, so `lein test` already covers the shared
db/scoring code. `npm test` exists for the cljs-only namespaces (every
`*_test.cljs` under `test/`: `events`, `subs`, `waivers`, `matchup` and the
`views/`) that `lein test` cannot reach, so a ClojureScript test is only worth
writing for genuinely browser-side behaviour.

## Seeing the in-season half work

The current season's weekly file does not exist until week 1 is played, and last
season's is a season with nothing left to project. `DRAFTDAY_AS_OF_WEEK`
truncates the weekly rows at a week, so a finished season replays as one in
progress:

```bash
DRAFTDAY_AS_OF_WEEK=8 lein run     # a real week-8 board, against real data
```

Without it, the Waivers tab in preseason is the preseason board — correctly, and
it says so — but nothing about the blend, the bids or the trend column can be
seen doing anything.

## Research harnesses

`dev/` holds research harnesses and command-line tools. **They are not part of
the shipped API or SPA**, and their caches under `data/` are gitignored and
re-fetchable. All of it is in the `:dev` profile, which Leiningen activates by
default for `run`/`test`/`repl` — no `with-profile` needed.

Besides the two documented below, `dev/draft_day/faab/` holds the waiver-bid
harnesses (`replay`, `interest`, `sweep`, `report`), `dev/draft_day/lineup_report.clj`
compares the lineup upgrade with the bench delta, and `dev/draft_day/tools/`
holds the CLIs that save trending lists and weekly projections and regenerate
the committed sample universe and id crosswalk. Each file's namespace docstring
says what it does and how to run it.

### Benchmark

Scores a ranking model — or two, head to head — against real historical draft
outcomes, gated against post-hoc leakage.

```
lein run -m draft-day.benchmark.report --help
```

- `--models M[,M]` / `--compare A B` — score one or more models, or two side by side
- `--seasons 2021-2025` — which seasons to score
- `--simulate` — draft a team off each board and score realized points, the
  metric closest to the actual decision
- `--source-report` — per-source depth, join rates, and vintage gate
- `--power-report` — what the corpus can resolve before running a sweep

Plus tuning flags (`--scoring`, `--truth`, `--pool`, `--adp-source`,
`--projection-source`). Example:

```
lein run -m draft-day.benchmark.report --compare points points+adp --simulate
```

This is the seam the shipped engine leaves open: scoring is a multimethod
dispatch on a `:model` keyword, so a formula validated here ships by passing a
different keyword into `static-rankings`, not by porting code out of `dev/`.

### Replay

Replays a real historical auction and compares what Draft Day's Worth said
against what the room actually paid.

```
lein run -m draft-day.replay.report
```

See the docstrings in `dev/draft_day/benchmark/core.clj`, `report.clj` and
`vintage.clj` for the harness architecture — vintage/leakage gating, paired
season-block-bootstrapped statistics, the draft-simulation metric.
