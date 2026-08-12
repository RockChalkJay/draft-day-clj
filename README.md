# draft-day-clj
Fantasy football draft assistant geared towards auction drafts

## Benchmark harness

`dev/draft_day/benchmark/` is a research tool — not part of the shipped API or
SPA — that scores a ranking model (or two, head-to-head) against real
historical draft outcomes, gated against post-hoc leakage.

```
lein run -m draft-day.benchmark.report --help
```

Some flags to start with:

- `--models M[,M]` / `--compare A B` — score one or more models, or two side
  by side
- `--seasons 2021-2025` — which seasons to score
- `--simulate` — draft a team off each board and score realized points, the
  metric closest to the actual decision
- `--source-report` — per-source depth, join rates, and vintage gate for the
  underlying data
- `--power-report` — what the corpus can resolve before running a sweep

There are also tuning flags (`--scoring`, `--truth`, `--pool`, `--adp-source`,
`--projection-source`) for adjusting the scoring format, truth metric, pool
size, and data sources. `--help` prints the full flag list and the currently
registered models.

Example:

```
lein run -m draft-day.benchmark.report --compare points points+adp --simulate
```

See the docstrings in `dev/draft_day/benchmark/core.clj`, `report.clj`, and
`vintage.clj` for the harness's own architecture (vintage/leakage gating,
paired season-block-bootstrapped statistics, the draft-simulation metric).

## TODO

- ~~Player budgeting~~

- **Persistence**

- ~~Don't position filter on suggestion cards~~

- ~~Remove strategy tabs - they don't add value~~

- **Injury history not just current health. Color code for serious injury or suspension**

- **Remove the 🚨 for tier cliffs. Postion views with tier coloring accomplish the same thing in a clean way.**

- **Fix tier row color bug**

- **Warning when a nominated player would cause 3 or more shared bye weeks at the same position**

- ~~Scoring-aware FantasyPros enrichments (ECR + market price)~~
