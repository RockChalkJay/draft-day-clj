# Tools and research harnesses

Everything under `dev/` is for the people building Draft Day, not for the people
using it. **None of it is part of the shipped API or SPA.** It lives in the
`:dev` profile, which Leiningen activates by default for `run`, `test` and
`repl`, and the `:uberjar` profile leaves it out. No `with-profile` is needed.

Each tool is a `-main` namespace:

```bash
lein run -m draft-day.tools.trends
```

Output goes under `data/`, which is gitignored and re-fetchable, with two
deliberate exceptions: `tools.snapshot` and `tools.refresh-player-ids` rewrite
files that are committed, so their diff is the review. Each namespace's
docstring is the full reference for that tool; this page is the map.

| Command | What it does | Network | Writes |
| --- | --- | --- | --- |
| [`tools.trends`](#toolstrends) | save Sleeper's trending add/drop lists as JSON | yes | `data/trends/` |
| [`tools.projections`](#toolsprojections) | save the week's raw projection line, kickoffs and injury list; report on gaps | yes | `data/projections/` |
| [`tools.snapshot`](#toolssnapshot) | regenerate the committed offline sample universe | yes | `resources/sample_players.edn` |
| [`tools.refresh-player-ids`](#toolsrefresh-player-ids) | regenerate the committed id crosswalk | yes | `resources/player_ids.edn` |
| [`benchmark.report`](#benchmarkreport) | score ranking models against real historical outcomes | yes (cached) | `data/benchmark_cache/` |
| [`replay.report`](#replayreport) | replay real auctions and compare Worth with what rooms paid | yes (cached) | `data/replay_cache/` |
| [`faab.report`](#faab-harnesses) | crawl and report what Sleeper leagues pay for waiver claims | yes (cached) | `data/faab_cache/` |
| [`faab.interest`](#faab-harnesses) | fit the rival-interest weights to real claims | yes (cached) | |
| [`faab.sweep`](#faab-harnesses) | score the bid model's chosen constants one at a time | yes (cached) | |
| [`faab.replay`](#faab-harnesses) | score the bid model on one league's real auctions | yes (cached) | |
| [`lineup-report`](#lineup-report) | compare the lineup upgrade with the bench delta on a real league | yes | |

## Collecting data you cannot get later

Two sources keep no history, so the only way to have last Tuesday's data is to
have saved it last Tuesday.

### `tools.trends`

Sleeper's trending add and drop lists. A saved one is the only record of what
the site's managers were adding and dropping at that moment.

```bash
lein run -m draft-day.tools.trends [--types add,drop] [--lookbacks 48] \
                                   [--limit 100] [--dir data/trends]
```

Each run is one fetch and one file: `<dir>/<season>/week-NN/<UTC time>.json`,
where `NN` is the number of weeks played according to nflverse (`week-unknown`
when that cannot be read). The file is the same snapshot the app writes
whenever the board fetches the list, so a running server also fills this
directory. If any list fails or comes back empty nothing is written, so a file
is always complete. Exit code 0 means written, 1 means a list failed.

### `tools.projections`

Sleeper revises a past week's projection line after the games, so the line the
board showed on Sunday morning cannot be fetched afterwards. This saves it,
along with ESPN's kickoffs and the complete injury list.

```bash
lein run -m draft-day.tools.projections [--week N] [--dir data/projections]
lein run -m draft-day.tools.projections --report [--dir data/trends] \
                                        [--gap-hours 72] [--runs-per-week 3]
lein run -m draft-day.tools.projections --help
```

The file is `<dir>/<season>/week-NN/<UTC time>.json`, where `NN` is the week the
line is *for* (the week being played). `tools.trends` files by weeks *played*
instead; both numbers are in each document as `target_week` and `through_week`.
If the line comes back empty nothing is written; a failed injury or kickoff
fetch is reported on stderr and leaves that key null or empty while the line
still lands. Exit code 0 means written (or reported), 1 means the line failed or
a flag was bad.

`--report` lists the snapshots under a directory, per week, with the longest gap
between two of them. It flags a gap over `--gap-hours`, a finished week with
fewer than `--runs-per-week` snapshots, a week missing between the first and the
last, and a newest snapshot older than `--gap-hours`. It works on `data/trends`
as well, where hourly runs make the per-week count far higher.

**Scheduling.** The author runs `tools.projections` from cron at 10:00 AM
Wednesday (after waivers clear, about 3:10 AM in that league; this is a
per-league setting), 6:30 PM Thursday (before Thursday night's 8:15 PM kickoff)
and 12:15 PM Sunday (after the 1:00 PM games' inactives at 11:30 AM). An
international game kicks off earlier, typically 9:30 AM. All of those are
Eastern times; the files are named in UTC. `tools.trends` is worth running on a
schedule all season for the same reason.

## Regenerating committed fixtures

### `tools.snapshot`

Regenerates `resources/sample_players.edn`, the committed offline universe. That
file is what `DRAFTDAY_OFFLINE=1` serves and what ingestion falls back to when
the network is gone, so the tests and offline development see whatever it
contains. Being a captured fixture, it goes stale silently.

```bash
lein run -m draft-day.tools.snapshot                  # current season
lein run -m draft-day.tools.snapshot --season 2026
lein run -m draft-day.tools.snapshot --allow-partial
```

A partial capture is refused by default (exit 1), because a sample quietly
missing a source is exactly the failure this tool exists to prevent. Degrading
to one has to be a decision somebody typed.

### `tools.refresh-player-ids`

Regenerates `resources/player_ids.edn`, the pinned Sleeper-to-GSIS crosswalk
the app derives `:player-id` from. It is deliberately out of band: the snapshot
being committed is what makes id derivation pure, offline and reproducible, so
refreshing it is a reviewed event, not something ingestion does behind your
back.

```bash
lein run -m draft-day.tools.refresh-player-ids
lein run -m draft-day.tools.refresh-player-ids --allow-changes
```

The diff guard is the point. A refresh may add players and may fill in a GSIS id
that was previously absent, because both leave existing ids alone. It must never
silently *change* a mapping that already resolved, since saved drafts are keyed
by the id that mapping produced. Those are listed and the write is refused
(exit 1) unless `--allow-changes` is passed.

## Research harnesses

These answer "is the model any good, and would a change help?" Nothing ships on
argument: a formula that wins here ships by passing a different keyword into
`static-rankings`, because scoring is a multimethod on a `:model` keyword, not by
porting code out of `dev/`.

### `benchmark.report`

Scores a ranking model, or two head to head, against real historical draft
outcomes, gated against post-hoc leakage.

```bash
lein run -m draft-day.benchmark.report --help
lein run -m draft-day.benchmark.report --compare points points+adp --simulate
```

The main flags: `--models M[,M]` and `--compare A B` pick the models;
`--seasons 2021-2025` the years; `--simulate` drafts a team off each board and
scores the realized points, the metric closest to the actual decision; `--vorp`
simulates a raw-points board against a VORP board and bootstraps the
difference; `--source-report` prints per-source depth, join rates and the
vintage gate; `--power-report` says what the corpus can resolve before a sweep.
Tuning flags are `--scoring`, `--truth`, `--pool`, `--adp-source` and
`--projection-source` (`sleeper`, `fftoday` or `fp-archive`). The models are
`adp`, `ecr`, `points`, `points+adp`, `points+adp+fade` and
`points+rookie-capital`. Models decide the order; `--scoring` decides how points
are counted. Season coverage is limited by the inputs, not the outcomes; see
`--source-report`.

The sources live in `dev/draft_day/benchmark/sources/`. For the harness design
(vintage and leakage gating, paired season-block bootstrapped statistics, the
draft-simulation metric), read the docstrings in
[`benchmark/core.clj`](../benchmark/core.clj),
[`report.clj`](../benchmark/report.clj) and [`vintage.clj`](../benchmark/vintage.clj).

### `replay.report`

Replays a real historical auction through the engine and compares what Worth
said with what the room actually paid, against the raw Value and Mkt baselines.

```bash
lein run -m draft-day.replay.report                 # use the cached corpus
lein run -m draft-day.replay.report --rebuild       # recrawl, resuming state
lein run -m draft-day.replay.report --fresh         # recrawl from scratch
lein run -m draft-day.replay.report 12345 67890     # score these draft ids
```

A crawl takes optional bounds as `--max-users=N`, `--max-drafts=N` and
`--max-drafts-per-user=N`. `replay/price_curve.clj` measures how auction money
distributes by rank across the corpus, which the benchmark's draft simulator
uses as its opponents' bidding.

### FAAB harnesses

Four tools for the waiver-bid model in
[`rankings/faab.clj`](../../../src/clj/draft_day/rankings/faab.clj):

```bash
lein run -m draft-day.faab.report                   # what Sleeper leagues pay
lein run -m draft-day.faab.report --crawl           # crawl more, then report
lein run -m draft-day.faab.interest                 # fit rival interest, ablate
lein run -m draft-day.faab.sweep --only pseudo-bids # one constant at a time
lein run -m draft-day.faab.replay --league 123 --league-prior
```

- `faab.report` crawls (or loads) a corpus of real Sleeper waiver auctions and
  prints the measurements the bid model's backbone is built from. `--crawl`
  resumes, `--fresh` starts over, `--prior` also prints the backbone as EDN, and
  `--max-seasons=N`, `--max-users=N` and `--max-seasons-per-user=N` bound a crawl.
- `faab.interest` fits `faab/claim-weights`, the conditional logit that spreads a
  rival's claims over the free agents, to the claims real managers made, scored
  out of sample one league at a time. `--replay PATH` then runs the sweep's
  leagues with those weights and compares them, paired, against a saved run.
- `faab.sweep` scores every constant the bid model *chose* rather than measured,
  one at a time, against real auctions: who bids, the win chance, and the value
  bid. `--only NAME` limits it, `--set NAME=VALUE` tries a value, and `--league`
  picks leagues.
- `faab.replay` rebuilds each past waiver run of one league from what was known
  before it, prices it through the shipped code, and scores every prediction
  against the bids actually placed. `--league ID` picks the league (the default
  is the author's 2025 league) and `--league-prior` keeps the committed backbone
  instead of refitting it without that league.

### `lineup-report`

What changes if `:lineup-upgrade` replaces `:upgrade` as the waiver board's
headline. It needs a real Sleeper league, since with none connected everyone is
a free agent and the delta is undefined.

```bash
lein run -m draft-day.lineup-report --league-id 123 --roster-id 4
```

Omit `--roster-id` and it reports every team in turn, which is the honest way to
see whether an effect is real or an artifact of one roster's shape.

## Tests

`tools.trends`, `tools.projections` and `tools.snapshot` have tests under
`test/draft_day/tools/`, with the fetchers stubbed, so `lein test` never reaches
a vendor.
