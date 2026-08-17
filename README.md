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

- **Add stats and player pic to the on the block card** Include additional stats like target percentage, number of targets, number of receptions.

- ~~Drag-and-drop column bugs found reviewing #12: droppable `text/plain` payload,
  picker drag dead in Firefox, missing `preventDefault`, insertion line flicker~~

## Known scoring coverage gaps

Draft Day scores a flat stat line — `Σ(projected stat × weight)` over the 21 keys in
`draft-day.scoring/stat-keys`. Rules with any other shape (FG distance buckets, tiered
points-allowed, yardage bonuses) have nowhere to land. A Sleeper import therefore lists
exactly what it could not apply (one real league: 54 of its 74 non-zero rules), because a
config that looks complete but scores differently is worse than one that admits its gaps.

The gaps were measured 2026-08-15 against the live 2026 Sleeper payload, scoring each
variant against the half-PPR preset and comparing board order. They cost wildly different
amounts, so they are listed by what they actually cost rather than by why they exist.

**Standard / half-PPR / PPR leagues lose nothing at skill positions.** No dropped key
carries a non-zero weight in any preset. If you play a vanilla format, only the kicker
issue below applies to you.

### PPFD reorders the board — the one defect that changes who you draft

`ingestion/sleeper.clj`'s stat-key list drops first downs that Sleeper does project:
`rec_fd` (474 players), `rush_fd` (376), `pass_fd` (77). Awarding 0.5 per receiving and
rushing first down moves **40 of the top 200 by ≥10 slots** (mean absolute move 6.4), and it
reorders *within* position — volume backs up (Jonathan Taylor +21, McCaffrey +19, Cook and
Henry +17), low-volume QBs down. A within-position shuffle survives the VORP transform
instead of cancelling out, so a PPFD league drafting off this board drafts the wrong
players. This is the gap worth closing.

### TE premium misprices rather than misorders

Position reception premiums are dropped too: `bonus_rec_te` (126 players), `bonus_rec_wr`
(210), `bonus_rec_rb` (138). Adding `bonus_rec_te` at 0.5 looks alarming on a global points
list — 77 of the top 200 move ≥10 slots — but restricted to tight ends the order barely
budges: **max move 2 slots, mean 0.4**. Every starting TE gains ~12 points in step. Since
replacement-level TE rises with them, VORP absorbs part of even that. The board is right;
the auction dollars at one position come out low.

### Kickers are wrong in every league, including the presets

Sleeper emits `fgm` on **0 of 44 kickers**. It publishes only distance buckets (`fgm_40_49`
on all 44, `fgm_50p` on 40, `fgm_yds` on 44, plus `fgmiss_40_49`, `fgmiss_50p`, `xpmiss`),
and `fgm` is the key the presets price at 3.0 — so that weight multiplies nothing and every
kicker is scored on extra points alone:

| Kicker | Scored 3/4/5 by distance | What Draft Day scores |
| --- | --- | --- |
| Brandon Aubrey | 118.0 | 42.0 |
| Cam Little | 114.0 | 42.0 |
| Ka'imi Fairbairn | 114.0 | 39.0 |

The position compresses into a 39–42 band with no real spread. Harmless in practice — K is a
$1 nomination and the understatement is near-uniform, so ordering is noise either way — but
it is the only entry here that bites a league running nothing but a stock preset, and the
only one no import warning covers, since `fgm` sits in `stat-keys` and so looks supported.

### Extractable but not yet extracted, low stakes

Also dropped and unmeasured, all cheap to add alongside the above: `pass_int_td`, `pr_td`,
`def_kr_td`, `pts_allow_0`, `yds_allow_0_100`.

### Genuinely absent — no fix without another source

Most yardage and long-play bonuses (`bonus_rush_yd_100`, `rec_40p`, `pass_td_50p`, …), the
short FG buckets (`fgm_0_19`…`fgm_30_39`), and all but the first `pts_allow_*` /
`yds_allow_*` tier. No weight can act on a stat nobody projects.

Three modelled weights are inert for the same reason: `ff`, `def_td` and `safe` never appear
in a projection, so team defenses score on sacks/interceptions/fumble recoveries alone. The
custom scoring editor shows them (with `fgm`) as "not projected" via `db/unprojected-stats`
rather than offering an editable box that cannot move any player's points. `fgm` is the odd
one out in that set — it is inert today but *is* recoverable from the distance buckets.

### Closing it

The cheapest fix with real return is three keys — `rec_fd`, `rush_fd`, `bonus_rec_te` —
added to `ingestion/sleeper.clj`'s stat-key list, the preset table in
`src/cljc/draft_day/scoring.cljc` (at 0.0, so preset behaviour stays byte-identical), and
`db/scoring-catalog`, which have to move together. Kickers need more than a new key: the
buckets must be summed into a synthetic `fgm`, or the buckets priced individually.
