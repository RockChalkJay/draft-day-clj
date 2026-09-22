# Scoring coverage

Where a league's real rules and what Draft Day can score come apart, and what
is left of the gap. Linked from the [README](../README.md#the-math).

## The gap was vocabulary, not shape

This document used to say that Draft Day scores a flat stat line, so rules of
any other shape — field-goal distance buckets, tiered points allowed, yardage
bonuses — had nowhere to land. That was wrong, and it shaped the design for a
while, so it is worth stating plainly.

Sleeper states a tier or a bonus **as a stat**. `pts_allow_7_13` arrives as
`1.0` in a week a defense held its opponent to ten; `bonus_rec_yd_100` arrives
as `1.0` in a week a receiver went over a hundred; `rec_40p` arrives as the
count of catches over forty yards. So `Σ(stat × weight)` is not an
approximation of how Sleeper scores — it is how Sleeper scores.

Measured against the live 2026 week 2 line for one real league, scoring
Sleeper's own stats under all 85 of the league's rules reproduces Sleeper's own
`players_points` for **all 166 rostered players to the cent**. Over the 29 keys
the vocabulary held before, 48 of those players were wrong, by up to 14.5
points. `draft-day.scoring-golden-test` is a committed, offline fixture that
pins both halves of that: the wide vocabulary agrees, and the narrow one does
not.

The vocabulary is now 88 keys (`draft-day.scoring/stat-keys`), which is every
rule Sleeper exposes in a league's scoring settings. The league above loses
none of them.

## What is still genuinely unsupported

One shape, and it really is a shape rather than a missing key: a **position
reception premium** — `bonus_rec_te`, `bonus_rec_wr`, `bonus_rec_rb` — prices
one stat differently depending on who caught it. One weight per stat cannot
express that, and a key would not help. ESPN writes the same rule as a
per-position override on an ordinary receptions rule, and
`league-import.espn/dropped-overrides` reports it for the same reason.

ESPN's points-allowed bands are the other open case, and also not a missing
key: ESPN splits 14-17 / 18-21 / 22-27 where Sleeper splits 14-20 / 21-27 /
28-34. Those are different partitions of the same space, so nothing maps
without a band neither provider's stat line publishes. ESPN's 0, 1-6 and 7-13
bands do line up and could be mapped; that has not been done yet.

## Modelled but unprojected

38 of the 88 keys are ones no Sleeper projection horizon carries
(`scoring/unprojected-stats`). A league that scores them is scored on them
*once the games are played* — they reach the rest-of-season board, the waiver
board's form column and the player detail modal — and they cannot reach the
draft board, because nothing projects them. Settings says this, separately from
the rules that are not modelled at all, because the two are different facts.

The families involved are the yardage-threshold bonuses, the long-touchdown
counts, the outer points- and yards-allowed bands, and most special-teams
scoring.

## The two projection horizons

Sleeper publishes a season line and a weekly line from the same house, and the
season line is much the sparser: no tier bucket, no long touchdown, no
completion over forty, no forced fumble, no safety. The buckets it *does*
publish are receiving ones.

That asymmetry is why the season line is completed from the week-one line
(`ingestion.sleeper/complete-season-line`), scaled by each player's own implied
games. Without it, widening the vocabulary is a tilt rather than a gain:
receivers and tight ends gain 9.1 and 7.4 points a season while quarterbacks,
kickers and defenses gain exactly nothing, and replacement and VORP compare
those positions directly. Filled, a starting quarterback gains about 8 points a
season and a starting defense about 72.

Three traps live here, each found by measurement:

**The season line's defense is not a season.** It reports
`yds_allow_0_100: 1.0` for all 32 defenses beside a `gp` of 1.0, while `sack`
and `int` on the same line are real season totals. It is the modal bucket of
one game. At a ten-point weight it would have handed every defense the same
spurious +10, so `ingestion.sleeper/season-only-noise` refuses it.

**The two horizons disagree about made field goals** rather than covering
different parts of the grid. Aubrey's season line carries nine makes from 40-49
and eight from 50+ and nothing under forty; his weekly line carries every band
under fifty and no 50+ at all. Filling one from the other read him as 31 makes
against the 17 his season line states, and scored him 160 against Sleeper's own
116 — where the sparse line scores 118. Made kicks are exempt from the fill;
`summed-fgm` and `scoring/fg-buckets` already reconcile that grid.

**The tier buckets are one-hot.** Every defense carries exactly one
`pts_allow_*` and one `yds_allow_*` at 1.0 — Sleeper's modal week, not a
distribution — and across all 32 only the 14-20, 21-27 and 28-34 points bands
ever appear. Stretched over a season that asserts a defense lands in one bucket
every week, which none does, so a league paying well for a shutout collects
nothing for it in a projection. Taken anyway, because the alternative is a
defense projected on sacks and interceptions alone. The real-valued `pts_allow`
Sleeper sends beside the one-hot is what a distribution would be built from if
`dev/draft_day/benchmark/` ever earned one.

## Realized production: two sources, one preferred

`ingestion.sleeper-actual` reads Sleeper's own account of what has happened and
is preferred over nflverse's, per player and never mixed. Three measured
reasons:

- nflverse publishes **no team defense row at all**, so a defense had no
  realized production to blend and the tier rules had no realized side to land
  on.
- Against Sleeper's own 2026 weeks 1-3, nflverse's first downs read **higher**
  on a quarter to three quarters of the player-weeks either side reports, and it
  files a **blocked field goal** under `fg_blocked` where Sleeper counts it a
  miss (`fg_missed + fg_blocked` matched `fgmiss` on both cases in the sample).
- It publishes **no count of a touchdown's length**, so the long-touchdown
  rules have nowhere to come from.

Those are two vocabularies rather than anyone's error, but only one of them is
the one the standings are kept in. `nflverse-weekly/refused-columns` records the
columns whose names invite a mapping their meaning does not support; nflverse
keeps the usage columns and the three-season history, which it is better at.

Presence is not an appearance: Sleeper answers a week with an entry for
everyone who dressed and credits `gp` only where it counts a game. 271 of 728
entries on the live weeks 1-2 carry no `gp`, so counting them inflated games
played by 37% — and games played is the denominator the rest-of-season blend
divides by.

## Standard, half-PPR and PPR lose nothing

No key added to the vocabulary is priced by any preset, so a league playing a
vanilla format scores exactly as it did. `only-the-reception-weight-separates-the-presets`
and `every-preset-weight-is-applied-as-written` pin that.
