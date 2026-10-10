# The math

Each stage, what it means, the formula as implemented, and the key it writes.
Constants are named where the code names them.

## Points — `scoring.cljc` → `:points`

`points = Σ(projected stat × weight)` over the 88 keys in `scoring/stat-keys`.
A named preset prices the 29 keys a format is about (the rest sit at 0.0, so an
import has a key to land every rule on) and the three differ only in the
reception weight: `:standard` 0.0, `:half-ppr` 0.5, `:ppr` 1.0. A custom or imported league carries a full `{stat-key weight}` map
instead of a preset keyword, and the vendor format to read is *derived* from
its reception weight rather than stored — `< 0.25` standard, `< 0.75` half,
else PPR.

Sleeper states a tier or a bonus *as a stat* — `pts_allow_7_13` arrives as 1.0
in the week it happened — so a flat weighted sum is not an approximation of how
a host scores but the thing itself, and it reproduces Sleeper's own per-player
points to the cent. What is left over is one rule that really does have another
shape: a position reception premium prices one stat by who earned it, which one
weight cannot hold. See
[scoring-coverage.md](scoring-coverage.md) for that, for the rules
nothing projects, and for what each one costs.

## Floor / ceiling — `projections.clj` → `:floor` `:ceiling`

Expert *disagreement* stands in for uncertainty:

```
band    = k_pos × min(1, rank_std / 10)
ceiling = points × (1 + band)
floor   = points × (1 − band)
```

`k_pos` is 0.20 for QB, 0.35 for RB and WR, 0.40 for TE, 0.15 K, 0.25 DST —
tight ends are the position the experts agree on least.

## Replacement level and VORP — `replacement.clj` → `:vorp`

```
idx_pos   = num_teams × starters_pos + flex_claims_pos
level_pos = points of the player at that index
vorp      = points − level_pos
```

**Flex claims are measured, not assumed.** Pool everyone left after each
position's dedicated starters, rank that pool on its own merits, take the best
`num_teams × flex_spots`, and count what positions they actually are. Splitting
the flex 50/50 RB/WR — the obvious guess — put PPR running-back replacement six
slots too deep, handing every RB about +14.6 phantom points and roughly +$9.
The measured claims are nothing like even: standard is RB 7 / WR 5, PPR is
WR 12 / RB 0.

Two deliberate choices. VORP is **signed**, not floored at zero — flooring
collapsed 549 of 633 sample players to 0.0 and destroyed the ordering of the
entire back half of the draft. And K/DST get `nil`, not `0.0`, because a 0.0
reads as *at replacement* and floated all 76 specialists above every
below-replacement skill player.

## Tiers — `tiers.clj` → `:tiers` `:tier`

Sort the pool, then greedily cut at the largest **absolute** gaps, taking a cut
only if both segments it creates stay at or above `MIN-TIER-SIZE` (2) — "a
one-player tier is not a tier, it is a rank with extra styling". Tiers are
**sized**, not counted: `tier_count = clamp(round(n / target), 2, 12)` with
`TARGET-TIER-SIZE` of 4 within a position and 12 overall. Everything at or
below replacement shares one final tail tier.

Absolute gaps rather than relative drops, because a relative drop is measured
against a falling number and so grows without bound as points decay — it
dragged nearly every cut into the tail and produced one 13-player top tier
above four 2-player tiers, exactly backwards.

Both scales are always computed: `:position` cuts on points within a position,
`:overall` cuts on VORP across the whole board. K and DST have no replacement
level, so they get a floor at the `num_teams`-th best kicker — otherwise
tiering spends every tier on 44 kickers.

## Value — `value.clj` → `:value`

```
discretionary = budget − total_roster_slots
value         = 1 + (vorp / Σvorp) × discretionary
```

Reserve the league minimum for every slot, then split what is left by VORP
share among the players who will actually be bought. The tail — everyone below
replacement, plus exactly as many kickers and defenses as the roster drafts —
is priced at `MIN-BID` of $1, apportioned **per position** by largest
remainder. Ranking that tail globally by VORP instead gave tight ends 27 of 96
minimum-bid slots against 12 TE starters.

Values sum to the budget within per-player rounding — about $2398 of $2400 on
the sample, since each player is rounded independently.

## Inflation — `inflation.clj`, `inflation_index.clj`

```
inflation = clamp( (remaining_cash − remaining_slots) / expected_premium,
                   0.5, 1.8 )
```

where `expected_premium` sums `max(0, value − 1)` over only the top
`remaining_slots` undrafted players, so a deep tail of $1 filler cannot dilute
it.

**Phase decay**: `1 − 0.2t²`, `t` = fraction of slots filled. 1.00 at the open,
0.80 when rosters are full — a multiplier means less with three picks left.

**Per position**: `global × (1 + 0.5 × (ratio_p − 1) × Σpar_p/(Σpar_p + 20))`,
where `ratio_p` is dollars paid over par at that position. The shrinkage term
is load-bearing: without it a $3 bid on a $1 flier reads as a 3× overpay and
pins the position at the top of the band on the first nomination, pricing a $40
back at $63.

The band is applied **once**, at the very end, to the finished
`position × decay` product. Clamping in the middle produced a no-pick position
priced *below* the global factor, and decaying after the clamp produced an
effective range outside both published bounds.

**Inflation Index** is a separate diagnostic, not a multiplier:
`Σ(price_paid − par_value)` over picks at priced positions. Rising means the
room is overpaying. K and DST are excluded because the board never prices them
— including them had a room paying par on all 180 picks still report +$24.

## Worth and Bargain — `value.clj`, `engine.clj` → `:worth` `:bargain`

```
worth   = 1 + (value − 1) × multiplier
bargain = value − worth
```

The `$1` base keeps every minimum-bid player at $1 at any inflation. Drafted
players get `0`.

## Injury risk — `injury.clj` → `:injury-risk`

Games missed per season over the last `min(3 fetched seasons, years_exp)`
seasons, banded at `[0.5, 1.5, 3.0, 5.0]` into 1-5, and floored at 5 by a
serious current designation (IR, PUP, NFI, suspension, NA, DNR — membership is by
*duration*, not severity, so Questionable and even Out are excluded on a
preseason board).

Two things are load-bearing. **The denominator is years in the league**, not
the width of the window: scored over a flat three seasons, the most fragile
players in football come out as last year's rookie class, who were not in the
league for two of them. And it measures **availability, not injury** — the
evidence is games played, so a benching counts like a hamstring, which is why
the column says "games missed" and never "injured". A player with no history to
judge is left blank rather than guessed at.

This is also deliberately *not* built on the weekly injury report: a player on
season-ending IR drops off that report entirely, so counting designations would
rate the worst injury of the season as iron-man durable.

## Signals that feed nothing

`:tcm` (tier-cliff multiplier), `:injury-risk`, and `:market`/`:edge` are
display columns and inputs to no price. Injury is the clearest case: market
prices already carry the room's injury opinion, so discounting Worth by risk
would charge a fragile player twice. The cautionary tale is the positional
demand multiplier, which was computed on every pick and consumed by nothing
until it was removed.

Valuation is hardwired to one weighting: there is no user-selectable strategy
profile. For where each constant comes from, what is measured and what is only
chosen, see [predictions-and-math.md](predictions-and-math.md).
