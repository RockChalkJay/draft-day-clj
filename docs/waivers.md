# The waiver wire

The draft board and the waiver board read the same universe and share almost no
numbers, because they answer different questions. On draft night you divide one
bankroll among a whole roster at once. In November you are buying a single
roster seat with a budget you spend down over months — so Worth, Value, Market
and Bargain are all absent from the Waivers tab, and nothing there is priced in
dollars the auction invented.

Connect a Sleeper or ESPN league and the board becomes a *waiver wire*
rather than a ranking: everyone on somebody's roster drops out, and what is left
is what you can actually claim. Without a league it still ranks every player by
rest-of-season value, and says out loud that it is doing so.

## Rest-of-season projection — `ros.clj` → `:ros-points`

The universe carries **preseason, full-season** projections. In September that
is the best evidence there is; in November it is a claim about a season that has
half happened, made by someone who has not looked. So each stat is blended, per
game, with what the player has actually done:

```
pre-pg = preseason-season-total / season-games
ros-pg = (PRIOR-GAMES · pre-pg + realized-total) / (PRIOR-GAMES + played)
ros    = ros-pg · games-remaining
```

Writing the realized side as a total rather than as (games × per-game) cancels
the division, and that is what makes the expression total: **at zero games
played it collapses to the prorated preseason line**, with no special case. That
is the right answer for a rookie who has not debuted, for every team defense
(nflverse publishes no DST row at all), and for the whole board in August.

`PRIOR-GAMES` is how many games of evidence the projection is worth — six, which
keeps one loud week from reordering the board while still letting a September
role change win by December. It is *chosen, not measured*; `dev/` is where it
would earn a number.

A player with **no preseason line at all** — the undrafted rookie who is now the
lead back, the player this whole tab exists for — falls out of the same
expression with a prior of zero: his production is spread over `PRIOR-GAMES +
played` rather than over `played`, so he is deliberately discounted early and
climbs as the games accumulate. One 140-yard game does not make him a starter;
five of them do.

Two things are easy to get wrong and are not. `:games` counts the player's own
weekly rows, never weeks elapsed — dividing by weeks charges an injured player
for the absence twice, once in the total and again in a depressed rate. And
weeks left are not games left: a team plays 17 games across 18 weeks, so the bye
comes out of the count while it is still ahead.

Realized production comes from Sleeper's own weekly stats where it has them and
from nflverse otherwise, one source per player and never mixed. Sleeper's score
reproduces a league's totals exactly, it has a row for every team defense, and
nflverse's first-down count differs from it in about a quarter of player-weeks.

A player with a serious injury designation (`db/serious-injury?`: IR, PUP, NFI,
suspension and the like) has zero games remaining once a week has been played,
so his `:ros-points` is 0 until Sleeper clears the tag. Sleeper gives no return
dates, so this is deliberately blunt, and a preseason designation is ignored.

`:through-week` is read off the newest week nflverse has published, not off the
calendar. It travels with the in-season columns from the same fetch, so they
cannot disagree: a missing weekly file reads as week 0 *and* as no realized
production anywhere, and the board degrades to the preseason board rather than
to a November league priced on an August projection.

## Upgrade, walk-away and bid — `waiver.clj`, `faab.clj`

**`:lineup-upgrade`** is the headline, and what the board sorts by. It is what
the claim adds to your *starting lineup*: seat the player, re-fill the lineup
under the league's own seats, and take the difference. That is the question a
manager is actually asking, and it is why it leads — a bench delta can be large
for a player who would never start a week. It is *absent* rather than 0 when the
league's seats are unknown, so "no lineup to compute against" cannot be misread
as "adds nothing".

**`:upgrade`** is the bench delta beneath it. A claim costs a *roster spot*, not
a positional slot, so the comparison is against your worst player — not your
worst player at his position. Players parked on IR or taxi are excluded from that
count in both directions: they fill no active seat, so they must not make a
roster look full, and dropping one frees no seat for the claim being priced.
Which player that is depends on what the league told us: knowing the seats,
`drop-candidate` picks by *marginal starting-lineup cost* — who you can lose
most cheaply — and falls back to plain worst-points only when the seats are
unknown. With a spot already open you give up nothing and the
upgrade is his whole rest-of-season line. It stays signed: most of a free-agent
pool is worse than the man you would drop, and flattening that to zero would
make the entire tail look equally plausible.

**`:walk-away`** is what the player is worth to *you*, in FAAB dollars: a share
of your remaining budget, not an auction price. A free agent's gain is measured
against the best *other* free agent at his position, since free agents at a
position are substitutes. The best `claims-left` gains split the budget, where
`claims-left` is how many waiver runs the season has left (read off the
calendar rather than chosen, so many runs left means small amounts and one run
left means spend it). The budget is two pools rather than one, so bench depth
cannot outbid a starter: 85% for players who would start and `stash-share`, 15%,
for the rest. The result is capped at what you have left and at what the Sleeper
market has been seen to pay for that position (`faab/market-cap`).

**`:bid`** is a prediction of the rivals rather than a second share of the
budget: the bid that maximizes the chance of winning times the surplus over the
walk-away. The rival model is a conditional logit on real waiver claims plus a
Poisson count of claims a week, with each rival's habits drawn from their own
bid history where the league has one (`rankings/faab.clj`). Its companion keys
are `:bid-sure` (the cheapest bid that wins 90% of the time), `:win-prob`,
`:rivals` (the expected number of bidders) and `:competition` (the median and
90th-percentile top rival bid, or nobody). The model, what it is fit on and
where it is weakest are in [predictions-and-math.md](predictions-and-math.md).

**The bid model is on hold in the interface.** `/api/waivers` prices every
claim, but `db/bid-predictions?` is false, so the Bid, Sugg. and Rivals columns
are not offered. The walk-away and the upgrades, which are plain arithmetic on
points, are what the board shows.

A `$0` bid is a **real bid**, not a refusal: FAAB accepts one, and a player
whose gain rounds to nothing is honestly worth the minimum. That is the
opposite of the auction board, where `$0` meant undraftable and the `$1` floor
existed to say so. A *blank* bid is the third answer: this league does not run
FAAB, or you have nothing left to spend.

**`:rival-max`** is not a formula at all. It is the largest budget anyone else
still holds, a fact rather than an opinion. It rides on the response's `:faab`
object; no screen draws it today.

Replacement level is computed over the **whole league**, never over the free
agents: scoped to whoever happens to be unclaimed it would drift down every time
a good player was added, and the remaining scraps would start reading as
starters. `:trend` (recent opportunity per game against the season rate) is a
display column on the same shelf as `:injury-risk` — it feeds nothing.
