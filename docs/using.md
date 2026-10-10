# Using it

Draft Day is two apps in one window. The **draft** half is the auction room: a
board that prices every player and re-prices after each pick. The **season**
half takes over once the draft is done: your roster, your matchup, the waiver
wire and the rest of your league. Which half you are in follows the active
league (`db/phase`), and **Settings** is reachable from both.

## Set up the league

In **Settings → Leagues & Accounts**, connect a Sleeper or ESPN account and pick
one of its leagues to pull that league's real scoring and roster settings. You
can also paste a league ID for a league the account's listing did not show
(ESPN's listing endpoint is undocumented, so pasting is the fallback there), or,
for a league that is not connected, set everything by hand.

ESPN needs your own session cookies (`SWID` and `espn_s2`), which is why the
server proxies the call and the browser never talks to a provider directly.
Credentials are stored in your browser, on the account they belong to, and are
sent only with requests that need the host.

A connected league's scoring, roster, team count and (once an import supplies
one) auction budget come from the host and are read-only; **Re-sync** re-imports
them alongside the rosters. An import reports exactly which of your league's
rules it could not apply, and separately which it applies but nobody projects.
See [scoring-coverage.md](scoring-coverage.md): a config that looks complete but
scores differently is worse than one that admits its gaps.

Settings shows one section at a time (Leagues & Accounts, Scoring, Roster &
League, Draft, Data), with a badge on a section that has something waiting.

![The Settings view: league import, league size and budget, a per-position
budget plan, scoring preset, roster slot counts, and a danger zone with a
player-cache reset.](img/settings.png)

You can play in more than one league, across more than one host. The header's
league switcher changes which one the whole app is about; the board, the
waivers and the rosters all follow it.

## Draft half

**Run the draft.** Click **Start Draft**, then click a player to put him **on
the block**, then enter the winning price and the team that got him. Every pick
re-prices the whole board. The header tracks your bankroll and your **Max Bid**,
the most you can spend and still fill every remaining slot at a dollar. The
**League** tab on this half is the auction room: every team's picks and
bankroll.

**Read the board.** The columns that are not self-evident (all of them are
toggled and dragged from the column picker, and the layout is yours to keep):

| Column | Means |
| --- | --- |
| `Worth` | **the number to bid to**: Value scaled by live inflation |
| `Value` | the stable, inflation-free dollar value |
| `Barg` | `Value − Worth`; green is a target, red is a reach |
| `VORP` | points above replacement at his position |
| `Mkt` | ESPN + FantasyPros consensus price, rescaled to your budget (`ESPN` and `FP$` show the unscaled prices) |
| `ECR` | FantasyPros' expert consensus rank |
| `Risk` | 1-5 durability, from games missed per season |

Off by default, one click away in the picker: `Edge` (`Worth − Mkt`; green means
the model likes him more than the room), `Tier`, `FP T` (FantasyPros' own tier),
`ADP`, `Inj`, `Proj`, `Ceil`, `Floor`, and five usage columns (`Tgt`, `Rec`,
`Tgt%`, `pTgt`, `pRec`) that show only for receiving positions.

**Filter to a position and the tier scale changes with it.** Unfiltered, `Tier`
is an *overall* tier cut on VORP, the only score that compares a QB to a WR.
Filter to RB and it becomes RB's own tier, cut on points within the position,
because "tier 2 RB" and "tier 2 WR" mean nothing next to each other. The board
switches with no round trip, since the server ships both scales on every player.

![The board filtered to running backs, with tier striping on the positional
scale and the RB filter button active.](img/board-rb.png)

## Season half

The season half has four tabs. The app opens here on its own once the league's
host reports the draft as done, or a week of games has been played.

**My Team.** Your synced roster, with starters, bench and IR/taxi, each player's
bye, and the seat a claim would cost you marked as the drop candidate. Its side
cards (This week, Lineup check, Best claims) answer from boards already fetched
and link to the tab that says more.

**Matchup.** This week's head-to-head, one row per lineup seat with the slot
label down the middle. Projected points are muted and actual points are bold; a
dash is *not* a zero, it means the game has not kicked off. A toggle draws the
best lineup you could have set in place of the one you did: a player it moves in
is marked ▲, a starter it benches ▼. The best lineup by actual points appears
only once every game is final. Both Sleeper and ESPN leagues have a matchup
board.

**Waivers.** The waiver wire, described below.

**League.** Every synced team in the league: starters, bench, record, FAAB left,
and a tag for each manager's bidding style where the host has history.

Click any player to open the **player card**: a head in his team's colours, tiles
for this week's projection, average points and adds, a line for his injury
designation or newest news, and tabs for the game log, news (fetched when the
card opens) and the season table.

### Work the waiver wire

Once the season starts, the **Waivers** tab syncs your league's real rosters and
ranks what is left. Its columns answer a different question from the draft
board's, so it shares none of them. The board opens sorted by `Adds`, with the
players off Sleeper's trending list first, and `#` always keeps its own ranking.

| Column | Means |
| --- | --- |
| `Pos` | position, and his rank at it on fantasy points so far (the preseason rank before week 1) |
| `Wk` | projected points for this week's game; blank on a bye or when he is not projected |
| `Pts`, `Avg`, `GP` | fantasy points so far this season, per game, and games played |
| `Adds` | times he was added in the last 48 hours across Sleeper leagues (from Sleeper's trending list) |
| `Risk` | injury risk, 1 (durable) to 5 (fragile), from games missed per season |
| `Inj` | current injury designation |

Off by default, switched on from the column picker: `Wk#` (his positional rank
on this week's projection), `Opp`, `Lineup` (rest-of-season points he adds to
your *starting lineup*, after seating him and re-filling it), `Upg` (the gain
over the player you would drop, whether or not he would start), `Trend` (recent
opportunity per game against his season rate; above `1.00×` the role is
growing), `Form` (points per game over the last three weeks), `VORP`, and the
season counting stats (`Tgt`, `Car`, `Rec`, `Yds`, `TD`).

The bid model is **on hold in the interface** (`db/bid-predictions?` is false).
`/api/waivers` still prices every bid, but the typical bid (`Bid`), suggested bid
(`Sugg.`) and `Rivals` columns are not offered. In a synced league that does not
run FAAB they would be hidden in any case. A stored layout keeps those columns
for the day the flag flips. See [waivers.md](waivers.md) for how a claim is
valued and [predictions-and-math.md](predictions-and-math.md) for the bid model.
