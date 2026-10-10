# Draft Day

A fantasy football assistant for **auction** drafts, and for the season after
them. It turns your league's own scoring rules and roster slots into a live
dollar value for every player on the board, and re-prices the whole board after
every pick. Once the draft is over it syncs your real league and tells you who
is worth claiming on waivers and what share of your FAAB he is worth.

![The Draft Day board mid-auction: watch list, on-the-block tile and my roster
above a sortable player table showing Worth, Value, Market, Bargain, VORP and
injury risk for every player.](docs/img/board.png)

<sub>This screenshot is captured against the bundled offline universe, which is
why the status line reads `sample`. That is real data from
`resources/sample_players.edn`, not placeholder rows.</sub>

- [Why this exists](#why-this-exists)
- [See it work](#see-it-work)
- [Getting started](#getting-started)
- [Tools](#tools)
- [Documentation](#documentation)
- [Glossary](#glossary)

## Why this exists

Almost every draft tool is built for **snake** drafts, where a ranked list is
the whole answer: when your turn comes, take the highest name left. An auction
asks a different question. Every player is available to everyone, all the time,
and the only thing you ever decide is *what is he worth to me, right now, with
this much money left*. A ranking cannot answer that. A price can.

Three things follow from that, and they are what this app is.

**1. Prices have to come from your league.** Consensus auction values from ESPN
or FantasyPros are priced for a generic 12-team PPR league with a generic
roster. Change the reception weight to 0.5 and every receiver is worth less;
add a second flex and every running back is worth more. Draft Day derives its
dollars from *your* scoring weights and *your* roster slots, and shows the
consensus alongside, as the `Mkt` and `Edge` columns, rather than as the
answer.

**2. Prices have to move as the room drafts.** Money spent is money gone. When
the first fourteen players off the board go $114 over their model value, that
$114 came out of the room's remaining budget, so everyone still available just
got *cheaper*, and the board has to say so:

![Before and after: the board at nomination one, then the same board after the
room has overpaid on the first fourteen players. The market multiplier falls
from ×1.00 to ×0.91 and the inflation index reads $114.](docs/img/pricing.gif)

The header tells both halves of that story. **Infl Idx** is `$114`: the room is
overpaying. **Market** has fallen to `×0.91`, which is the *consequence*: with
the discretionary money drained, every remaining price comes down. A tool that
only showed you the overpay would have you believe the draft got more expensive;
what actually happened is that value is now on sale.

**3. The board has to be honest about what it does not know.** Every display
signal that is not a price is kept out of the price. Injury risk, tier cliffs
and the market consensus are shown as columns and feed nothing, because the
market price already carries the room's injury opinion, and discounting Worth by
risk would charge a fragile player twice.

**And the season is a different problem.** In November you are not splitting a
bankroll across a roster; you are buying one roster seat with a budget you spend
down over months. So the **Waivers** tab re-projects every player over the games
that are actually left, measures what a claim adds to *your starting lineup*,
and prices it against the FAAB you have left. **My Team**, **Matchup** and
**League** tabs sit beside it, fed by your real Sleeper or ESPN league.

## See it work

**The draft.** Put a player on the block, enter the winning price and the team,
and the whole board re-prices. Here the first pick goes for $88 against a $90
value and the second for $61 against $86, so the room is underspending and the
market multiplier in the header ticks up to ×1.01.

![Nominating Jahmyr Gibbs, bidding $88 and recording the pick, then
nominating Bijan Robinson at $61 for another team. Bankroll, max bid and the
market multiplier in the header update, and the board re-prices.](docs/img/draft-in-action.gif)

**The waivers.** In season, the Waivers tab ranks what is left. Filter to a
position, click two players to put them side by side, and open either one's
card for the week's projection, his game log and his news.

![The Waivers tab filtered to running backs, two players compared side by side
across this week, over replacement, trend, form and adds, and then one player's
card open on its game log.](docs/img/waivers-compare.gif)

<sub>The draft GIF is captured against the bundled offline universe, so its
status line reads `sample`. The waivers GIF is week 5 of the 2026 season from
live data, with no league connected, so it ranks everyone rather than who is
actually free.</sub>

## Getting started

**Prerequisites**: a JDK (17 or newer), [Leiningen](https://leiningen.org/) and
Node 18+. Tested with JDK 25, Leiningen 2.12 and Node 26.

```bash
git clone https://github.com/RockChalkJay/draft-day-clj.git
cd draft-day-clj
npm install

# macOS/Homebrew only: the JDK is keg-only and not on PATH. `lein` finds Java
# on its own, but shadow-cljs needs JAVA_HOME exported first.
export JAVA_HOME=/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home

npm run release                 # compile the SPA into resources/public/js
lein run                        # serve it, and the API, on http://localhost:8080
```

Open <http://localhost:8080>. The first run pulls a live player universe from
Sleeper, FantasyPros and ESPN, which takes a minute. To skip the network
entirely and boot off the bundled sample universe:

```bash
DRAFTDAY_OFFLINE=1 lein run
```

### Your first five minutes

1. **Look at the board.** Every player is priced under PPR with 12 teams and a
   $200 budget. `Worth` is the number to bid to.
2. **Change the rules.** In **Settings → Scoring**, switch the preset (or edit a
   weight) and watch the board re-rank and re-price.
3. **Run a mock draft.** Click **Start Draft**, click a player to put him on the
   block, enter the winning price and the team, and watch every other player's
   `Worth` move.
4. **Bring your league.** In **Settings → Leagues & Accounts**, connect a Sleeper
   username (no key needed) or an ESPN account (your own `SWID` and `espn_s2`
   cookies) and pick a league. Its real scoring and roster replace the defaults.
   [docs/using.md](docs/using.md) walks through the rest.

### Developing

Run two processes and use **one port**. `npm run watch` rebuilds `main.js` into
`resources/public/js` on every save; `lein run` serves that same directory *and*
the API on **:8080**. Point your browser at :8080 and let `watch` recompile
underneath it. (shadow-cljs also serves `resources/public` on :8280, but nothing
answers `/api/*` there, so the board cannot load players from it.)

```bash
lein test             # all Clojure tests (no network)
npm test              # the ClojureScript node-test build
lein test :integration  # hits live vendors; skips without DRAFTDAY_ESPN_* vars
```

See [docs/development.md](docs/development.md) for running a single test and
for replaying a finished season as one in progress.

### Configuration

All optional.

| Env var | Default | Effect |
| --- | --- | --- |
| `PORT` | `8080` | server port |
| `DRAFTDAY_OFFLINE` | unset | `1` forces the bundled sample universe, with no network calls |
| `DRAFTDAY_CACHE_TTL_HOURS` | `24` | how long the on-disk player universe stays fresh |
| `DRAFTDAY_WEEKLY_TTL_HOURS` | `1` | the same window for the weekly projection line, which moves faster |
| `DRAFTDAY_REALIZED_TTL_HOURS` | `1` | the window for what players have actually scored |
| `DRAFTDAY_TRENDING_TTL_HOURS` | `1` | the window for Sleeper's most-added list |
| `DRAFTDAY_INJURY_TTL_HOURS` | `3` | the window for injury designations |
| `DRAFTDAY_NEWS_TTL_MINUTES` | `15` | how long a player's news is reused |
| `DRAFTDAY_AS_OF_WEEK` | unset | dev only: truncate the in-season data at a week, so a finished season replays as one in progress |
| `DRAFTDAY_ESPN_SWID` / `_S2` / `_LEAGUE` / `_SEASON` | unset | `lein test :integration` only: check ESPN's stat ids, slot table and team abbreviations against a live league |

## Tools

`dev/` holds the command-line tools and research harnesses behind the app. They
are not shipped; they are how the model gets measured, and how data that
Sleeper will not give back later gets saved.

| Tool | Does |
| --- | --- |
| `tools.trends` | saves Sleeper's trending add and drop lists as timestamped JSON |
| `tools.projections` | saves the week's raw projection line, kickoffs and injury list, and reports gaps in what has been saved |
| `tools.snapshot` | regenerates the committed offline sample universe |
| `tools.refresh-player-ids` | regenerates the committed Sleeper-to-GSIS id crosswalk, refusing to change an id that already resolved |
| `benchmark.report` | scores ranking models against real historical draft outcomes |
| `replay.report` | replays real auctions and compares Worth with what rooms paid |
| `faab.*` | measure, fit and score the waiver-bid model on real Sleeper leagues |
| `lineup-report` | compares the lineup upgrade with the bench delta on a real league |

```bash
lein run -m draft-day.tools.trends
```

Full usage, flags, output locations and a cron schedule are in
**[dev/draft_day/tools/README.md](dev/draft_day/tools/README.md)**.

## Documentation

| Read | For |
| --- | --- |
| [docs/using.md](docs/using.md) | setting up a league, the board and waiver columns, every season tab |
| [docs/architecture.md](docs/architecture.md) | how the browser, the stateless server and the ingestion pipeline fit together |
| [docs/data.md](docs/data.md) | every data source, how it joins, the shape of a player, the cache |
| [docs/math.md](docs/math.md) | each pricing stage: formula, constants, and the key it writes |
| [docs/waivers.md](docs/waivers.md) | rest-of-season projection, lineup upgrade, walk-away and bid |
| [docs/predictions-and-math.md](docs/predictions-and-math.md) | what is predicted, what is measured, what is only chosen, and where it is weakest |
| [docs/scoring-coverage.md](docs/scoring-coverage.md) | where a league's real rules and what the board can score come apart |
| [docs/api.md](docs/api.md) | the JSON API |
| [docs/development.md](docs/development.md) | tests, and replaying a finished season as one in progress |
| [dev/draft_day/tools/README.md](dev/draft_day/tools/README.md) | the data-collection tools and research harnesses |
| [docs/TODO.md](docs/TODO.md) | the working list |

## Glossary

If you have never priced an auction board, these are the terms, roughly in the
order the code computes them. The math is in [docs/math.md](docs/math.md) and
[docs/waivers.md](docs/waivers.md).

| Term | Meaning |
| --- | --- |
| **Auction draft** | Every player is up for bid by every manager, and each manager has a fixed budget (here usually $200) to fill the roster. The opposite of a **snake draft**, where a fixed pick order takes turns choosing. |
| **Projection** | A vendor's guess at a player's raw stat line: so many rushing yards, so many receptions. Draft Day does not project; it uses Sleeper's. |
| **Points** | A projection scored by *your* league's rules: the sum of each projected stat times your weight for it. The only place your settings enter the math. |
| **Replacement level** | The score of the best player at a position you could get for free, because he is the first one who will not be started. In a 12-team league starting 2 RBs, the 25th back is roughly replacement level. |
| **VORP** (Value Over Replacement Player) | `points − replacement level` at his position. The one number that compares a quarterback to a tight end, because it has divided out how deep each position is. |
| **VBD** (Value Based Drafting) | The strategy VORP serves: buy the biggest VORP, not the biggest points total. |
| **Value** | VORP converted into dollars: what he is worth in a vacuum. Reserve $1 for every roster slot, then split the rest in proportion to VORP. |
| **Inflation** | The live correction. If the room has more money than board left, everything is about to get expensive; if it has overspent, the rest is on sale. |
| **Worth** | Value put through live inflation. **The number to bid to.** |
| **Bargain** | `Value − Worth`. Positive means the room is colder than the player's standalone value (a target); negative means you are reaching. |
| **Tiers** | Groups of interchangeable players, cut where scores fall off a cliff. With six players left in a tier you can wait to nominate; with one you cannot. |
| **ECR / ADP / AAV** | Other people's opinions, shown as columns: FantasyPros' Expert Consensus Rank, Sleeper's Average Draft Position, and Average Auction Value. |
| **Mkt / Edge** | `Mkt` blends the ESPN and FantasyPros auction values into your league's budget. `Edge` is `Worth − Mkt`: where the model disagrees with the room. |
| **Injury risk** | A 1-5 scale from games missed per season. It measures **availability, not injury**: a benching counts like a hamstring. |
| **Max bid** | The most you can spend on one player and still fill every remaining roster slot at $1. |
| **FAAB** | Free Agent Acquisition Budget: a season-long budget you bid from to claim players off the waiver wire. |
| **Waiver run** | The scheduled time claims are processed. The number left in the season is what a FAAB budget has to last across. |
| **ROS** (rest of season) | A player's projected points over the games still to play: the preseason projection, corrected by what he has actually done. |
| **Lineup upgrade** | The points a claim adds to your *starting lineup* after seating him and re-filling it. The headline number on the waiver board. |
| **Upgrade** | The cruder bench delta: his ROS points over the player you would drop, whether or not he would ever start. |
| **Walk-away** | What a player is worth to you in FAAB dollars: a share of your remaining budget, never more than you have or than the market pays. |
| **Bid** | A suggested FAAB bid: the amount that best trades off winning the claim against overpaying, given a model of your rivals. Currently priced by the server but not shown in the interface. |
| **Matchup / optimal lineup** | This week's head-to-head. The optimal lineup is the best one your roster could have set, by projection or by what actually happened. |

## License

MIT. See [LICENSE](LICENSE).
