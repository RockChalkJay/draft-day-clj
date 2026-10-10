# The data

## Sources

| Source | Endpoint | Contributes | Joined on |
| --- | --- | --- | --- |
| **Sleeper** | `api.sleeper.app/projections/nfl/{season}` | the player universe itself: name, position, team, the projected stat line, ADP, injury status, years of experience | — (it *is* the universe) |
| **Sleeper** | `api.sleeper.app/schedule/nfl/regular/{season}` | bye weeks, derived as the one missing week per team | team |
| **FantasyPros** | `/nfl/rankings/…cheatsheets.php` | ECR, positional rank, expert tier, and the rank spread that becomes floor/ceiling | name + position |
| **FantasyPros** | `draftwizard.fantasypros.com/auction/…` | AAV — consensus auction value | name + position |
| **ESPN** | `lm-api-reads.fantasy.espn.com/…/players` | live auction value, ADP, projected targets and receptions | name + position |
| **nflverse** | `nflverse-data` release CSVs | last season's realized usage, and games played over three seasons for the durability scale | **GSIS id — exact** |
| **nflverse** | `stats_player_week_{season}.csv` | this season week by week: what each player has produced, his recent opportunity, and how far the season has got | **GSIS id — exact** |
| **Sleeper** | `api.sleeper.app/projections/nfl/{season}/{week}` | this week's projected line, which is what `Wk` shows | Sleeper id |
| **Sleeper** | `api.sleeper.app/stats/nfl/{season}/{week}` | what each player actually scored, week by week. Preferred over nflverse for realized production where it exists | Sleeper id |
| **Sleeper** | `api.sleeper.app/stats/nfl/{season}?position[]=DEF` | season totals for team defenses, which nflverse has no row for | team abbreviation |
| **Sleeper** | `api.sleeper.app/v1/players/nfl` | the injury designation, body part, notes and update time. Replaces the projections feed's stale `injury_status`; a player the list omits is healthy | Sleeper id |
| **Sleeper** | `api.sleeper.app/v1/players/nfl/trending/{add,drop}` | the most-added and most-dropped players in the last 48 hours (the `Adds` column). Every live fetch is also saved as JSON under `data/trends/` | Sleeper id |
| **ESPN** | `site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard` | kickoff times: this week's opponent, and which games have started, so the matchup can tell a player who scored nothing from one who has not played yet | team |
| **ESPN** | `site.api.espn.com/apis/fantasy/v2/games/ffl/news/players` | the player card's news tab: Rotowire items, newest ten. Fetched when a card opens, never with the universe | ESPN id |
| **Sleeper / ESPN** | each host's league, roster, matchup and transaction endpoints (see [api.md](api.md)) | who is rostered right now, FAAB left, waiver position, this week's matchups, and the league's FAAB bid history (Sleeper only) | fetched per league, not per universe |
| **DynastyProcess** | `db_playerids.csv` | the id crosswalk tying those together. Pinned as a snapshot at `resources/player_ids.edn`, not fetched at runtime | — |

Sleeper defines the rows; everything else is a **best-effort left join**. A
source that fails leaves its column empty and the board still renders — it just
says so in the provenance report. Unmatched enrichment rows are dropped, never
added.

Name matching (`match.cljc`) lowercases, strips generational suffixes
(`jr`/`sr`/`ii`/`iii`/`iv`/`v`) and all non-alphanumerics, then concatenates
with position: `"T.J. Hockenson", "TE"` → `tjhockenson_te`. nflverse is the one
source that joins exactly, on the GSIS id every universe player already carries.

The league endpoints are the exception to all of this: they describe *your*
league rather than the shared universe, so they are fetched per request and
never cached alongside it. (A league's FAAB bid history is cached on disk per
league and season, and never sent to the browser.) The weekly file is genuinely
absent before week 1 — it 404s until the season opens — which the pipeline
reports as an unavailable source, and which the rest-of-season projection reads
as week 0.

The in-season sources have their own cadence, because they move faster than the
universe: the weekly line and the realized weeks refresh hourly, trending adds
hourly, injuries every three hours and ESPN news every fifteen minutes. Each has
its own TTL variable (see [Getting started](../README.md#getting-started)).

## Shape of a player

```clojure
{:player-id "00-0038563"          ; GSIS id where resolvable, else Sleeper id
 :ids {:sleeper "10213" :gsis "00-0038563" :fantasypros "25337"
       :espn "4428718" :pfr "TuckTr00"}
 :player-name "Tre Tucker" :position "WR" :team "LV" :bye 13

 ;; the projected stat line — the only input to :points
 :stats {:rec 3.0 :rec_yd 32.0}

 :sleeper/years-exp 3
 :sleeper/injury-status nil

 ;; ESPN is deliberately NOT format-scoped: it publishes the same auction
 ;; value under both its PPR and STANDARD rank types.
 :espn/adp 168.17
 :espn/auction-value 0.24
 :espn/proj-receptions 48.41
 :espn/proj-targets 81.62

 ;; nflverse. :games-by-season is what he played; :games-seasons is how long
 ;; each season was — a consumer has to tell a season the player missed from
 ;; a season the network missed.
 :nflverse/prior-season 2025
 :nflverse/prior-games 17.0
 :nflverse/prior-targets 92.0
 :nflverse/prior-receptions 57.0
 :nflverse/prior-target-share 0.013
 :nflverse/games-by-season {2023 9.0 2024 15.0 2025 6.0}
 :nflverse/games-seasons   {2023 17  2024 17  2025 17}
 :nflverse/history         {…}   ; per-season stat lines behind the player card's Season table

 ;; columns vendors publish PER SCORING FORMAT, kept side by side
 :vendor/by-format
 {:ppr {:fantasypros/ecr 157 :fantasypros/pos-rank "WR60"
        :fantasypros/ecr-tier 9 :fantasypros/ecr-pos-tier 7
        :fantasypros/rank-ave 168.08 :fantasypros/rank-std 28.09
        :fantasypros/rank-min 116 :fantasypros/rank-max 291
        :fantasypros/aav 1.0 :sleeper/adp 203.7}
  :half-ppr {…}
  :standard {…}}}
```

**Why `:vendor/by-format` exists.** The universe cache is shared across
leagues, so which scoring format a vendor column should be read at cannot be
decided at ingestion time. Every format-scoped column is fetched for all three
formats and stored side by side; `rankings/vendor.clj` flattens the matching
one onto flat keys per request. Baking in PPR is what once made a standard
league read PPR tiers, PPR market prices and PPR ADP while reporting a full
row count.

## Cache and provenance

The universe is cached to `data/players_cache.v16.transit`. The schema version
rides in both the filename and the payload, so bumping it orphans the old file
rather than silently reusing an incompatible one. Freshness is file mtime
against `DRAFTDAY_CACHE_TTL_HOURS`. `POST /api/cache/reset` drops it, along with
the realized-weeks cache.

The in-season data lives in separate, separately versioned files under `data/`
(the weekly line, realized weeks, injuries, trending adds and each league's bid
history), each on its own TTL, so a stale universe never holds back a fresh
week. Everything under `data/` is a local cache and is gitignored.

Every join reports itself, and `GET /api/players` returns the report under
`:universe`:

```clojure
{:rows 148 :matched 147 :hit-rate 0.9932 :coverage 0.2318
 :by-position {"WR" {:n 216 :rows 47 :matched 47} …}
 :unmatched-sample ["ajdillon_rb"]
 :ok? true :expected-partial? false}
```

A hit rate under 0.80 raises a per-position warning unless the source is
declared partial. This is the difference between "ESPN prices are missing" and
"ESPN prices are missing *for tight ends*", which is the one you need during a
draft.
