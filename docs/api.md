# API

Kebab-case on the wire — JSON keys decode straight to keywords, with no
camelCase conversion.

| Method | Path | Does |
| --- | --- | --- |
| `GET` | `/api/health` | liveness |
| `GET` | `/api/players` | the cached universe + its provenance. `?refresh=true` forces a reload |
| `GET` | `/api/players/:id/news` | a player's newest fantasy news, fetched when his card opens |
| `POST` | `/api/rankings` | the fully valued board |
| `POST` | `/api/account/connect` | an account's identity and the leagues it plays in |
| `POST` | `/api/league/import` | proxy a league's real settings from its host |
| `POST` | `/api/league/sync` | who is rostered right now, what is left to bid, and the league's bid history |
| `POST` | `/api/waivers` | the in-season board: free agents, upgrades and bids |
| `POST` | `/api/matchup` | this week's head-to-head, every roster valued, fetched live |
| `POST` | `/api/cache/reset` | drop the on-disk universe and realized-weeks caches |

## `POST /api/rankings`

```clojure
;; request
{:num-teams          12
 :scoring            :ppr            ; preset keyword, or a {stat-key weight} map
 :replacement-config {:qb 1 :rb 2 :wr 2 :te 1 :flex 1}
 :league-state       {:teams [{:team-id "t0" :bankroll 200.0
                               :roster [{:pos "RB" :player-id nil} …]} …]
                      :drafted-player-ids ["00-0038563" …]
                      :starting-bankroll  200.0
                      :picks [{:player-id … :position "RB" :price 47} …]}}

;; response
{:inflation       0.92    ; global conserving factor, banded
 :inflation-index 114.0   ; Σ(paid − par) — rising means the room is overpaying
 :market-heat     0.99    ; phase decay
 :players         [ … ]}  ; every player, every key from every stage
```

Two guards return `400` before any work happens: a scoring config with no
non-zero weight on a projected stat (an all-zero board is a lie, not a board),
and a bankroll that cannot cover $1 per roster slot.

## `POST /api/account/connect`

Takes `{:provider :credentials :season}` and returns the account and the
leagues it plays in:

```clojure
{:user    {:user-id "u1" :display-name "rockchalkjay" :avatar "…"}
 :leagues [{:league-id "…" :name "…" :season "2026" :num-teams 12 :status "in_season"}]
 :leagues-error "…"}     ; present only when the host could not list them
```

`:credentials` is whatever `draft-day.providers` says identifies a manager to
that host — a username on Sleeper, a `SWID` and an `espn_s2` on ESPN. A POST
rather than a GET because a session cookie in a query string reaches browser
history, proxy logs and `Referer` headers, and there is no default provider:
defaulting one meant a typo'd ESPN connect was looked up on Sleeper and came
back "user not found".

`:leagues-error` is reported *beside* an empty list rather than as a failure,
because "plays in no leagues" and "the listing broke" are different facts and
a manager needs opposite things from them. ESPN's listing endpoint is
undocumented, so its failure is expected and falls back to pasting a league ID.

## `POST /api/league/import`

Takes `{:provider :sleeper :league-id "…" :season "…" :credentials {…}}` and
returns the league's scoring and roster settings, plus `:unsupported-scoring` —
the rules a flat stat-line model cannot score. Providers are a multimethod pair
(`fetch-raw-league`, `normalize-league`); adding one is a new namespace with two
`defmethod`s, an entry in `draft-day.providers`, and a `:require` for its
registration. It is backend-proxied rather than called from the browser because
ESPN reads a private league only with the manager's own session cookies.

The network multimethods take one request map rather than positional arguments
so a host that needs a season in its URL or a cookie on its request has
somewhere to read them from; the season, the access check, the provider on the
reply and the string-coercion of every roster id are the dispatcher's job, not
each provider's.

## `POST /api/waivers`

```clojure
;; request
{:num-teams          12
 :scoring            :ppr
 :replacement-config {:qb 1 :rb 2 :wr 2 :te 1 :flex 1}
 :roster-size        15               ; seats per team, so a claim knows its cost
 :roster             { … }            ; the league's seats — what :lineup-upgrade is computed against
 :my-roster-id       1
 :league             { … }}           ; the reply from /api/league/sync, verbatim

;; response
{:through-week 8       ; read off the data, not the calendar; 0 all preseason
 :season-games 17
 :week         9       ; the week the weekly line covers; nil when there is none
 :week-fetched-at "…"  ; ISO stamp — the browser renders the wall clock
 :trending     {:fetched-at "…" :lookback-hours 48}
 :claims-left  6       ; waiver runs left — what the bids are a share of
 :faab     {:type "faab" :budget 100 :left 60 :rival-max 95}
 :drop-candidate {:player-id … :player-name … :position … :ros-points …}
 :replacement-levels { … }  ; per position, over the whole league
 :bidding  {:source :league :auctions 212 :seasons [2025 2026]
            :league-multiplier 1.04 :min-bid 0}   ; what the bids were priced from
 :rostered {"00-0038563" "Kansas Screamers" …}   ; who has him
 :my-roster-players [ … ]   ; my own roster, valued the same way; nil ≠ empty
 :players  [ … ]}      ; free agents only
```

Each free agent carries `:ros-points`, `:lineup-upgrade`, `:upgrade`,
`:walk-away`, `:bid`, `:bid-sure`, `:win-prob`, `:rivals`, `:competition`,
`:trend`, `:trending/adds`, `:season-points` and the other `:season-*` columns.
The bid keys are priced on every request even though the interface does not
show them while `db/bid-predictions?` is false.

`:roster` is what makes `:lineup-upgrade` possible: `db/starting-slots` reads
the scoring seats off it. It is deliberately not `:replacement-config`, which
drops K and DST because replacement prices neither — right there, wrong here,
since both fill a starting slot and both score.

The request's `:roster-size` is a fallback; the synced league's own seat count
wins where it has one, since the browser derives its copy from the *draft*
config and a manager can sync a league he never imported.

Stateless on the same terms as `/api/rankings`: the browser owns the synced
league and re-POSTs it. `:through-week` comes from the universe rather than
from the request, so the board cannot be asked to price a week the data has not
reached. Only the free agents come back — `:rostered` is the compact index that
answers "who has him" without re-sending most of the universe on every refresh.

## `POST /api/league/sync`

Takes the same body as the import and returns each team's roster, FAAB spent
and remaining, waiver position and record, plus the league's waiver rules, the
`:provider` it came from and the league's FAAB `:bid-history` (summaries only,
cached on disk per league and season). If the history refresh fails or times
out the rosters still go out, with `:bid-history-error` beside whatever the
cache holds. Same multimethod pair convention as the import
(`fetch-raw-rosters`, `normalize-rosters`), and deliberately a *separate* pair:
an import is a league's rules, which change once a year, while a sync is its
state, which changes every time anyone makes a claim.

The reply names its provider because `rankings.waiver` picks its id crosswalk
off it. `/api/waivers` refuses a synced league that names none rather than
guessing Sleeper — the guess would resolve nobody in an ESPN league and hand
back a board on which its whole roster is available.

## `POST /api/matchup`

Takes `{:provider :league-id :season :credentials :scoring :league :roster
:my-roster-id}` and returns every roster in the league valued for the week:

```clojure
{:week            9
 :week-fetched-at "…"
 :my-roster-id    1
 :matchups        [ … ]    ; who plays whom
 :teams           [ … ]}   ; one per roster: seats, projected and actual points,
                           ; and the best lineup by projection and by actual
```

Unlike the other boards this is a **live fetch on every request**, because a
scoreboard changes while you look at it. The week is asked of the provider and
never derived from `:through-week`: while week *N* is being played,
`(inc through-week)` is the wrong game. It runs a lean pipeline (no VBD, no
rest-of-season blend, no vendor columns). A week with no games is an empty
list, a real answer. `:actual` is nil until a player's game has kicked off, and
the best lineup by actual points is nil until every game is final. A provider
failure answers with the provider's status (502 when it gave none).

Providers are a third multimethod pair (`fetch-raw-matchups`,
`normalize-matchups`) beside the import's and the sync's.

## `GET /api/players/:id/news`

News for one player by our player id, from ESPN's fantasy news feed (newest
ten items, tags stripped). The injury designation is not here; it comes from
Sleeper with the universe.

```clojure
{:news [ … ] :fetched-at "…" :errors [ … ]}
```

A failed feed lands in `:errors` and is never cached. Replies are cached in
memory per ESPN id for `DRAFTDAY_NEWS_TTL_MINUTES`. A player with no ESPN id
(DynastyProcess lags on rookies) answers `200 {:news [] :reason :no-espn-id}`;
an id the universe does not hold is a `404`. Offline it is empty.
