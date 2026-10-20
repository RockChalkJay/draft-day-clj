# Sensei

Sensei is Draft Day's AI assistant for in-season decisions: which free agent to
claim, whom to drop for him, who should start. It reads the same board the
Waivers tab draws, and its job is to weigh and explain those numbers, never to
make up its own. Linked from the [README](../README.md#api).

This document records how Sensei reaches a model and why, what the first step
(an MCP server) does, and what it measured.

## Use it

Sensei answers about every league you play in, found from your accounts. Name
the accounts in the server's env, not the leagues:

```bash
export DRAFTDAY_SENSEI_SLEEPER_USER=<sleeper username>
export DRAFTDAY_ESPN_SWID='{...}'                # ESPN, if you play there:
export DRAFTDAY_ESPN_S2=<espn_s2>                # the cookies Settings asks for
lein run
```

Either host alone is enough, but ESPN needs both cookies. The README's
[Connect Sensei](../README.md#connect-sensei) says where to find them in your
browser and how to keep them in your shell's startup file, including the
`export` that is easy to leave off. The ESPN names are the ones the integration tests already read. Sensei lists each account's leagues for the season, as Settings'
Connect does, and takes the team the account owns as yours, so a league joined
tomorrow is there tomorrow and there is no league or roster id to look up. The
`leagues` tool lists them with the key the other tools take; a question that
names a league ("my work league") needs nothing more, and with one league there
is nothing to name. `/mcp__sensei__weekly_review` reviews every league in turn,
or one if you give it a `league`.

Then connect Claude Code to the endpoint. For one session, with nothing written
to Claude Code's config:

```bash
claude --strict-mcp-config --mcp-config \
  '{"mcpServers":{"sensei":{"type":"http","url":"http://127.0.0.1:8080/api/sensei/mcp"}}}'
```

Or register it once with `claude mcp add --transport http sensei
http://127.0.0.1:8080/api/sensei/mcp`, which writes `~/.claude.json`. Ask
anything about your team, or run `/mcp__sensei__weekly_review`.

Claude Desktop's local servers speak stdio, not HTTP, so it needs a bridge such
as `mcp-remote`. That has not been tried. claude.ai cannot reach it at all: a
custom connector is called from Anthropic's cloud, and Sensei answers only this
machine.

## What a Claude Pro plan allows

The constraint behind every choice here: the plan is Claude Pro, which has no
API key. What Anthropic's terms say, as of October 2026:

- **Personal use of your own plan is fine.** `claude -p` and the Agent SDK draw
  on the subscription's limits ([help article, updated 2026-10-07][plan]).
  Those limits assume "ordinary, individual usage", and Sensei shares them with
  any other Claude Code use.
- **A product may not use a plan on its users' behalf.** No claude.ai login
  inside your app, no routing users' requests through Free/Pro/Max
  credentials, no collecting or storing claude.ai tokens. Each end user signs
  in with their own plan or API key through Anthropic's own flow
  ([legal and compliance][legal], [Agent SDK overview][sdk]).
- **The Agent SDK is Python and TypeScript only.** For any other language the
  docs say to run the CLI as a subprocess with `-p` and `--output-format json`
  ([headless][headless]). `--bare`, the mode recommended for scripts, does not
  read a subscription login.
- **Pro includes custom remote MCP connectors** on claude.ai, Desktop and
  mobile, called from Anthropic's cloud, so the server must be public HTTPS
  ([connectors][connectors]). Claude Code connects to a localhost server
  directly.

So the subscription can power Sensei for its owner on their own machine, and
for nobody else.

## Options considered

| Option | For | Against |
| --- | --- | --- |
| **MCP server, with a Claude app as the client** | Allowed on Pro; no inference bill; Claude's own chat UI; every other option reuses its tools | The advice lives in Claude's window; tools must find the league without a browser; remote use needs public HTTPS and, for anyone else, OAuth |
| **`claude -p` from the backend** | Anthropic's documented route for other languages; an in-app panel on the Pro plan; JSON schema output, streaming, resumable sessions | Personal use only; shares Pro limits; seconds of startup per question; must be isolated by hand, since `--bare` is out |
| Community Java SDKs over the CLI (markpollack's 1.4.0, and others) | Typed messages, sync and async clients, hooks | Everything against `claude -p`, plus third-party lag behind the CLI and Reactor in the tree. Little over a page of Clojure |
| The TypeScript Agent SDK in a sidecar | Anthropic-maintained, in-process tools | The same binary and terms as `claude -p`, plus a Node process to run |
| **The API with a Console key** | The only way to serve other people from Draft Day's own UI; concurrent, cacheable | Costs money; a separate Console account, since Pro carries no API credits |
| A key the user pastes into the browser | Fits "all state in the browser" | Needs an API key, not a plan; a key in localStorage |
| A local open-weight model | Free, private, offline | Weaker at weighing many factors and at calling tools |

**Decision.** The MCP server first. `claude -p` next, for an in-app panel on
the same tools. An API adapter on the same tools only if someone other than
the owner uses it.

## How it works

```
  sensei.context   accounts -> leagues -> the /api/waivers body of each
  sensei.tools     leagues, my_team, waiver_targets, player_news
  sensei.mcp       POST /api/sensei/mcp
  sensei.chat      POST /api/sensei/chat, GET /api/sensei/status
       ^ Claude Code      ^ claude -p, the in-app panel      ^ API adapter (later)
```

**The leagues without a browser.** The browser owns a league's state, but a
connected league's rules are whatever its import says and its rosters are
whatever its sync says. `sensei.context` runs both, concurrently, and builds
the body the Waivers tab would post. It round-trips that body through JSON, so
the board sees what it always sees. The only fact neither call knows is which
team is the manager's, and the account that found the league answers it:
`db/my-roster-id-for` matches the account's user id to a team's owner, the same
function the browser uses when a league is first synced.

**Several leagues at once.** Nothing is held per process, so leagues do not
interfere: a tool call carries its `league`, a batch prices one board per league
it asks about, and a league's import and sync are shared for 30 seconds between
calls (Claude fires a turn's tools in parallel), with a failed fetch never kept.
The account list is kept ten minutes. With more than one league, and no `league`
named, a tool answers with the choices rather than guessing: a wrong guess is
one league's board under another's name. The same holds if an account could not
be listed, since then the only league found is not known to be the only one.
Every reply states its league.

**One board.** The waiver board's pipeline moved out of `waivers-handler` into
`routes/waivers-reply`, and the tools read it there. Rows are the board's
columns read through `db/waiver-sort-accessors`, and their legend is the column
catalog's tooltips. `my_team` splits the roster through `db/team-roster`, as My
Team does.

**What the screen holds back, Sensei holds back.** No bid predictions while
`db/bid-predictions?` is off, and the instructions tell the model not to put a
FAAB figure on a claim. No raw rest-of-season points, which no surface shows.
`lineup`, `upgrade` and VORP stay.

**No credential reaches the model.** No tool takes one. Cookies come from the
environment, a league's entry carries them no further than the import and sync,
and `leagues` states neither the account's user id (on ESPN it is the SWID) nor
its credentials. Every tool reply is masked against every account's secrets on
its way out, in each spelling an error might echo.

**This machine only.** http-kit listens on every interface, so the endpoint
refuses anything but loopback. It also refuses a browser `Origin` other than
localhost, which is how a web page would reach a loopback server by DNS
rebinding. A tunnel (ngrok, cloudflared) connects from loopback too, so a `Host`
that is not this machine's, or an `X-Forwarded-For`, `Forwarded` or `X-Real-IP`
header, is refused as well.

**Hand-rolled protocol.** The official MCP Java SDK's server transport is a
Servlet, which does not mount on http-kit. A tools-only server speaks a
handful of methods: `initialize`, `ping`, `tools/list`, `tools/call`, and
`prompts/list` and `prompts/get` for the weekly review. If the protocol churns, `latacora/mcp-sdk` (Ring, over the
Java SDK) is the fallback. Claude Code 2.1.286 connected on the first try.

## Measured

Two questions from Claude Code 2.1.286 on Opus 5.5, against a real 12-team
Sleeper league in week 6. Both used `--tools ""`, `--strict-mcp-config` and a
working directory outside the repo:

| Question | Tool calls | Turns | Time | Cost estimate* |
| --- | --- | --- | --- | --- |
| Who should I pick up this week, and whom would I drop? | 4 | 5 | 26 s | $0.17 |
| Is Ladd McConkey healthy enough to start, and who would replace him? | 4 | 5 | 17 s | $0.12 |

\* Claude Code's client-side estimate at API prices. On Pro, nothing is billed;
the run draws on the plan's limits.

- Isolation held: the session listed only Sensei's four tools, loaded no
  CLAUDE.md or skills, and authenticated through the plan (`apiKeySource:
  none`).
- `my_team` takes about 1.1 s (import, sync, board) and `waiver_targets` about
  0.3 s. Tool results run 1–4K characters. News is capped at five notes,
  which halved it.
- The answers quoted the tools' numbers, named the drop and made no bid
  suggestion. Both found something the board does not say: the manager's
  kicker and defense were both on bye.

## Open questions

- **One account per host.** The env holds one ESPN cookie pair and one Sleeper
  username. A second ESPN login is out of reach, and so is a team the account
  co-owns when ESPN lists someone else first: the team is found by owner id, and
  ESPN publishes one.
- **The drop candidate ignores byes.** Both answers overruled it.
  `waiver/drop-candidate` nominated the Colts DST in the week the Vikings DST,
  the starter, was on bye. That is a board finding, not a Sensei one.
- **"What if I drop someone else?"** `lineup` and `upgrade` are measured
  against the board's drop, and the model said it could not price another one.
  A tool that takes the drop as an argument would answer it.
- **The in-app panel (`claude -p`).** `sensei.chat` runs one `claude -p` per
  question and streams its answer as NDJSON (`POST /api/sensei/chat`). It is
  off unless `DRAFTDAY_SENSEI=claude-cli`, answers this machine only (`GET
  /api/sensei/status` says `false`, not `403`, from anywhere else) and takes one
  question at a time (`429` for a second). The process runs from one
  owner-only directory under the system temp dir, refused if it is a link or
  another user's, since `-p` trusts its working directory without asking. Its
  flags are `--tools ""`, `--setting-sources ""` (no settings file, so no hook
  or plugin), `--strict-mcp-config` pointing at the port this server bound (not
  the one a request's `Host` names) and `--no-session-persistence`. Its
  environment sets `CLAUDE_CODE_DISABLE_CLAUDE_MDS=1` and carries no
  `DRAFTDAY_`, `ANTHROPIC_` or `CLAUDE_CODE_USE_` variable: an API key wins
  over the login under `-p` and would bill at API prices. The prompt goes in on
  stdin, never on the command line. Events are `{session-id}`, `{tool-start}`,
  `{text}`, and one `{done}` or `{error}` at the end. A client only appends
  `text`: the text after a tool call arrives opening its own paragraph. It
  renders that text as a safe markdown subset, with no image, no raw HTML and no
  link fetched on its own, because the tools hand the model words league-mates
  write and an instruction planted in a team name could otherwise make the
  browser fetch a URL. Every word is masked against the accounts' secrets,
  including one the CLI splits across two deltas: text that could be the start
  of a secret waits for the next. A run is stopped three minutes after it
  starts, answered or not, and stopping it kills whatever the process started.

  Each question is a fresh process, so the browser keeps the transcript and
  sends it with every question (`history->prompt`). That is the seam for
  sessions later: the request already accepts a `session-id` that is ignored
  today, the stream already reports the CLI's, and the session flags are one
  function (`session-flags`). Moving over means `--resume <id>` in place of
  `--no-session-persistence`, a fallback that replays the transcript when a
  resume fails, and pruning the session files. The cost of staying stateless is
  tokens and seconds that grow with the conversation; the cost of sessions is
  server-side state, files to prune, and a stale session answering after a
  league switch.

[plan]: https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan
[legal]: https://code.claude.com/docs/en/legal-and-compliance
[sdk]: https://code.claude.com/docs/en/agent-sdk/overview
[headless]: https://code.claude.com/docs/en/headless
[connectors]: https://support.claude.com/en/articles/11175166-get-started-with-custom-connectors-using-remote-mcp
