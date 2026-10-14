# Sensei

Sensei is Draft Day's AI assistant for in-season decisions: which free agent to
claim, whom to drop for him, who should start. It reads the same board the
Waivers tab draws, and its job is to weigh and explain those numbers, never to
make up its own. Linked from the [README](../README.md#api).

This document records how Sensei reaches a model and why, what the first step
(an MCP server) does, and what it measured.

## Use it

Sensei answers from one league, named by three env vars on the server:

```bash
export DRAFTDAY_SENSEI_PROVIDER=sleeper          # or espn
export DRAFTDAY_SENSEI_LEAGUE_ID=<league id>
export DRAFTDAY_SENSEI_ROSTER_ID=<roster id>    # your team; a wrong id lists the teams
lein run
```

An ESPN league also needs `DRAFTDAY_ESPN_SWID` and `DRAFTDAY_ESPN_S2`, the
names the integration tests already read.

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
  sensei.context   {provider league-id roster-id} -> the /api/waivers body
  sensei.tools     my_team, waiver_targets, player_news
  sensei.mcp       POST /api/sensei/mcp
       ^ Claude Code (now)      ^ claude -p (next)      ^ API adapter (later)
```

**The league without a browser.** The browser owns a league's state, but a
connected league's rules are whatever its import says and its rosters are
whatever its sync says. `sensei.context` runs both, concurrently, and builds
the body the Waivers tab would post. It round-trips that body through JSON, so
the board sees what it always sees. The only fact neither call knows is which
team is the manager's.

**One board.** The waiver board's pipeline moved out of `waivers-handler` into
`routes/waivers-reply`, and the tools read it there. Rows are the board's
columns read through `db/waiver-sort-accessors`, and their legend is the column
catalog's tooltips. `my_team` splits the roster through `db/team-roster`, as My
Team does.

**What the screen holds back, Sensei holds back.** No bid predictions while
`db/bid-predictions?` is off, and the instructions tell the model not to put a
FAAB figure on a claim. No raw rest-of-season points, which no surface shows.
`lineup`, `upgrade` and VORP stay.

**No credential reaches the model.** No tool takes one. ESPN cookies come from
the environment, and a provider error is masked before it becomes a tool
result.

**This machine only.** http-kit listens on every interface, so the endpoint
refuses anything but loopback. It also refuses a browser `Origin` other than
localhost, which is how a web page would reach a loopback server by DNS
rebinding.

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

- Isolation held: the session listed only Sensei's three tools, loaded no
  CLAUDE.md or skills, and authenticated through the plan (`apiKeySource:
  none`).
- `my_team` takes about 1.1 s (import, sync, board) and `waiver_targets` about
  0.3 s. Tool results run 1–4K characters. News is capped at five notes,
  which halved it.
- The answers quoted the tools' numbers, named the drop and made no bid
  suggestion. Both found something the board does not say: the manager's
  kicker and defense were both on bye.

## Open questions

- **The drop candidate ignores byes.** Both answers overruled it.
  `waiver/drop-candidate` nominated the Colts DST in the week the Vikings DST,
  the starter, was on bye. That is a board finding, not a Sensei one.
- **"What if I drop someone else?"** `lineup` and `upgrade` are measured
  against the board's drop, and the model said it could not price another one.
  A tool that takes the drop as an argument would answer it.
- **Calls in one turn rebuild the league.** Claude calls tools in parallel,
  and each HTTP request runs its own import and sync. A short memo per league
  would save a second per turn.
- **The in-app panel (`claude -p`).** Each new working directory leaves an
  empty auto-memory folder under `~/.claude/projects`, even with
  `--no-session-persistence`, so the panel should run from one fixed
  directory. Gate it behind `DRAFTDAY_SENSEI=claude-cli`, run one question at a
  time, and mock the UI before building it.

[plan]: https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan
[legal]: https://code.claude.com/docs/en/legal-and-compliance
[sdk]: https://code.claude.com/docs/en/agent-sdk/overview
[headless]: https://code.claude.com/docs/en/headless
[connectors]: https://support.claude.com/en/articles/11175166-get-started-with-custom-connectors-using-remote-mcp
