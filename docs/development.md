# Development

```bash
lein test                                        # all Clojure tests
lein test draft-day.rankings.value-test          # one namespace
lein test :only draft-day.rankings.value-test/value-conserves-to-budget
npm test                                         # the ClojureScript node-test build
lein test :integration                           # hits live vendors; skips without DRAFTDAY_ESPN_* env vars
```

`src/cljc` is on the JVM classpath, so `lein test` already covers the shared
db/scoring code. `npm test` exists for the cljs-only namespaces (every
`*_test.cljs` under `test/`: `events`, `subs`, `waivers`, `matchup` and the
`views/`) that `lein test` cannot reach, so a ClojureScript test is only worth
writing for genuinely browser-side behaviour.

## Seeing the in-season half work

The current season's weekly file does not exist until week 1 is played, and last
season's is a season with nothing left to project. `DRAFTDAY_AS_OF_WEEK`
truncates the weekly rows at a week, so a finished season replays as one in
progress:

```bash
DRAFTDAY_AS_OF_WEEK=8 lein run     # a real week-8 board, against real data
```

Without it, the Waivers tab in preseason is the preseason board — correctly, and
it says so — but nothing about the blend, the bids or the trend column can be
seen doing anything.

## Tools and research harnesses

`dev/` holds the command-line tools that save data Sleeper will not give back
later, regenerate the committed fixtures, and score the models against real
outcomes. They are not part of the shipped app. See
[dev/draft_day/tools/README.md](../dev/draft_day/tools/README.md) for each one.
