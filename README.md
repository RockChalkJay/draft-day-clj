# draft-day-clj
Fantasy football draft assistant geared towards auction drafts

## TODO

- ~~Player budgeting~~

- **Persistence**

- **Don't position filter on suggestion cards**

- **Remove strategy tabs - they don't add value**

- **Injury history not just current health. Color code for serious injury or suspension**

- **Remove the 🚨 for tier cliffs. Postion views with tier coloring accomplish the same thing in a clean way.**

- **Fix tier row color bug**

- **Warning when a nominated player would cause 3 or more shared bye weeks at the same position**

- **Scoring-aware FantasyPros enrichments (ECR + market price).** The player universe is a
  single, scoring-agnostic shared cache, so the FantasyPros enrichments are scraped once
  with PPR hardcoded (`pipeline.clj`): the ECR cheatsheet (`:ppr`) and the AAV auction
  calculator (`scoring=PPR`). ESPN's market fallback is likewise its PPR auction value.
  Non-PPR leagues therefore get PPR-flavored tiers and rank-spread (`:fantasypros/ecr-tier`,
  `:fantasypros/rank-std`) and PPR-flavored market prices (`:fantasypros/aav`,
  `:espn/auction-value`). Fix: fetch each format, store the variants under distinct keys in
  the universe, and select the one matching the request's scoring format at ranking time —
  i.e. move these joins out of ingestion into the rankings path.
