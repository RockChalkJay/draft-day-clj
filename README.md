# draft-day-clj
Fantasy football draft assistant geared towards auction drafts

## TODO

- **Scoring-aware FantasyPros ECR.** The player universe is a single, scoring-agnostic
  shared cache, so the ECR enrichment is scraped once with a hardcoded `:ppr` cheatsheet
  (`pipeline.clj`). Non-PPR leagues therefore get PPR-flavored tiers and rank-spread (the
  two ECR fields the rankings engine consumes: `:fantasypros/ecr-tier` and
  `:fantasypros/rank-std`). Fix: fetch all three cheatsheet formats, store each under
  distinct keys in the universe, and select the variant matching the request's scoring
  format at ranking time — i.e. move the ECR join out of ingestion into the rankings path.
