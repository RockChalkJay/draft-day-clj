(ns draft-day.bid-prior
  "What real Sleeper leagues bid for waiver claims: the Sleeper-wide backbone a
  league's own bid history is shrunk toward, and all a brand-new league has.

  Measured, not assumed. Crawled from Sleeper's public API on 2026-09-23 by
  `draft-day.faab.report` — a walk over leaguemates from the author's account —
  408 finished FAAB league-seasons (80 from 2023, 133 from 2024, 195 from 2025),
  55,745 waiver auctions and 80,957 competitive bids, normalized by the app's own
  `transactions/normalize-season`. Superflex and one-QB are balanced (205/203);
  the walk found dynasty leagues more than redraft (273 against 103).

  Over every budget, as shares of each league's own:

  | bidders | auctions | won at $0 | median win | 90th pct win |
  |---|---|---|---|---|
  | 1 | 42,586 | 64% | 0% | 7% |
  | 2 | 7,634 | 25% | 3.3% | 21% |
  | 3 | 2,720 | 8% | 10.0% | 35% |
  | 4+ | 2,805 | 2% | 20.0% | 60% |

  Shares are of the season's budget. FF Beacon publishes medians of 0 / 5 /
  10.5 / 20% from its own 15,667 winning bids, which this matches at one, three
  and four-plus bidders and runs a third under at two. Winners
  paid a median 2.0 times the second-highest bid, and 42% of contested
  auctions were won by 5% of the budget or more than it took.

  The number of bidders is what prices a claim, far more than anything else
  measured: once it is known, position, league size and budget move the typical
  bid by a third to two thirds, and superflex and dynasty barely at all. The phase of
  the season changes how often a bid is $0 rather than how big a positive one
  is. So `bid-share` is cut by bidders and phase, and the rest are multipliers
  on it.

  The cells are measured in $100 leagues, the Sleeper default and 235 of the 408:
  leagues on other budgets bid smaller shares at every quantile, not only at the
  $1 grain, and pooling them would blur the one scale bids are converted
  through. `budget-shift` says how far the rest sit.

  Managers differ more than auctions do — the middle 80% of them run from 0.4
  to 4 times the typical bid, and from 7% to 82% of their bids at $0 — and they
  keep their habits: see `persistence`. That is what makes each rival's own history
  worth reading, and last season's worth carrying.

  Not measured here: how the number of bidders is predicted, which is the model's
  job, and whether a crowd signal such as Sleeper's trending adds sharpens it —
  that cannot be read off a past season.")

(def winning-bid
  "The ns docstring's table as data, keyed by bidders (4 meaning four or more).
  Nothing reads this; it is the provenance for the claim that bidders price a
  claim, and the check the model is scored against."
  {1 {:auctions 42586 :p-zero 0.637 :p50 0.000 :p90 0.07}
   2 {:auctions 7634  :p-zero 0.250 :p50 0.033 :p90 0.21}
   3 {:auctions 2720  :p-zero 0.081 :p50 0.100 :p90 0.35}
   4 {:auctions 2805  :p-zero 0.020 :p50 0.200 :p90 0.60}})

(def bid-share
  "One competitive bid, keyed `[bidders phase]` with four meaning four or more
  and phases weeks 1-4, 5-10 and 11 on: how often it is $0, and quantiles of the
  positive ones as a share of a $100 budget. Every bid in the auction counts,
  the winner's and those it beat.

  Fewer bids are $0 as bidders grow; late in the season more are $0 but the
  positive ones run larger, as spent budgets thin the field to the managers
  still holding money."
  {[1 :early] {:n 11489 :p-zero 0.677 :positive {0.05 0.01 0.1 0.01 0.25 0.02 0.5 0.04 0.75 0.09 0.9 0.16 0.95 0.25 0.99 0.50}}
   [1 :mid]   {:n 6909  :p-zero 0.634 :positive {0.05 0.01 0.1 0.01 0.25 0.02 0.5 0.04 0.75 0.09 0.9 0.17 0.95 0.24 0.99 0.48}}
   [1 :late]  {:n 5364  :p-zero 0.666 :positive {0.05 0.01 0.1 0.01 0.25 0.02 0.5 0.05 0.75 0.13 0.9 0.27 0.95 0.41 0.99 0.79}}
   [2 :early] {:n 3762  :p-zero 0.439 :positive {0.05 0.01 0.1 0.01 0.25 0.02 0.5 0.06 0.75 0.12 0.9 0.22 0.95 0.32 0.99 0.52}}
   [2 :mid]   {:n 2718  :p-zero 0.449 :positive {0.05 0.01 0.1 0.01 0.25 0.03 0.5 0.06 0.75 0.12 0.9 0.23 0.95 0.31 0.99 0.51}}
   [2 :late]  {:n 1730  :p-zero 0.526 :positive {0.05 0.01 0.1 0.01 0.25 0.03 0.5 0.07 0.75 0.15 0.9 0.30 0.95 0.40 0.99 0.76}}
   [3 :early] {:n 2070  :p-zero 0.292 :positive {0.05 0.01 0.1 0.01 0.25 0.03 0.5 0.08 0.75 0.17 0.9 0.30 0.95 0.40 0.99 0.70}}
   [3 :mid]   {:n 1314  :p-zero 0.358 :positive {0.05 0.01 0.1 0.01 0.25 0.03 0.5 0.08 0.75 0.16 0.9 0.30 0.95 0.40 0.99 0.71}}
   [3 :late]  {:n 681   :p-zero 0.401 :positive {0.05 0.01 0.1 0.02 0.25 0.05 0.5 0.10 0.75 0.22 0.9 0.42 0.95 0.60 0.99 0.99}}
   [4 :early] {:n 2977  :p-zero 0.163 :positive {0.05 0.01 0.1 0.02 0.25 0.05 0.5 0.12 0.75 0.25 0.9 0.41 0.95 0.62 0.99 1.00}}
   [4 :mid]   {:n 1583  :p-zero 0.227 :positive {0.05 0.01 0.1 0.02 0.25 0.05 0.5 0.12 0.75 0.25 0.9 0.39 0.95 0.50 0.99 0.83}}
   [4 :late]  {:n 652   :p-zero 0.293 :positive {0.05 0.01 0.1 0.03 0.25 0.06 0.5 0.15 0.75 0.31 0.9 0.51 0.95 0.70 0.99 1.00}}})

(def position-shift
  "How far a position's positive bids sit from the typical bid for the same
  number of bidders, as a natural log: +0.41 is half again as much. In the
  app's spelling — Sleeper's \"DEF\" is \"DST\" here. Measured in $100 leagues;
  the counts are positive bids."
  {"QB"  {:n 3002 :log-shift 0.41}
   "RB"  {:n 5955 :log-shift 0.15}
   "WR"  {:n 6057 :log-shift 0.00}
   "TE"  {:n 2693 :log-shift 0.00}
   "K"   {:n 244  :log-shift -0.92}
   "DST" {:n 1081 :log-shift -0.69}})

(def team-shift
  "The same, by league size — eight teams or fewer, nine to twelve, more than
  twelve — also in $100 leagues. The small-league cell rests on 57 bids and is
  not to be leaned on."
  {:small    {:n 57    :log-shift -0.13}
   :standard {:n 17268 :log-shift 0.00}
   :large    {:n 2027  :log-shift -0.41}})

(def budget-shift
  "How far positive bids in leagues on any budget but $100 sit from
  `bid-share`'s, by bidders, as a natural log of the ratio of their median
  shares: a third as much for a lone bidder, two thirds with four or more.
  Measured over all 408 league-seasons; the counts are the other budgets'
  positive bids, 22,363 of them against 19,352 in $100 leagues."
  {1 {:n 7430 :log-shift -1.10}
   2 {:n 4114 :log-shift -0.59}
   3 {:n 2844 :log-shift -0.52}
   4 {:n 7975 :log-shift -0.42}})

(def heaping
  "How often positive bids land on round numbers, beside the rate chance alone
  would give. In a $100 league, of bids of $5 or more: on a multiple of $5, of
  $10, and one dollar over a multiple of $5 — the last only a little above
  chance, so bidding $11 beats the $10 crowd while $11 itself is not crowded. In
  a $1000 league, of bids of $50 or more, the same on multiples of $50."
  {:budget-100  {:n 12113 :round 0.398 :round-2x 0.153 :one-over 0.250 :by-chance 0.20}
   :budget-1000 {:n 3747  :round 0.191 :round-2x 0.087 :one-over 0.096 :by-chance 0.02}})

(def managers
  "How managers differ, across 2,888 manager-seasons with ten or more
  competitive bids: bids a week, share of bids at $0, and aggression — the
  median natural log of his positive bids over the typical bid for the same
  bidders and phase. A manager with no history bids a week and at $0 like the
  median, and bids the backbone's typical bid — not the median aggression, which
  is a median over managers rather than over bids. The spread is how far one
  with history can be believed to sit from that."
  {:per-week   {:p10 0.69  :p50 1.25 :p90 2.82}
   :zero-share {:p10 0.07  :p50 0.50 :p90 0.82}
   :aggression {:p10 -0.92 :p50 0.19 :p90 1.39}})

(def persistence
  "Rank correlation of each habit between one manager's season and the season
  before it in the same league, and between two of his leagues in the same
  season, over pairs where both sides had ten or more bids."
  {:seasons {:pairs 921 :per-week 0.63 :zero-share 0.64 :aggression 0.53}
   :leagues {:pairs 277 :per-week 0.56 :zero-share 0.47 :aggression 0.38}})
