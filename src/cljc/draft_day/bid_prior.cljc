(ns draft-day.bid-prior
  "What real Sleeper leagues bid for waiver claims: the Sleeper-wide backbone a
  league's own bid history is shrunk toward, and all a brand-new league has.

  Measured, not assumed. Crawled from Sleeper's public API on 2026-09-23 by
  `draft-day.faab.report` — a walk over leaguemates from the author's account —
  5,658 finished FAAB league-seasons (1,012 from 2023, 1,577 from 2024, 3,069
  from 2025), 754,611 waiver auctions and 1,103,016 competitive bids, normalized
  by the app's own `transactions/normalize-season`. A walk over leaguemates is
  not a random sample of Sleeper: it found superflex leagues more than one-QB
  (3,106 against 2,552) and dynasty far more than redraft (4,039 against 1,263).

  Over every budget, as shares of each league's own:

  | bidders | auctions | won at $0 | median win | 90th pct win |
  |---|---|---|---|---|
  | 1 | 574,042 | 61% | 0% | 7% |
  | 2 | 103,780 | 23% | 3.3% | 22% |
  | 3 | 37,925 | 7% | 9.3% | 35% |
  | 4+ | 38,864 | 1% | 18.2% | 57% |

  Shares are of the season's budget. FF Beacon publishes medians of 0 / 5 /
  10.5 / 20% from its own 15,667 winning bids, which this matches at one
  bidder, runs about a tenth under at three and four-plus, and a third under at
  two. Winners paid a median 2.0 times the second-highest bid, and 42% of
  contested auctions were won by 5% of the budget or more than it took.

  The number of bidders is what prices a claim, far more than anything else
  measured: once it is known, position and budget move the typical bid by a
  third or more — a QB half again as much, a kicker or defense 0.4 times, a
  league on another budget 0.4 to 0.6 times the share — league size by a sixth,
  and superflex and dynasty by under a tenth. The phase of the season changes
  how often a bid is $0 rather than how big a positive one is. So `bid-share`
  is cut by bidders and phase, and the rest are multipliers on it.

  The cells are measured in $100 leagues, the Sleeper default and 2,999 of the
  5,658: leagues on other budgets bid smaller shares at every quantile, not
  only at the $1 grain, and pooling them would blur the one scale bids are
  converted through. `budget-shift` says how far the rest sit.

  Managers differ more than auctions do — the middle 80% of them run from 0.5
  to 3 times the typical bid, and from 14% to 83% of their bids at $0 — and they
  keep their habits: see `persistence`. That is what makes each rival's own history
  worth reading, and last season's worth carrying.

  Not measured here: how the number of bidders is predicted, which is the model's
  job, and whether a crowd signal such as Sleeper's trending adds sharpens it —
  that cannot be read off a past season.")

(defn phase
  "The phase `bid-share` is cut by, from the week an auction was decided in:
  weeks 1-4, 5-10, and 11 on."
  [week]
  (cond (<= week 4) :early (<= week 10) :mid :else :late))

(defn bucket
  "The bidder count `bid-share` is cut by, four and up pooled: the expensive
  auctions are rare enough that splitting them further leaves cells too thin."
  [bidders]
  (min 4 bidders))

(def winning-bid
  "The ns docstring's table as data, keyed by bidders (4 meaning four or more).
  Nothing reads this; it is the provenance for the claim that bidders price a
  claim, and the check the model is scored against."
  {1 {:auctions 574042 :p-zero 0.607 :p50 0.000 :p90 0.07}
   2 {:auctions 103780 :p-zero 0.234 :p50 0.033 :p90 0.22}
   3 {:auctions 37925  :p-zero 0.071 :p50 0.093 :p90 0.35}
   4 {:auctions 38864  :p-zero 0.013 :p50 0.182 :p90 0.57}})

(def bid-share
  "One competitive bid, keyed `[bidders phase]` with four meaning four or more
  and phases weeks 1-4, 5-10 and 11 on: how often it is $0, and quantiles of the
  positive ones as a share of a $100 budget. Every bid in the auction counts,
  the winner's and those it beat.

  Fewer bids are $0 as bidders grow; late in the season more contested bids are
  $0 but the positive ones run larger, as spent budgets thin the field to the
  managers still holding money."
  {[1 :early] {:n 142561 :p-zero 0.661 :positive {0.05 0.01 0.1 0.01 0.25 0.02 0.5 0.04 0.75 0.09 0.9 0.17 0.95 0.25 0.99 0.50}}
   [1 :mid]   {:n 78111  :p-zero 0.610 :positive {0.05 0.01 0.1 0.01 0.25 0.02 0.5 0.05 0.75 0.10 0.9 0.17 0.95 0.25 0.99 0.50}}
   [1 :late]  {:n 62386  :p-zero 0.651 :positive {0.05 0.01 0.1 0.01 0.25 0.02 0.5 0.05 0.75 0.12 0.9 0.26 0.95 0.40 0.99 0.85}}
   [2 :early] {:n 46228  :p-zero 0.423 :positive {0.05 0.01 0.1 0.01 0.25 0.03 0.5 0.06 0.75 0.13 0.9 0.24 0.95 0.33 0.99 0.68}}
   [2 :mid]   {:n 32994  :p-zero 0.451 :positive {0.05 0.01 0.1 0.01 0.25 0.03 0.5 0.06 0.75 0.13 0.9 0.23 0.95 0.32 0.99 0.62}}
   [2 :late]  {:n 21220  :p-zero 0.523 :positive {0.05 0.01 0.1 0.01 0.25 0.03 0.5 0.08 0.75 0.17 0.9 0.35 0.95 0.50 0.99 0.94}}
   [3 :early] {:n 26199  :p-zero 0.283 :positive {0.05 0.01 0.1 0.02 0.25 0.04 0.5 0.08 0.75 0.17 0.9 0.30 0.95 0.41 0.99 0.80}}
   [3 :mid]   {:n 17211  :p-zero 0.332 :positive {0.05 0.01 0.1 0.02 0.25 0.04 0.5 0.08 0.75 0.16 0.9 0.28 0.95 0.40 0.99 0.72}}
   [3 :late]  {:n 8748   :p-zero 0.430 :positive {0.05 0.01 0.1 0.02 0.25 0.04 0.5 0.10 0.75 0.21 0.9 0.40 0.95 0.58 0.99 0.98}}
   [4 :early] {:n 38015  :p-zero 0.168 :positive {0.05 0.01 0.1 0.03 0.25 0.06 0.5 0.12 0.75 0.25 0.9 0.45 0.95 0.65 0.99 1.00}}
   [4 :mid]   {:n 22163  :p-zero 0.236 :positive {0.05 0.01 0.1 0.02 0.25 0.05 0.5 0.12 0.75 0.23 0.9 0.39 0.95 0.51 0.99 0.90}}
   [4 :late]  {:n 7669   :p-zero 0.342 :positive {0.05 0.01 0.1 0.02 0.25 0.06 0.5 0.15 0.75 0.33 0.9 0.55 0.95 0.75 0.99 1.00}}})

(def position-shift
  "How far a position's positive bids sit from the typical bid for the same
  number of bidders, as a natural log: +0.41 is half again as much. In the
  app's spelling — Sleeper's \"DEF\" is \"DST\" here. Measured in $100 leagues;
  the counts are positive bids."
  {"QB"  {:n 39633 :log-shift 0.41}
   "RB"  {:n 74859 :log-shift 0.15}
   "WR"  {:n 78894 :log-shift 0.00}
   "TE"  {:n 33326 :log-shift 0.00}
   "K"   {:n 2462  :log-shift -0.92}
   "DST" {:n 11531 :log-shift -0.92}})

(def team-shift
  "The same, by league size — eight teams or fewer, nine to twelve, more than
  twelve — also in $100 leagues. Leagues of more than twelve bid a sixth less;
  eight or fewer do not differ."
  {:small    {:n 1500   :log-shift 0.00}
   :standard {:n 213182 :log-shift 0.00}
   :large    {:n 29678  :log-shift -0.18}})

(def budget-shift
  "How far positive bids in leagues on any budget but $100 sit from
  `bid-share`'s, by bidders, as a natural log of the ratio of their median
  shares: 0.4 times as much for a lone bidder, about 0.6 with four or more.
  Measured over all 5,658 league-seasons; the counts are the other budgets'
  positive bids, 354,908 of them against 244,360 in $100 leagues."
  {1 {:n 125008 :log-shift -0.92}
   2 {:n 65282  :log-shift -0.63}
   3 {:n 44298  :log-shift -0.64}
   4 {:n 120320 :log-shift -0.47}})

(def heaping
  "How often positive bids land on round numbers, beside the rate chance alone
  would give. In a $100 league, of bids of $5 or more: on a multiple of $5, of
  $10, and one dollar over a multiple of $5 — the last only a little above
  chance there, so bidding $11 beats the $10 crowd while $11 itself is not
  crowded. In a $1000 league, of bids of $50 or more, the same on multiples of
  $50, where one over is five times chance: that crowd has already found the
  trick."
  {:budget-100  {:n 154808 :round 0.395 :round-2x 0.150 :one-over 0.240 :by-chance 0.20}
   :budget-1000 {:n 58675  :round 0.208 :round-2x 0.100 :one-over 0.103 :by-chance 0.02}})

(def managers
  "How managers differ, across 18,684 manager-seasons in $100 leagues with ten
  or more competitive bids: bids a week, share of bids at $0, and aggression —
  the median natural log of his positive bids over the typical bid for the same
  bidders and phase in $100 leagues. Measured on the one budget because pooled,
  a manager inherits his league's budget as a habit, and since a league keeps
  its budget, the habit reads as persistent. A manager with no history bids a
  week and at $0 like the median, and bids the backbone's typical bid — not the
  median aggression, which is a median over managers rather than over bids. The
  spread is how far one with history can be believed to sit from that."
  {:per-week   {:p10 0.69  :p50 1.19 :p90 2.53}
   :zero-share {:p10 0.14  :p50 0.50 :p90 0.83}
   :aggression {:p10 -0.69 :p50 0.15 :p90 1.10}})

(def persistence
  "Rank correlation of each habit between one manager's season and the season
  before it in the same league, and between two of his leagues in the same
  season, over pairs in $100 leagues where both sides had ten or more bids."
  {:seasons {:pairs 4674 :per-week 0.57 :zero-share 0.62 :aggression 0.43}
   :leagues {:pairs 1973 :per-week 0.43 :zero-share 0.56 :aggression 0.37}})
