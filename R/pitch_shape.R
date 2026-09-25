# pitch_shape.R
#
# Shape and outcome per pitcher per pitch type, for the Context tab. Pure
# function over a frame, no Shiny.
#
# This file held the peer-group engine (build_cohort, nested_baselines, the
# baselines and comparables tables, the release scatter) until 2026-09-21, when
# the Context tab's Details section was cut by request and the engine had no
# caller left. Its findings are in the commit that added it (c76ed05): fixed-K
# over fixed windows, and the two matched cohorts sharing 16 of 25 pitchers.


#' Shape AND outcome per pitcher per pitch type
#'
#' Delegates to search_aggregate(), which is the Search tab's own aggregator,
#' rather than computing a second set of rates beside it. whiff%, chase% and
#' xwOBA have definitions that were argued over and verified against Savant
#' (foul tips are whiffs, bunt attempts are swings, a bunt attempt out of the
#' zone is a chase), and a second implementation here would drift from those the
#' first time one of them was corrected. It also carries the denominators
#' (swings, oz, pa) the METRIC_SPEC floors are applied to.
#'
#' It handles the traded-pitcher case the same way: pitch_team is filtered on,
#' never grouped by. Until 2026-09-12 this grouped by it and a traded pitcher
#' appeared once per club (148 duplicated combinations across 63 pitchers).
#'
#' `hand` is the BATTER side, matching the global selector. `min_pitches`
#' defaults to 50, the floor for ranking a pitch's shape; the Arsenal panel
#' passes 1 so every pitch he threw is a row.
pitch_shape <- function(df, hand = "All", min_pitches = 50, from = NULL, to = NULL) {
  pitch_shape_finish(search_aggregate(df, hand = hand, from = from, to = to), min_pitches)
}

#' The part of pitch_shape() that runs on an already-aggregated frame
#'
#' Split out so the Context tab can apply it to a precomputed search_aggregate()
#' result from league_pool.rds, and get byte-for-byte what pitch_shape() returns.
pitch_shape_finish <- function(out, min_pitches = 50) {
  # search_aggregate() joins every club he threw this pitch for inside the
  # window with a slash, which is the honest label for a row that averages both
  # halves of a trade.
  out$pitch_team <- out$team
  out[out$pitches >= min_pitches, , drop = FALSE]
}
