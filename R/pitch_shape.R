# pitch_shape.R
# Shape and outcome per pitcher per pitch type, for the Context tab.

#' Shape and outcome per pitcher per pitch type, via search_aggregate().
#' `hand` is the batter side.
pitch_shape <- function(df, hand = "All", min_pitches = 50, from = NULL, to = NULL) {
  pitch_shape_finish(search_aggregate(df, hand = hand, from = from, to = to), min_pitches)
}

#' pitch_shape() on an already aggregated (or pooled) frame
pitch_shape_finish <- function(out, min_pitches = 50) {
  # Traded pitchers: one row, teams joined with a slash
  out$pitch_team <- out$team
  out[out$pitches >= min_pitches, , drop = FALSE]
}
