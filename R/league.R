# league.R
#
# Lookups against the precomputed league reference. Nothing here scans app_data,
# so these are safe to call inside a reactive.
#
# Requires R/theme.R for METRIC_SPEC, MIN_REF_PITCHERS, COUNT_BUCKETS.

library(dplyr)


# The fallback ladder, finest first. A cell is used only if at least
# MIN_REF_PITCHERS contributed to it; otherwise the lookup steps to the next
# rung. Measured distortion from dropping each dimension, as the shift a fine
# cell's median takes when mapped into the coarser reference:
#
#            drop p_throws   drop count_bucket   drop stand
#   hb            22.5              4.7              3.4
#   ivb            4.4              5.2              3.4
#   velo          11.5              9.0              3.3
#   whiff%         7.0             11.5              9.5
#
# stand goes first because it is best or tied-best for every mean and only
# middling for rates. p_throws is never dropped: hb is not handedness
# normalized, so pooling the hands gives a bimodal reference with nothing at its
# own centre. That is a structural failure, not a thin-sample one, and no floor
# would catch it.
LADDER <- list(
  list(stand = "exact", bucket = "exact", grain = "pitch_type x p_throws x stand x count"),
  list(stand = "All",   bucket = "exact", grain = "pitch_type x p_throws x count"),
  list(stand = "All",   bucket = "All Counts", grain = "pitch_type x p_throws")
)


#' Walk the ladder and return the first usable reference row
#'
#' Returns NULL when no rung has enough contributing pitchers, which the callers
#' surface as a missing percentile rather than a confident wrong one.
lg_cell <- function(ref, metric, pitch_type, p_throws, stand, count_bucket = "All Counts") {
  for (rung in LADDER) {
    want_stand  <- if (rung$stand  == "exact") stand else rung$stand
    want_bucket <- if (rung$bucket == "exact") count_bucket else rung$bucket

    row <- ref[ref$metric       == metric      &
               ref$pitch_type   == pitch_type  &
               ref$p_throws     == p_throws    &
               ref$stand        == want_stand  &
               ref$count_bucket == want_bucket, ]

    if (nrow(row) == 1 && row$n_pitchers[[1]] >= MIN_REF_PITCHERS) {
      return(list(row = row, grain = rung$grain))
    }
  }
  NULL
}


#' League mean for a cell
#'
#' Returns NA when nothing on the ladder qualifies.
lg_mean <- function(ref, metric, pitch_type, p_throws, stand, count_bucket = "All Counts") {
  hit <- lg_cell(ref, metric, pitch_type, p_throws, stand, count_bucket)
  if (is.null(hit)) NA_real_ else hit$row$mean[[1]]
}


#' Where a value sits in the league distribution, 0 to 100
#'
#' Always returns the n it was computed from and which grain answered, because a
#' percentile without its sample size is exactly the number that misleads. The
#' caller decides how to show a fallback; it must not look identical to an exact
#' hit.
#'
#' The lookup is findInterval() against 101 stored breakpoints, so it costs
#' nothing at runtime no matter how large the season is.
#'
#' `n` here is the number of PITCHERS behind the reference. The caller's own
#' sample is a separate question, handled by lg_eligible() below.
lg_pctile <- function(ref, value, metric, pitch_type, p_throws, stand,
                      count_bucket = "All Counts") {
  miss <- list(pctile = NA_real_, n_pitchers = NA_integer_, grain = NA_character_, exact = NA)
  if (!is.finite(value)) return(miss)

  hit <- lg_cell(ref, metric, pitch_type, p_throws, stand, count_bucket)
  if (is.null(hit)) return(miss)

  q <- hit$row$q[[1]]
  # q holds the 0th through 100th percentile. findInterval returns 0 below the
  # minimum and 101 at or above the maximum, so subtracting 1 and clamping maps
  # onto 0 to 100.
  p <- min(100, max(0, findInterval(value, q) - 1))

  list(pctile     = p,
       n_pitchers = hit$row$n_pitchers[[1]],
       grain      = hit$grain,
       exact      = identical(hit$grain, LADDER[[1]]$grain))
}


#' Does the pitcher's own sample support quoting a percentile at all
#'
#' Separate from MIN_PITCH_COUNT, which only decides whether a pitch type is
#' shown. Measured on 2026: at 5 to 10 swings a window whiff% misses the same
#' pitcher's season percentile by a median of 27 points and lands more than 25
#' points away over half the time. A mean is far steadier, which is why the
#' floors differ by metric rather than being one number.
#'
#' `counts` is a named list of the denominators available for this pitch in the
#' selected window: pitches, swings, oz, pa.
lg_eligible <- function(metric, counts) {
  spec <- METRIC_SPEC[METRIC_SPEC$metric == metric, ]
  if (nrow(spec) != 1) stop("unknown metric: ", metric)
  n <- counts[[spec$denom]]
  isTRUE(is.finite(n) && n >= spec$floor)
}
