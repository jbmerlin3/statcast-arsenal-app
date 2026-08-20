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


# ---- Percentile to colour ----------------------------------------------------

#' Fill colour for a percentile, on the scale its direction demands
#'
#' Direction always arrives from METRIC_SPEC and is never inferred from the
#' metric name or from the value. The switch has no fallthrough on purpose: an
#' unrecognised direction stops here rather than defaulting to high-is-good,
#' which is the failure that renders a .420 xwOBA green.
#'
#' "low" inverts the percentile, not the palette. Same scale, same reading, so a
#' plus pitch is the same colour whichever direction its metric runs.
#'
#' Vectorised over `pctile`, since a metric has one direction and a column has
#' many values. NA in, NA out.
pctile_fill <- function(pctile, direction) {
  spec <- switch(direction,
    high    = list(stops = PCTILE_PAL_DIVERGING, invert = FALSE),
    low     = list(stops = PCTILE_PAL_DIVERGING, invert = TRUE),
    neutral = list(stops = PCTILE_PAL_NEUTRAL,   invert = FALSE),
    stop("unknown direction: ", direction,
         ". Expected high, low, or neutral.", call. = FALSE))

  p   <- if (spec$invert) 100 - pctile else pctile
  out <- rep(NA_character_, length(p))
  ok  <- is.finite(p)
  if (any(ok)) {
    m <- grDevices::colorRamp(spec$stops)(pmin(1, pmax(0, p[ok] / 100)))
    out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
  }
  out
}


# ---- One cell, one question --------------------------------------------------

#' Resolve one table cell into exactly one of the four states
#'
#' This is the only place a state is decided. Everything a renderer needs comes
#' back in one object, so tables.R never reconstructs a state from a percentile,
#' a count, or a NULL. If a caller has to ask a second question, the return
#' shape is wrong rather than the caller.
#'
#' `counts` is the pitch type's own denominators in the selected window, a named
#' list with `pitches`, `swings`, `oz`, `pa`. Nothing here scans app_data, so
#' this is safe to call inside a reactive.
#'
#' Precedence. Below floor beats no reference when both are true, for two
#' reasons. It is a statement about the number actually printed on the page:
#' 22.2 off 9 swings is not worth reading even against a perfect reference, so
#' it is the nearer defect. And it is the more useful label, because it carries
#' the n, which tells the reader how much more data would fix it, where no
#' reference tells them nothing they can act on. The ladder is still walked and
#' its answer still returned in `has_ref`, so a systematic hole in league_ref
#' stays visible to a test even while every thin cell is labelled below floor.
#'
#' A non-finite value folds into below floor for the same reason. In practice it
#' only arises from a zero denominator, and the parenthetical n then says so.
resolve_cell <- function(ref, value, metric, pitch_type, p_throws, stand,
                         count_bucket = "All Counts", counts) {
  spec <- METRIC_SPEC[METRIC_SPEC$metric == metric, ]
  if (nrow(spec) != 1) stop("unknown metric: ", metric, call. = FALSE)

  n_own <- counts[[spec$denom]]
  hit   <- lg_pctile(ref, value, metric, pitch_type, p_throws, stand, count_bucket)

  state <- if (!lg_eligible(metric, counts) || !isTRUE(is.finite(value))) {
    "below_floor"
  } else if (!is.finite(hit$pctile)) {
    "no_reference"
  } else if (isTRUE(hit$exact)) {
    "exact"
  } else {
    "fallback"
  }

  sty <- CELL_STATE_STYLE[CELL_STATE_STYLE$state == state, ]

  # Below floor's marker is its own denominator, which differs per column and so
  # cannot live in the style table. Inline is the only unambiguous place for it:
  # swings for whiff%, out-of-zone for chase%, PA for xwOBA.
  marker <- if (identical(state, "below_floor")) {
    paste0(" (", if (isTRUE(is.finite(n_own))) n_own else 0, ")")
  } else {
    sty$marker
  }

  state_note <- switch(state,
    exact    = NA_character_,
    fallback = paste0("Percentile from a coarser league cut, ", hit$grain,
                      ", built on ", hit$n_pitchers, " pitchers."),
    below_floor = paste0("Grey values sit below the ", spec$floor, " ", spec$denom,
                         " floor for this metric. The figure in parentheses is ",
                         "the value's own denominator."),
    no_reference = paste0("No league reference at any grain for this pitch type ",
                          "and pitcher hand, so no percentile is shown. The ",
                          "pitcher's own sample is not the limit here."))

  list(
    state       = state,
    fill        = if (sty$filled) pctile_fill(hit$pctile, spec$direction) else PCTILE_UNFILLED,
    text_color  = sty$text_color,
    font_style  = sty$font_style,
    font_weight = sty$font_weight,
    marker      = marker,
    n           = if (isTRUE(is.finite(n_own))) as.integer(n_own) else NA_integer_,
    denom       = spec$denom,
    floor       = spec$floor,
    pctile      = hit$pctile,
    n_pitchers  = hit$n_pitchers,
    # Named only when the state is fallback. An exact hit is at the finest grain
    # by definition, so naming it would be noise.
    grain       = if (identical(state, "fallback")) hit$grain else NA_character_,
    state_note  = state_note,
    # A separate channel from state_note on purpose. state_note is about this
    # cell's reference; metric_note is about how the metric reads at all, and an
    # hb cell can be a fallback as well.
    metric_note = if (metric %in% names(METRIC_NOTES)) unname(METRIC_NOTES[[metric]]) else NA_character_,
    # Diagnostic, not a rendering input. Below floor hides whether the reference
    # existed, so this is what lets a test see a hole in league_ref.
    has_ref     = isTRUE(is.finite(hit$pctile))
  )
}


#' Resolve a whole arsenal column, one row per pitch type
#'
#' gt fills column-wise, so this is the grain tables.R actually renders at: it
#' folds one tab_style per distinct fill into a column. The scalar form above
#' stays the unit of truth and the unit of test.
#'
#' Deliberately branch-free. Any logic added here rather than to resolve_cell()
#' is a state decided in two places, which is the thing this pair exists to
#' prevent.
#'
#' `counts` is one row per pitch type, aligned to `values`, carrying the four
#' denominator columns.
resolve_column <- function(ref, values, metric, pitch_types, p_throws, stand,
                           count_bucket = "All Counts", counts) {
  stopifnot("values, pitch_types and counts must be the same length" =
              length(values) == length(pitch_types) && nrow(counts) == length(values))

  cells <- lapply(seq_along(values), function(i) {
    resolve_cell(ref, values[[i]], metric, as.character(pitch_types[[i]]),
                 p_throws, stand, count_bucket, as.list(counts[i, , drop = FALSE]))
  })

  out <- do.call(rbind, lapply(cells, as.data.frame, stringsAsFactors = FALSE))
  cbind(pitch_type = as.character(pitch_types), out, stringsAsFactors = FALSE)
}
