# league.R
# League reference lookups and the four cell states. Nothing here scans app_data.

library(dplyr)


# Fallback ladder, finest first: a cell under MIN_REF_PITCHERS steps down a rung.
# Batter side goes first, then count. Only rate metrics reach the last rung, which pools
# both pitcher hands; shape metrics never do (raw HB has opposite signs by hand).
LADDER <- list(
  list(stand = "exact", bucket = "exact", grain = "pitch_type x p_throws x stand x count"),
  list(stand = "All",   bucket = "exact", grain = "pitch_type x p_throws x count"),
  list(stand = "All",   bucket = "All Counts", grain = "pitch_type x p_throws"),
  list(stand = "All",   bucket = "All Counts", hand = "All", grain = "pitch_type, both hands")
)


#' First usable reference row on the ladder, or NULL
lg_cell <- function(ref, metric, pitch_type, p_throws, stand, count_bucket = "All Counts") {
  is_rate <- metric %in% METRIC_SPEC$metric[METRIC_SPEC$kind == "rate"]
  for (rung in LADDER) {
    if (!is.null(rung$hand) && !is_rate) next
    want_stand  <- if (rung$stand  == "exact") stand else rung$stand
    want_bucket <- if (rung$bucket == "exact") count_bucket else rung$bucket
    want_hand   <- if (is.null(rung$hand)) p_throws else rung$hand

    row <- ref[ref$metric       == metric      &
               ref$pitch_type   == pitch_type  &
               ref$p_throws     == want_hand   &
               ref$stand        == want_stand  &
               ref$count_bucket == want_bucket, ]

    if (nrow(row) == 1 && row$n_pitchers[[1]] >= MIN_REF_PITCHERS) {
      return(list(row = row, grain = rung$grain))
    }
  }
  NULL
}


#' Percentile (0-100) of a value in the league distribution, with the pitcher
#' count and the grain that answered
lg_pctile <- function(ref, value, metric, pitch_type, p_throws, stand,
                      count_bucket = "All Counts") {
  miss <- list(pctile = NA_real_, n_pitchers = NA_integer_, grain = NA_character_,
               exact = NA, mean = NA_real_)
  if (!is.finite(value)) return(miss)

  hit <- lg_cell(ref, metric, pitch_type, p_throws, stand, count_bucket)
  if (is.null(hit)) return(miss)

  q <- hit$row$q[[1]]
  # q holds the 0th to 100th percentiles
  p <- min(100, max(0, findInterval(value, q) - 1))

  list(pctile     = p,
       n_pitchers = hit$row$n_pitchers[[1]],
       grain      = hit$grain,
       exact      = identical(hit$grain, LADDER[[1]]$grain),
       mean       = hit$row$mean[[1]])
}


#' Distance from the league mean on a 0-100 scale: 50 = average, 0 and 100 = a
#' full span below and above (clamped)
shape_delta_pctile <- function(value, league_mean, span = SHAPE_DELTA_SPAN) {
  if (!is.finite(value) || !is.finite(league_mean) || !is.finite(span) || span <= 0) {
    return(NA_real_)
  }
  50 + 50 * max(-1, min(1, (value - league_mean) / span))
}


#' Does the pitcher's own sample clear the metric's floor
lg_eligible <- function(metric, counts) {
  spec <- METRIC_SPEC[METRIC_SPEC$metric == metric, ]
  if (nrow(spec) != 1) stop("unknown metric: ", metric)
  n <- counts[[spec$denom]]
  isTRUE(is.finite(n) && n >= spec$floor)
}


# ---- Percentile to color -----------------------------------------------------

#' Fill color for a percentile. "low" inverts it; "extreme" folds it to
#' 2 * |p - 50|, so both tails fill the same. An unknown direction stops.
pctile_fill <- function(pctile, direction) {
  spec <- switch(direction,
    high    = list(stops = PCTILE_PAL_DIVERGING, invert = FALSE, fold = FALSE),
    low     = list(stops = PCTILE_PAL_DIVERGING, invert = TRUE,  fold = FALSE),
    extreme = list(stops = PCTILE_PAL_DIVERGING, invert = FALSE, fold = TRUE),
    neutral = list(stops = PCTILE_PAL_NEUTRAL,   invert = FALSE, fold = FALSE),
    stop("unknown direction: ", direction,
         ". Expected high, low, extreme, or neutral.", call. = FALSE))

  p   <- if (spec$invert) 100 - pctile else pctile
  if (spec$fold) p <- 2 * abs(p - 50)
  out <- rep(NA_character_, length(p))
  ok  <- is.finite(p)
  if (any(ok)) {
    m <- grDevices::colorRamp(spec$stops)(pmin(1, pmax(0, p[ok] / 100)))
    out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
  }
  out
}


# ---- Cell states -------------------------------------------------------------

#' Resolve one table cell to exact, fallback, below_floor or no_reference, with
#' everything a renderer needs. Below floor wins over no reference; a value that
#' can't be computed counts as below floor.
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

  # Below floor shows its own denominator, unless that's the COUNT column
  marker <- if (identical(state, "below_floor") &&
                !identical(spec$denom, DENOM_SHOWN_AS_COUNT)) {
    paste0(" (", if (isTRUE(is.finite(n_own))) n_own else 0, ")")
  } else if (identical(state, "below_floor")) {
    ""
  } else {
    sty$marker
  }

  state_note <- switch(state,
    exact    = NA_character_,
    fallback = paste0("Percentile from a coarser league cut, ", hit$grain,
                      ", built on ", hit$n_pitchers, " pitchers."),
    below_floor = if (identical(spec$denom, DENOM_SHOWN_AS_COUNT))
      paste0("Every window is ranked against the full-season league, including ",
             "short ones, so a cell reads as what happened over the dates ",
             "selected rather than as a projection. The COUNT column is the ",
             "sample behind each row.")
    else
      paste0("A figure in parentheses is the value's own denominator where it ",
             "differs from COUNT: ", spec$denom, " for this metric. Every window ",
             "is ranked against the full-season league, so a short one reads as ",
             "what happened over it rather than as a projection."),
    no_reference = paste0("No league reference at any grain for this pitch type ",
                          "and pitcher hand, so no percentile is shown. The ",
                          "pitcher's own sample is not the limit here."))

  list(
    state       = state,
    # IVB and HB shade by distance from the league mean. A value that can't be
    # ranked (no swings, say) stays white.
    fill        = if (!sty$filled) PCTILE_UNFILLED else {
                    p_fill <- if (metric %in% MAGNITUDE_METRICS) {
                      shape_delta_pctile(value, hit$mean)
                    } else hit$pctile
                    if (isTRUE(is.finite(p_fill)))
                      pctile_fill(p_fill, metric_direction(metric, pitch_type))
                    else PCTILE_UNFILLED
                  },
    text_color  = sty$text_color,
    font_style  = sty$font_style,
    font_weight = sty$font_weight,
    marker      = marker,
    n           = if (isTRUE(is.finite(n_own))) as.integer(n_own) else NA_integer_,
    denom       = spec$denom,
    floor       = spec$floor,
    pctile      = hit$pctile,
    n_pitchers  = hit$n_pitchers,
    grain       = if (identical(state, "fallback")) hit$grain else NA_character_,
    state_note  = state_note,
    metric_note = if (metric %in% names(METRIC_NOTES)) unname(METRIC_NOTES[[metric]]) else NA_character_,
    # For tests: did a league reference exist
    has_ref     = isTRUE(is.finite(hit$pctile))
  )
}


#' Resolve one column, one row per pitch type
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


# ---- Whole tables ------------------------------------------------------------

#' Resolve a whole table: cell styles, source notes and column footnotes.
#' `denoms` is matched to `tbl` by pitch type.
resolve_table <- function(tbl, denoms, ref, p_throws, stand, count_bucket = "All Counts") {
  cols <- ARSENAL_METRIC_COLS[names(ARSENAL_METRIC_COLS) %in% names(tbl)]
  pt   <- as.character(tbl$pitch_type)

  idx <- match(pt, as.character(denoms$pitch_type))
  stopifnot("every pitch type in the table needs a denominator row" = !anyNA(idx))
  dn <- as.data.frame(denoms)[idx, intersect(DENOM_COLS, names(denoms)), drop = FALSE]

  # HB is displayed raw but ranked arm side positive: mirror it for lefties
  rank_value <- function(metric, v) {
    if (identical(metric, "hb")) v * arm_side_sign(p_throws) else v
  }

  cells <- do.call(rbind, lapply(seq_along(cols), function(k) {
    col <- names(cols)[k]
    out <- resolve_column(ref, rank_value(unname(cols[k]), tbl[[col]]),
                          unname(cols[k]), pt,
                          p_throws, stand, count_bucket, dn)
    cbind(column = col, metric = unname(cols[k]), row = seq_along(pt),
          out, stringsAsFactors = FALSE)
  }))

  list(cells = cells, notes = state_notes(cells), col_notes = col_notes_for(cols))
}


#' Source notes in a fixed order: scope, double dagger, small sample
state_notes <- function(cells) {
  notes <- character(0)
  if (any(cells$state == "no_reference")) {
    notes <- c(notes, paste0(
      "‡ No league reference exists at any grain for this pitch type and ",
      "pitcher hand, so no percentile is shown. The pitcher's own sample is not ",
      "the limit here."))
  }
  if (any(cells$state == "below_floor")) {
    notes <- c(notes, paste0(
      "A number in parentheses marks a small sample and gives that value's own ",
      "denominator, and the shading still compares it against the league. A cell ",
      "is left unshaded only when there is no value to rank, such as a pitch type ",
      "nobody swung at."))
  }

  notes <- c(paste0(
    "Percentiles rank this pitcher against other pitchers throwing the same ",
    "pitch type from the same side, not against all pitches."), notes)

  notes
}


#' Column-label footnotes, for metrics whose reading needs a caveat
col_notes_for <- function(cols) {
  cn <- vapply(unname(cols),
               function(m) if (m %in% names(METRIC_NOTES)) unname(METRIC_NOTES[[m]]) else NA_character_,
               character(1))
  names(cn) <- names(cols)
  cn[!is.na(cn)]
}


