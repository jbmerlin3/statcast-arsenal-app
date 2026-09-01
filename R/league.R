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
  # `mean` rides along because the shape metrics colour by distance from it
  # rather than by rank. It comes from the cell the LADDER selected, not from a
  # second lookup, or the fill could end up measured against a different
  # population than the percentile beside it.
  miss <- list(pctile = NA_real_, n_pitchers = NA_integer_, grain = NA_character_,
               exact = NA, mean = NA_real_)
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
       exact      = identical(hit$grain, LADDER[[1]]$grain),
       mean       = hit$row$mean[[1]])
}


#' Where a shape sits, as a distance rather than as a rank
#'
#' Returns a pseudo-percentile so the rest of the pipeline is unchanged: 50 is
#' the league average for that pitch type, 0 and 100 are a full span below and
#' above it. pctile_fill() then inverts for a pitch that wants the low end, so a
#' changeup with extra drop reddens exactly as a four-seam with extra ride does.
#'
#' Clamped rather than allowed to run off: 5% of real cells sit beyond the span,
#' and they should read as extreme rather than as an error.
shape_delta_pctile <- function(value, league_mean, span = SHAPE_DELTA_SPAN) {
  if (!is.finite(value) || !is.finite(league_mean) || !is.finite(span) || span <= 0) {
    return(NA_real_)
  }
  50 + 50 * max(-1, min(1, (value - league_mean) / span))
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
#' "extreme" FOLDS the percentile instead of inverting it: 2 * |p - 50|, which is
#' the share of the league sitting closer to the median than this pitcher does.
#' Added 2026-08-31 for the release traits, where the question is not whether a
#' number is high but whether it is unusual. A 96th-percentile release height and
#' a 4th both fold to 92 and fill identically, which is the intent.
#'
#' Folding a rank rather than measuring a distance is the choice worth naming
#' here, because MAGNITUDE_METRICS exists for precisely the opposite call on ivb
#' and hb. It goes the other way for release because the argument that forced
#' magnitude there does not hold: a cutter's IVB spread is genuinely tighter than
#' a curveball's, so a rank means different things across those rows, while
#' release height is the same slot measured the same way whatever the pitch is
#' labelled. With the spread stable across rows a rank IS comparable, and it has
#' the better sentence attached: "farther from average than 92% of the league"
#' needs no span constant to be tuned and no units to be explained.
#'
#' Note what folding costs, since the palette cannot say it: direction is gone.
#' Only the raw value printed in the cell tells the reader which tail they are
#' looking at, so an extreme metric must never be rendered without its number.
#'
#' Vectorised over `pctile`, since a metric has one direction and a column has
#' many values. NA in, NA out.
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
    # Two things differ from a plain percentile fill here.
    #
    # metric_direction(), not spec$direction: IVB and HB run one way for a
    # four-seam and the other for a changeup, so direction is a property of the
    # pitch shape rather than of the metric.
    #
    # And those same two colour by DISTANCE from the league average rather than
    # by rank, because a rank is not comparable across rows: 90th percentile is
    # one inch in a cutter's tight IVB spread and four in a curveball's. `pctile`
    # below still reports the true rank, so a cell can read 90th and look pale,
    # which is the honest reading of leading a tight group by very little.
    fill        = if (!sty$filled) PCTILE_UNFILLED else {
                    p_fill <- if (metric %in% MAGNITUDE_METRICS) {
                      shape_delta_pctile(value, hit$mean)
                    } else hit$pctile
                    pctile_fill(p_fill, metric_direction(metric, pitch_type))
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


# ---- One table, one call -----------------------------------------------------

#' Resolve a whole arsenal table: per-cell styling plus the table's note lines
#'
#' The grain tables.R renders at. resolve_column() would leave the renderers
#' holding ten note vectors and doing the union, dedupe and ordering itself,
#' which is the same aggregation this pair exists to keep off the table side,
#' reappearing one level up.
#'
#' Returns three things and nothing else is needed to render:
#'   cells     one row per (column, pitch type), carrying fill, text colour,
#'             font style and weight, and the marker to append
#'   notes     ordered, deduplicated, marker-prefixed source-note lines
#'   col_notes named by table column, for metrics whose reading needs a caveat
#'
#' Order is fixed rather than incidental: the scope note, then dagger, then
#' double dagger, then the grey line. A set that reordered between renders would make the byte-identity
#' baseline flake.
#'
#' `denoms` is matched to `tbl` by pitch type rather than by position, so the
#' two frames cannot silently drift out of alignment.
resolve_table <- function(tbl, denoms, ref, p_throws, stand, count_bucket = "All Counts") {
  cols <- ARSENAL_METRIC_COLS[names(ARSENAL_METRIC_COLS) %in% names(tbl)]
  pt   <- as.character(tbl$pitch_type)

  idx <- match(pt, as.character(denoms$pitch_type))
  stopifnot("every pitch type in the table needs a denominator row" = !anyNA(idx))
  dn <- as.data.frame(denoms)[idx, intersect(DENOM_COLS, names(denoms)), drop = FALSE]

  cells <- do.call(rbind, lapply(seq_along(cols), function(k) {
    col <- names(cols)[k]
    out <- resolve_column(ref, tbl[[col]], unname(cols[k]), pt,
                          p_throws, stand, count_bucket, dn)
    cbind(column = col, metric = unname(cols[k]), row = seq_along(pt),
          out, stringsAsFactors = FALSE)
  }))

  list(cells = cells, notes = state_notes(cells), col_notes = col_notes_for(cols))
}


#' The table's source-note lines, ordered and deduplicated
#'
#' Shared by the arsenal table and the usage table so the four states are
#' described in one voice and, more to the point, decided in one place. A second
#' copy of this is a second state machine.
#'
#' Order is fixed rather than incidental: the scope note, then dagger, then
#' double dagger, then the grey line. A set that reordered between renders would
#' make the byte-identity baselines flake.
state_notes <- function(cells) {
  fb <- cells[cells$state == "fallback", , drop = FALSE]

  # One dagger line whatever the number of grains, because two lines under one
  # marker leave the reader unable to tell which applies to a given cell. When
  # more than one rung answered, the line enumerates them.
  #
  # The arsenal table only ever produces one grain, measured across 60 pitchers.
  # The usage table produces two, and that is structural rather than incidental:
  # its columns each carry a different count bucket, and the ladder's second and
  # third rungs differ by bucket, so different columns land on different rungs.
  # An assertion written for the arsenal table caught this on the usage table
  # the first time it ran.
  #
  # Ordered by the ladder, finest first, so the line does not reorder between
  # renders. An unrecognised grain still stops, since it means the ladder grew a
  # rung this has not been thought about against.
  ladder_grains <- vapply(LADDER, function(r) r$grain, character(1))
  grains <- unique(fb$grain)
  unknown <- setdiff(grains, ladder_grains)
  if (length(unknown)) {
    stop("fallback grain not on the ladder: ", paste(unknown, collapse = " | "),
         call. = FALSE)
  }
  grains <- ladder_grains[ladder_grains %in% grains]

  notes <- character(0)
  if (nrow(fb)) {
    parts <- vapply(grains, function(gr) paste0(
      gr, " (at least ", min(fb$n_pitchers[fb$grain == gr]), " pitchers)"),
      character(1))
    notes <- c(notes, paste0(
      "† Percentile from a coarser league cut than the exact one, because that ",
      "cell held fewer than ", MIN_REF_PITCHERS, " pitchers. Cut used: ",
      paste(parts, collapse = "; "), "."))
  }
  if (any(cells$state == "no_reference")) {
    notes <- c(notes, paste0(
      "‡ No league reference exists at any grain for this pitch type and ",
      "pitcher hand, so no percentile is shown. The pitcher's own sample is not ",
      "the limit here."))
  }
  # One line, and it does not enumerate per-metric floors. The floors differ by
  # denominator, so naming them would put several near-identical grey lines under
  # one table. The parenthetical already carries each value's own denominator,
  # which is where the reader needs it.
  if (any(cells$state == "below_floor")) {
    notes <- c(notes, paste0(
      "Grey italic values sit below the sample floor for their metric and are ",
      "not placed against the league. The figure in parentheses is that value's ",
      "own denominator."))
  }

  # Always present when there is any context at all, because the fill implies a
  # comparison and the table never says which one. It does not fix the sinker
  # versus slider reading, see CLAUDE.md, but it stops the page implying a
  # cross-pitch ranking it is not making.
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


#' League mean movement for each pitch type, for the reference marks
#'
#' stand is "All" because the movement chart pools both batter sides, and
#' p_throws is passed through and never pooled: hb is not arm-side normalised,
#' so a righty's and a lefty's sliders sit on opposite sides of zero and their
#' mean is a point neither of them throws. See CLAUDE.md.
#'
#' Returns NULL rows for pitch types with no usable reference rather than
#' inventing one, and carries n_pitchers so every mark can state what it is
#' built from.
movement_ref <- function(ref, pitch_types, p_throws) {
  rows <- lapply(as.character(pitch_types), function(pt) {
    hb  <- lg_cell(ref, "hb",  pt, p_throws, "All", "All Counts")
    ivb <- lg_cell(ref, "ivb", pt, p_throws, "All", "All Counts")
    if (is.null(hb) || is.null(ivb)) return(NULL)
    data.frame(pitch_type = pt,
               # Converted BACK out of arm-side normalisation. league_ref stores
               # HB arm-side positive for both hands, because that is the only
               # way a percentile means the same thing for a righty and a lefty.
               # The movement chart is the one consumer that wants the true
               # direction: a lefty's slider really does sweep the other way,
               # and the league cross has to land where his pitches are. Without
               # this the mark would sit mirrored across the vertical axis for
               # every left-hander.
               hb = hb$row$mean[[1]] * arm_side_sign(p_throws),
               ivb = ivb$row$mean[[1]],
               # Both means come from the same grain and the same contributing
               # pitchers, so one n describes the mark.
               n_pitchers = min(hb$row$n_pitchers[[1]], ivb$row$n_pitchers[[1]]),
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}


#' Resolve the usage-by-count table
#'
#' Same four states, same floors, same fallback ladder as the arsenal table,
#' through the same resolve_column(). The only difference is that the count
#' bucket varies by COLUMN here rather than being fixed for the table, which is
#' exactly what the reference is keyed by.
#'
#' `denoms` is one row per pitch type per bucket, with a `pitches` column.
resolve_usage <- function(wide, denoms, ref, p_throws, stand) {
  buckets <- setdiff(names(wide), "pitch_type")
  pt <- as.character(wide$pitch_type)

  cells <- do.call(rbind, lapply(buckets, function(b) {
    dn <- as.data.frame(denoms[denoms$count_bucket == b, , drop = FALSE])
    idx <- match(pt, dn$pitch_type)
    stopifnot("every pitch type needs a denominator row per bucket" = !anyNA(idx))
    out <- resolve_column(ref, wide[[b]], "usage_pct", pt, p_throws, stand, b,
                          dn[idx, intersect(DENOM_COLS, names(dn)), drop = FALSE])
    cbind(column = b, metric = "usage_pct", row = seq_along(pt), out,
          stringsAsFactors = FALSE)
  }))

  list(cells = cells, notes = state_notes(cells), col_notes = character(0))
}
