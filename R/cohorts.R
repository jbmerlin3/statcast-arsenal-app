# cohorts.R
#
# Peer groups defined by RELEASE characteristics, and a pitch's shape measured
# against them. Pure functions over frames, no Shiny.
#
# ---- What a cohort is and is not ---------------------------------------------
#
# A cohort is a symmetric window around the target in one or more release
# dimensions, restricted to the same hand and the same pitch type. It is a KNN
# style neighbourhood rather than a fitted model, and that is a deliberate
# choice: the app can name the members, and a named comparable is what makes the
# number usable to a scout. A regression coefficient cannot be checked against
# a scouting memory, and a list of six pitchers can.
#
# Tolerances are arguments with defaults, never hardcoded, because the right
# window depends on which dimension you are matching and how many pitchers are
# in the league that year.
#
# ---- The velocity confound ---------------------------------------------------
#
# Comparing IVB across a release-height cohort is contaminated, because harder
# throwers tend to be taller and taller pitchers release higher. So a
# release-height cohort is partly a velocity cohort, and any raw IVB delta
# against it double counts. cohort_delta() therefore fits the metric on the
# control variables WITHIN the cohort and compares the target to its own
# prediction, with `control_for = NULL` available to see the unadjusted number.


# ---- Why there are two matching modes ----------------------------------------
#
# Fixed windows make cohorts that are not comparable to each other. Measured on
# Headrick's four-seam: a +/-0.15 ft release-height window returned 22 peers and
# a +/-5 degree arm-angle window returned 61. The arm-angle group is nearly
# three times looser, so its mean sits closer to league average, and the gap
# between the two baselines' deltas is partly a statement about how wide the
# windows happen to be rather than about the pitcher.
#
# Reading down the baseline column is the whole point of the table, so the rungs
# have to be the same size to be read against each other. Fixed-K makes them so:
# every baseline takes the K nearest peers in its own matching dimension, and
# the only thing that changes between rows is WHICH dimension defines "near".
#
# Fixed window is kept because it answers a different question, and a legitimate
# one: "who is actually within a tenth of a foot of this release point", where
# the count itself is the answer and a forced 25 would hide it.
#
# Default half-widths for window mode. Each is roughly a third of the league SD
# on that axis, so a cohort is tight enough to mean something and wide enough to
# populate.
COHORT_TOLERANCES <- list(
  release_height = 0.15,   # ft
  arm_angle      = 5.0,    # degrees
  extension      = 0.20,   # ft
  release_side   = 0.25    # ft
)

# Which profile column each match_on name reads. Named so a typo in match_on
# fails here with the valid list rather than silently matching nobody.
COHORT_COLS <- c(release_height = "rel_z", arm_angle = "arm",
                 extension = "ext", release_side = "rel_side")

# Cohort members below this are reported, never silently widened. Ties to
# MIN_REF_PITCHERS in theme.R so the Context tab and the Characteristics
# percentiles agree on what counts as a quotable peer group.
COHORT_MIN_N <- 20

# Peers per baseline in fixed-K mode. 25 rather than 20 so a cohort clears
# COHORT_MIN_N with room, and rather than 50 because the windows that produce
# roughly 25 are the ones that still look like a peer group on the scatter.
COHORT_K <- 25L


#' Shape per pitcher per pitch type
#'
#' The unit a cohort compares. Separate from pitcher_release_profile() because
#' release point is a pitcher trait and shape is a pitch trait, and conflating
#' them is how a cohort ends up matching on a four-seam's release and scoring a
#' curveball's break.
#' Shape AND outcome per pitcher per pitch type
#'
#' Delegates to search_aggregate(), which is the Search tab's own aggregator,
#' rather than computing a second set of rates beside it. Two reasons, and the
#' second is the one that matters:
#'
#'   whiff%, chase% and xwOBA have definitions that were argued over and
#'   verified against Savant (foul tips are whiffs, bunt attempts are swings, a
#'   bunt attempt out of the zone is a chase). A second implementation here
#'   would drift from those the first time one of them was corrected.
#'
#'   It already carries the denominators (swings, oz, pa) that the sample floors
#'   in METRIC_SPEC are applied to, so the Context tab can grey a thin rate with
#'   the same rule and the same numbers as everywhere else in the app.
#'
#' It also handles the traded-pitcher case correctly, and for the same reason
#' documented there: pitch_team is filtered on, never grouped by. Until
#' 2026-09-12 this file grouped by it and a traded pitcher appeared once per
#' club. 148 duplicated pitcher-by-pitch-type combinations across 63 pitchers,
#' the LHP four-seam league baseline holding 172 rows for 160 men, and
#' Headrick's 25-peer cohort holding 21.
#'
#' `hand` is the BATTER side, matching the global selector, so the outcome rates
#' describe the split on screen. Release point is deliberately not split this
#' way: it is a property of the pitcher, and pitcher_release_profile() takes the
#' whole window.
pitch_shape <- function(df, hand = "All", min_pitches = 50) {
  out <- search_aggregate(df, hand = hand)
  # comparables_gt() prints a team. search_aggregate() joins every club he threw
  # this pitch for inside the window with a slash, which is the honest label for
  # a row that averages both halves of a trade.
  out$pitch_team <- out$team
  out[out$pitches >= min_pitches, , drop = FALSE]
}


#' The peer group for one pitcher's one pitch type
#'
#' Returns the MEMBER LIST with ids and names, not summary stats, so the caller
#' can show comparables. The target is excluded from its own cohort: leaving it
#' in shrinks every delta toward zero by 1/n, which is small but is a bias in a
#' known direction and costs nothing to remove.
#'
#' `match_on` may name any of COHORT_COLS, in any combination. Matching on
#' nothing returns the whole same-hand, same-type league, which is the "League"
#' baseline and is a legitimate call rather than an error.
#'
#' `distance` is the euclidean distance in TOLERANCE UNITS, so one tolerance
#' width on each axis is distance 1 whatever the axes are. That makes the
#' comparables sort meaningful across different match_on choices.
#' `mode` is "fixed_k" or "window". In fixed_k the matching dimensions are
#' standardised over the same-hand pool and the K nearest peers are taken, so
#' every baseline returns a cohort of the same size and the baselines can be
#' read against one another. In window the tolerances apply as literal
#' half-widths and the returned N is itself informative.
#'
#' `distance` carries different units per mode and says so in the returned
#' object: tolerance widths in window mode, pooled SDs in fixed_k. Both are
#' euclidean over the matched axes, so nearest-first means the same thing.
build_cohort <- function(shape, profile, target_id, pitch_type,
                         match_on = character(0),
                         tolerances = COHORT_TOLERANCES,
                         min_pitches = 50,
                         mode = c("fixed_k", "window"),
                         k = COHORT_K) {
  mode <- match.arg(mode)
  bad <- setdiff(match_on, names(COHORT_COLS))
  if (length(bad)) {
    stop("unknown match_on: ", paste(bad, collapse = ", "),
         ". Valid: ", paste(names(COHORT_COLS), collapse = ", "), call. = FALSE)
  }

  tgt_shape <- shape[shape$pitcher == target_id &
                     as.character(shape$pitch_type) == pitch_type, , drop = FALSE]
  if (!nrow(tgt_shape)) return(NULL)
  tgt_prof <- profile[profile$pitcher == target_id, , drop = FALSE]
  if (!nrow(tgt_prof)) return(NULL)

  hand <- tgt_shape$p_throws[1]
  pool <- merge(shape[as.character(shape$pitch_type) == pitch_type &
                      shape$p_throws == hand &
                      shape$pitches >= min_pitches, , drop = FALSE],
                profile[, c("pitcher", unname(COHORT_COLS), "height_in", "n_arm")],
                by = "pitcher")
  pool <- pool[pool$pitcher != target_id, , drop = FALSE]
  if (!nrow(pool)) return(NULL)

  keep <- rep(TRUE, nrow(pool))
  d2   <- rep(0, nrow(pool))
  for (ax in match_on) {
    col <- COHORT_COLS[[ax]]
    tv  <- tgt_prof[[col]][1]
    # A target with no value on a matching axis cannot have a cohort on it. That
    # is the arm-angle NA case over a short window, and it must return nothing
    # rather than a cohort built by treating NA as a match.
    if (!is.finite(tv)) return(NULL)
    dv <- abs(pool[[col]] - tv)

    if (identical(mode, "window")) {
      tol <- tolerances[[ax]]
      if (is.null(tol)) stop("no tolerance supplied for ", ax, call. = FALSE)
      keep <- keep & is.finite(dv) & dv <= tol
      d2   <- d2 + (dv / tol)^2
    } else {
      # Standardised over the same-hand POOL, not over both hands and not over
      # the members that survive. Scaling on the survivors would make the unit
      # depend on the answer.
      sdv <- stats::sd(pool[[col]], na.rm = TRUE)
      if (!is.finite(sdv) || sdv <= 0) return(NULL)
      keep <- keep & is.finite(dv)
      d2   <- d2 + (dv / sdv)^2
    }
  }

  out <- pool[keep, , drop = FALSE]
  if (!nrow(out)) {
    out$distance <- numeric(0)
  } else {
    out$distance <- sqrt(d2[keep])
    out <- out[order(out$distance), , drop = FALSE]
    # K applies only where a dimension defines the cohort. The League baseline
    # matches on nothing, so every same-hand peer is equally "near" and taking
    # 25 of them would be taking an arbitrary 25. It stays the whole league,
    # which is what the row is for.
    if (identical(mode, "fixed_k") && length(match_on)) {
      out <- utils::head(out, k)
    }
  }
  list(target = tgt_shape, target_profile = tgt_prof, members = out,
       match_on = match_on, hand = hand, mode = mode, k = k,
       tolerances = if (identical(mode, "window")) tolerances[match_on] else NULL,
       distance_units = if (identical(mode, "window")) "tolerance widths" else "pooled SDs")
}


#' Target minus cohort, for each metric
#'
#' Returns the cohort N, the cohort mean, the target value, the delta, the
#' cohort SD, and the target's percentile within the cohort.
#'
#' `control_for` names columns to adjust the metric on within the cohort. The
#' delta then becomes the target's residual from a fit of the metric on those
#' controls, which is the thing to report when the cohort axis is correlated
#' with the control: see the velocity note at the top of this file. Pass NULL
#' to get the raw difference of means.
#'
#' Percentile is suppressed below `pctile_min_n` rather than shown small.
#' A percentile off nine members has a resolution of eleven points and reads as
#' precision it does not have. The N is always returned so the caller can show
#' it and decide.
cohort_delta <- function(cohort, metrics = c("ivb", "hb", "velo"),
                         control_for = "velo", pctile_min_n = 10) {
  if (is.null(cohort) || !nrow(cohort$members)) return(NULL)
  mem <- cohort$members
  tgt <- cohort$target
  n   <- nrow(mem)

  do.call(rbind, lapply(metrics, function(m) {
    tv <- tgt[[m]][1]
    cv <- mem[[m]]
    ok <- is.finite(cv)
    # Never control a metric on itself. Asking for velo adjusted for velo is a
    # perfect fit and a zero delta, which would look like a finding.
    ctrl <- setdiff(control_for, m)
    ctrl <- ctrl[vapply(ctrl, function(c) c %in% names(mem) &&
                          sum(is.finite(mem[[c]])) > 3, logical(1))]

    adjusted <- FALSE
    # The denominator z is divided by. Raw cohort SD when the delta is a plain
    # difference of means; the within-cohort fit's RESIDUAL SD when it is not.
    #
    # This is the whole point of the column. An adjusted delta is a residual
    # from a fit, so the spread it should be read against is the spread the fit
    # leaves behind, not the spread it started with. Dividing an adjusted delta
    # by the raw SD makes z depend on how much velocity variation happened to be
    # in that particular cohort, which is exactly the comparison across
    # baselines the column exists to support.
    z_sd <- stats::sd(cv, na.rm = TRUE)
    if (length(ctrl) && sum(ok) > length(ctrl) + 2 && is.finite(tv) &&
        all(vapply(ctrl, function(c) is.finite(tgt[[c]][1]), logical(1)))) {
      f  <- stats::as.formula(paste(m, "~", paste(ctrl, collapse = " + ")))
      fit <- try(stats::lm(f, data = mem[ok, , drop = FALSE]), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        pred  <- as.numeric(stats::predict(fit, newdata = tgt[1, , drop = FALSE]))
        delta <- tv - pred
        sg    <- suppressWarnings(stats::sigma(fit))
        if (is.finite(sg) && sg > 0) z_sd <- sg
        adjusted <- TRUE
      }
    }
    if (!adjusted) delta <- tv - mean(cv, na.rm = TRUE)

    # The target's own denominator for this metric, so a renderer can apply the
    # same sample floor the rest of the app applies. NA for a metric whose
    # denominator this frame does not carry, which is the mean metrics.
    sp    <- METRIC_SPEC[METRIC_SPEC$metric == m, ]
    dcol  <- if (nrow(sp) == 1) sp$denom else NA_character_
    tden  <- if (!is.na(dcol) && dcol %in% names(tgt)) tgt[[dcol]][1] else NA_real_
    tfloor <- if (nrow(sp) == 1) sp$floor else NA_real_

    data.frame(
      metric     = m,
      cohort_n   = n,
      cohort_mean = mean(cv, na.rm = TRUE),
      cohort_sd  = stats::sd(cv, na.rm = TRUE),
      z_sd       = z_sd,
      target     = tv,
      delta      = delta,
      z          = if (is.finite(z_sd) && z_sd > 0) delta / z_sd else NA_real_,
      adjusted   = adjusted,
      controls   = if (adjusted) paste(ctrl, collapse = "+") else NA_character_,
      target_denom = as.numeric(tden),
      denom_floor  = as.numeric(tfloor),
      pctile     = if (n >= pctile_min_n && is.finite(tv))
                     round(100 * mean(cv[ok] <= tv)) else NA_real_,
      stringsAsFactors = FALSE)
  }))
}


#' The nested baselines, as one frame
#'
#' League, release-height matched, arm-angle matched. This is the shape of the
#' top block on the Context tab, and the ordering is the argument: each row
#' conditions on more than the one above it, so reading down the delta column
#' shows what the conditioning is doing.
#'
#' A fourth rung matching on BOTH axes existed and was dropped 2026-09-12. In
#' window mode it returned 9 peers for Headrick, which is under COHORT_MIN_N and
#' rendered greyed with its percentile withheld, so it occupied a row and
#' carried no number anybody could use. Fixed-K would give it 25, but those 25
#' are drawn from a neighbourhood so tight that the last few are not really
#' peers on either axis. Two honest rungs beat three where one is decorative.
#'
#' A baseline whose cohort is empty or below min_n still returns a row carrying
#' its N. Dropping it would hide the most informative case, which is a pitcher
#' so unusual that no peer group exists.
nested_baselines <- function(shape, profile, target_id, pitch_type,
                           metrics = c("ivb", "hb", "velo"),
                           control_for = "velo",
                           tolerances = COHORT_TOLERANCES,
                           min_n = COHORT_MIN_N,
                           mode = c("fixed_k", "window"),
                           k = COHORT_K) {
  mode <- match.arg(mode)
  specs <- list(
    list(label = "League",               match_on = character(0)),
    list(label = "Release-height match", match_on = "release_height"),
    list(label = "Arm-angle match",      match_on = "arm_angle")
  )
  ids <- list()
  out <- do.call(rbind, lapply(specs, function(s) {
    co <- build_cohort(shape, profile, target_id, pitch_type,
                       match_on = s$match_on, tolerances = tolerances,
                       mode = mode, k = k)
    ids[[s$label]] <<- if (is.null(co)) integer(0) else co$members$pitcher
    d  <- cohort_delta(co, metrics = metrics, control_for = control_for)
    if (is.null(d)) {
      return(data.frame(baseline = s$label, metric = metrics, cohort_n = 0L,
                        cohort_mean = NA_real_, cohort_sd = NA_real_, z_sd = NA_real_,
                        target = NA_real_, delta = NA_real_, z = NA_real_,
                        adjusted = NA, controls = NA_character_,
                        target_denom = NA_real_, denom_floor = NA_real_,
                        pctile = NA_real_, below_min_n = TRUE,
                        stringsAsFactors = FALSE))
    }
    d$baseline    <- s$label
    d$below_min_n <- d$cohort_n < min_n
    d[, c("baseline", setdiff(names(d), "baseline"))]
  }))
  # Membership rides along so the renderer can say how far the baselines
  # actually overlap. Two rows built from 20 of the same 25 men are not two
  # peer groups, and the table cannot show that from its Ns alone.
  attr(out, "member_ids") <- ids
  attr(out, "mode") <- mode
  attr(out, "k") <- k
  out
}


#' Pairwise membership overlap between baselines
#'
#' League is excluded: it contains everybody by construction, so its overlap
#' with a matched baseline is just that baseline's N and says nothing.
baseline_overlap <- function(fb) {
  ids <- attr(fb, "member_ids")
  if (is.null(ids)) return(NULL)
  ids <- ids[setdiff(names(ids), "League")]
  if (length(ids) < 2) return(NULL)
  cb <- utils::combn(names(ids), 2, simplify = FALSE)
  do.call(rbind, lapply(cb, function(pr) data.frame(
    a = pr[1], b = pr[2],
    n_a = length(ids[[pr[1]]]), n_b = length(ids[[pr[2]]]),
    shared = length(intersect(ids[[pr[1]]], ids[[pr[2]]])),
    stringsAsFactors = FALSE)))
}
