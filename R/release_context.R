# release_context.R
#
# Where a pitcher releases the ball, and whether that is unusual for him.
#
# Everything here is a pure function over a frame, like the rest of R/, so it
# runs from the console with no server.
#
# ---- The question this file exists to answer ----------------------------------
#
# The Search tab finds pitchers by SHAPE: "who throws 96 with 17 inches of ride."
# It cannot ask whether a shape is unusual GIVEN where the ball leaves the hand.
# That is a different question and it needs a peer group defined by release
# characteristics rather than by movement.
#
# The load-bearing fact is that arm angle and release height are correlated at
# 0.82 (RHP) and 0.81 (LHP), measured 2026-09-12 over the 699 pitchers with 100+
# pitches. A low slot usually means a low release. Listed height is what breaks
# the link, and the residual from that break is the interesting number: a 6-6
# pitcher at a 40 degree slot releases from well above the 5-11 pitchers who
# share his slot.
#
# Without height in the model the residual just re-measures arm angle.


#' Listed height per MLBAM id
#'
#' Tracked CSV rather than a chain-built rds, for the reason in
#' scripts/build_pitcher_heights.R: height changes a few times a season and the
#' nightly deploy should not depend on StatsAPI being up.
#'
#' Returns pitcher/height_in so it joins straight onto app_data's `pitcher`.
load_pitcher_heights <- function(path = NULL, year = 2026) {
  if (is.null(path)) path <- file.path("lookups", sprintf("pitcher_heights_%d.csv", year))
  if (!file.exists(path)) {
    warning("No height lookup at ", path,
            ". Release-context residuals will be unavailable. ",
            "Build it with: Rscript scripts/build_pitcher_heights.R", call. = FALSE)
    return(NULL)
  }
  h <- utils::read.csv(path, stringsAsFactors = FALSE)
  need <- c("mlbam_id", "height_in")
  if (!all(need %in% names(h))) {
    warning("Height lookup is missing ", paste(setdiff(need, names(h)), collapse = ", "),
            call. = FALSE)
    return(NULL)
  }
  data.frame(pitcher = as.integer(h$mlbam_id), height_in = as.numeric(h$height_in),
             stringsAsFactors = FALSE) |>
    subset(is.finite(height_in) & !duplicated(pitcher))
}


#' One row per pitcher: where he releases from, and how tall he is
#'
#' Release point is a property of the PITCHER, not of a pitch type, so this
#' aggregates over every pitch in the window rather than per type. Arm angle is
#' averaged the same way Savant's own leaderboard does it, which is what
#' scripts/verify_savant.R validates against at MAE 0.13 degrees.
#'
#' `min_pitches` matches the Search tab's existing minimum so the two surfaces
#' agree on who counts as a real pitcher. A league pool built from cameo appearances
#' is a pool of noise.
#'
#' arm_angle carries a real NA band. It is computed by a pose pipeline that lands
#' days after the game, so a short window ending today can be mostly NA even
#' though the season is at 0.84 percent. Measured 2026-09-12: September sat at
#' 11.4 percent NA while May through July were at 0.06 or better. So `n_arm` is
#' returned alongside `arm_angle` and every consumer is expected to show it.
pitcher_release_profile <- function(df, heights = NULL, min_pitches = 100) {
  stopifnot(is.data.frame(df))
  need <- c("pitcher", "player_name", "p_throws", "release_pos_x",
            "release_pos_z", "release_extension", "arm_angle")
  missing <- setdiff(need, names(df))
  if (length(missing)) stop("pitcher_release_profile needs ", paste(missing, collapse = ", "),
                            call. = FALSE)

  out <- df |>
    dplyr::group_by(pitcher, player_name, p_throws) |>
    dplyr::summarise(
      n       = dplyr::n(),
      n_arm   = sum(!is.na(arm_angle)),
      rel_x   = mean(release_pos_x, na.rm = TRUE),
      rel_z   = mean(release_pos_z, na.rm = TRUE),
      ext     = mean(release_extension, na.rm = TRUE),
      arm     = mean(arm_angle, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::filter(n >= min_pitches)

  if (!is.null(heights)) out <- dplyr::left_join(out, heights, by = "pitcher")
  else out$height_in <- NA_real_

  # rel_x is negated for display for the same reason rel_side is in the traits
  # table: Savant measures it from the catcher's view, positive toward first
  # base, which puts a RHP negative. Negated here reads positive toward third,
  # so a righty is positive. A display convention, not a correction, and the
  # rarity maths below is sign-agnostic either way.
  out$rel_side <- -out$rel_x
  out
}


#' Release height above or below what the slot and the body imply
#'
#' Fitted league-wide and SEPARATELY BY HAND, then the residual is returned per
#' pitcher. Fitting the hands together would pool two mirror-image release_pos_x
#' distributions; height and arm angle are hand-neutral but the fit is cheap
#' enough that splitting costs nothing and removes the question.
#'
#' Measured 2026-09-12 on 2026 data through 09-07:
#'   RHP  n=490  adj R2 0.740  residual SD 3.23 in  height coef +0.777 ft/ft
#'   LHP  n=209  adj R2 0.740  residual SD 3.04 in  height coef +0.802 ft/ft
#'
#' ---- Why a BAND and not just a number ----
#'
#' Listed height is self-reported and rounded to the inch: 18 distinct values
#' across 1,455 players. The residual moves by the height coefficient for every
#' inch of error, which is 0.78 in, or 24 to 26 percent of the residual SD. Two
#' inches is about half an SD, which is enough to move a pitcher from
#' unremarkable to notable.
#'
#' Brent Headrick is the worked case: 6-6, 40.2 degree slot, 6.32 ft release,
#' residual +3.3 in. A two-inch misreport puts him at +1.7 or +4.9. Reporting
#' +3.3 alone would overstate what the number can carry, so `resid_lo` and
#' `resid_hi` are first-class outputs and the UI shows them.
#'
#' A fit rather than a nearest-neighbour mean, because a residual needs a
#' smooth expectation and a mean over 20 neighbours is a noisier one.
#' ---- Why low-support arms are FLAGGED and not capped or dropped ----
#'
#' The fit is linear in arm angle, and the arm-angle distribution has thin
#' tails: the 1st percentile is 1.1 degrees and the 99th is 64.7, but Tyler
#' Rogers throws from -60.7. Out there the line is extrapolating, so the
#' residual measures the model's reach rather than the pitcher's release.
#'
#' `arm_support` counts same-hand pitchers within `support_window` degrees, and
#' `low_support` marks those under `support_min`. Measured 2026-09-12 over 699
#' pitchers: 23 of them (3.3%) have fewer than 10 neighbours, and their mean
#' |residual| is 5.9 in against 2.4 in for everyone else. A 2.5x inflation
#' concentrated in 3% of the league is the signature of extrapolation, not of
#' 23 unusually interesting arms.
#'
#' They are flagged rather than capped, because capping would silently delete
#' the genuinely strange release points, which is what the tab is for. A
#' submariner belongs at the top of an unusualness sort; he just should not be
#' read as "releases 13 inches lower than his slot implies" when nobody shares
#' his slot.
#'
#' The threshold catches BOTH tails, which is the point of defining it on
#' support instead of on submarine arm angles: Rogers at -60.7 and Hill at -22
#' are flagged, and so are Vesia at 69.9 and Dion at 66.9.
expected_release_height <- function(profile, height_error_in = 1,
                                    support_window = 5, support_min = 10) {
  usable <- is.finite(profile$rel_z) & is.finite(profile$arm) & is.finite(profile$height_in)
  profile$exp_rel_z  <- NA_real_
  profile$rel_z_resid <- NA_real_
  profile$resid_sd   <- NA_real_
  profile$ht_coef    <- NA_real_

  for (h in unique(profile$p_throws[usable])) {
    idx <- usable & profile$p_throws == h
    # Three coefficients from a two-predictor fit, so a hand with almost nobody
    # in it produces a fit that interpolates its own noise. 30 is a floor, not a
    # sample-size recommendation.
    if (sum(idx) < 30) next
    d  <- data.frame(rel_z = profile$rel_z[idx], arm = profile$arm[idx],
                     ht_ft = profile$height_in[idx] / 12)
    m  <- stats::lm(rel_z ~ arm + ht_ft, data = d)
    profile$exp_rel_z[idx]   <- stats::fitted(m)
    profile$rel_z_resid[idx] <- stats::residuals(m)
    profile$resid_sd[idx]    <- stats::sd(stats::residuals(m))
    profile$ht_coef[idx]     <- unname(stats::coef(m)[["ht_ft"]])
  }

  # The band is the residual shifted by what `height_error_in` inches of listed
  # height would do to the prediction. Symmetric because the rounding is.
  shift <- abs(profile$ht_coef) * (height_error_in / 12)
  profile$resid_lo <- profile$rel_z_resid - shift
  profile$resid_hi <- profile$rel_z_resid + shift

  # Local support, counted within hand and excluding self.
  profile$arm_support <- NA_integer_
  fin <- is.finite(profile$arm)
  for (i in which(fin)) {
    profile$arm_support[i] <- sum(
      fin & profile$p_throws == profile$p_throws[i] &
      abs(profile$arm - profile$arm[i]) <= support_window) - 1L
  }
  profile$low_support     <- !is.na(profile$arm_support) & profile$arm_support < support_min
  profile$support_window  <- support_window
  profile$support_min     <- support_min
  profile
}


#' How many same-hand pitchers release the ball near this one
#'
#' Standardises release side, release height and extension league-wide within
#' hand, then counts neighbours inside `radius` in that 3D space.
#'
#' Returns the COUNT and the RADIUS, deliberately, not a composite rarity score.
#' A single number would have to weight three axes against each other, that
#' weighting would be invented, and nobody reading the page could tell a 7 from
#' a 9. "Four other left-handers release the ball within 0.75 SD of here" is a
#' sentence a scout can check.
#'
#' Scaled within hand rather than across both, because release side is mirrored
#' and pooling it would make every pitcher look unusual against the other hand.
release_rarity <- function(profile, radius = 0.75) {
  profile$rarity_n      <- NA_integer_
  profile$rarity_radius <- radius
  profile$rarity_pool   <- NA_integer_

  for (h in unique(profile$p_throws)) {
    idx <- profile$p_throws == h &
      is.finite(profile$rel_x) & is.finite(profile$rel_z) & is.finite(profile$ext)
    if (sum(idx) < 2) next
    z <- scale(cbind(profile$rel_x[idx], profile$rel_z[idx], profile$ext[idx]))
    dm <- as.matrix(stats::dist(z))
    # Self is distance 0 and must not be counted as its own neighbour.
    diag(dm) <- Inf
    profile$rarity_n[idx]    <- as.integer(rowSums(dm <= radius))
    profile$rarity_pool[idx] <- as.integer(sum(idx))
  }
  profile
}

#' Arm angle as a scout says it
#'
#' The Context tab is read by coaches and front-office staff, not only by
#' analysts, and "38.4 degrees" is a measurement where "three-quarters" is the
#' word the room already uses. Both are shown: the label is what makes the table
#' skimmable, the number is what makes it checkable.
#'
#' Cuts follow conventional usage rather than the data's own quantiles. A
#' data-driven split would move every season and rename a pitcher's slot without
#' his slot changing, which is the one thing a label like this must not do.
ARM_SLOT_CUTS <- c(-Inf, 0, 15, 30, 45, 60, Inf)
ARM_SLOT_LABELS <- c("Submarine", "Sidearm", "Low 3/4", "3/4", "High 3/4", "Over the top")

arm_slot_label <- function(arm) {
  out <- as.character(cut(arm, breaks = ARM_SLOT_CUTS, labels = ARM_SLOT_LABELS,
                          right = FALSE))
  out[!is.finite(arm)] <- NA_character_
  out
}


#' How unusual a release point is, in words
#'
#' Wraps the raw neighbour count from release_rarity() into three buckets, so
#' the table can say "Rare" instead of making the reader decide whether 4 is a
#' small number. The count stays available for anyone who wants it.
rarity_label <- function(n_neighbours) {
  ifelse(is.na(n_neighbours), NA_character_,
  ifelse(n_neighbours <= 2,  "Rare",
  ifelse(n_neighbours <= 10, "Uncommon", "Typical")))
}


# ---- League percentiles, as a sentence ---------------------------------------
#
# The Characteristics tab has read league percentiles since it was built: that
# is what its cell shading IS, via league_ref and lg_pctile(). What no surface
# in this app does is SAY one. A scouting writeup does not say "shaded 0.92 on
# the ramp", it says "he releases the ball lower than 92% of MLB righties", and
# that sentence is the unit a coach or a front office repeats.
#
# So nothing here computes a percentile. It reads the same reference the
# Characteristics fills read, through the same lg_pctile(), and renders the
# number as the comparative a reader would write.
#
# Which end of each metric the comparative describes, and the word for it. Kept
# beside the metrics rather than in METRIC_SPEC because these are phrasings, not
# properties of the measurement: "flatter" is a fact about how people talk about
# approach angle, and METRIC_SPEC has no business holding English.
LEAGUE_PHRASE <- list(
  velo   = list(label = "Velocity",       high = "harder than",      low = "softer than",        digits = 1, unit = " mph"),
  ivb    = list(label = "Ride",           high = "more ride than",   low = "less ride than",     digits = 1, unit = " in"),
  hb     = list(label = "Run",            high = "more run than",    low = "less run than",      digits = 1, unit = " in"),
  vaa    = list(label = "Approach angle", high = "flatter than",     low = "steeper than",       digits = 2, unit = "°"),
  spin   = list(label = "Spin",           high = "more spin than",   low = "less spin than",     digits = 0, unit = " rpm"),
  ext    = list(label = "Extension",      high = "further out than", low = "shorter than",       digits = 2, unit = " ft"),
  rel_ht = list(label = "Release height", high = "higher than",      low = "lower than",         digits = 2, unit = " ft"),
  # Hand-neutral ends on purpose. Positive is toward third base for both hands,
  # so "further out" would mean arm side for a righty and glove side for a
  # lefty. The percentile is within hand, so the comparison is still to arms on
  # his own side.
  rel_side = list(label = "Release side",  high = "further toward third than", low = "further toward first than", digits = 2, unit = " ft"),
  zone_pct  = list(label = "In-zone rate", high = "in the zone more than", low = "in the zone less than", digits = 1, unit = "%"),
  whiff_pct = list(label = "Whiff rate",   high = "more whiffs than", low = "fewer whiffs than", digits = 1, unit = "%"),
  xwoba     = list(label = "xwOBA",        high = "more contact damage than", low = "less contact damage than", digits = 3, unit = "")
)


#' One pitch's traits against the league, as percentiles and comparatives
#'
#' `shape_row` is one row of pitch_shape(), `ref` is league_ref. Returns one row
#' per metric with the percentile, the phrase, and whether the reference cell
#' was the exact cut or a coarser rung of the LADDER.
#'
#' The grain is deliberately the same one the Characteristics tab uses:
#' pitch_type x p_throws x stand x count_bucket. A percentile against "all
#' four-seams" would pool the hands, and hb and rel_side are mirrored between
#' them, so the number would be describing a population nobody pitches in.
#'
#' `exact` rides along per metric and is NOT hidden. A sentence built on a
#' coarser cut is still worth saying, but it is a weaker claim than one built on
#' the exact cell, and the difference has to survive to the page. Same reason
#' the results table carries its dagger.
league_percentiles <- function(shape_row, ref, stand = "All",
                               count_bucket = "All Counts",
                               metrics = names(LEAGUE_PHRASE)) {
  if (is.null(shape_row) || !nrow(shape_row)) return(NULL)
  pt   <- as.character(shape_row$pitch_type[1])
  hand <- as.character(shape_row$p_throws[1])

  rows <- lapply(metrics, function(m) {
    if (!m %in% names(shape_row)) return(NULL)
    v <- shape_row[[m]][1]
    if (!is.finite(v)) return(NULL)
    # hb is mirrored on the way into the lookup and nowhere else, exactly as
    # resolve_table() does it and for the same reason: league_ref stores HB
    # arm-side positive for both hands, while pitch_shape() reports the raw sign
    # the movement chart draws.
    #
    # Caught by reading the output rather than by reasoning about it. Noah
    # Cameron's four-seam at -6.0 in of raw HB was reported as "less run than
    # 100% of LHP four-seams", because -6.0 was being ranked inside a
    # distribution of positive arm-side values and landed under all of them. The
    # number rendered, the sentence parsed, and it was nonsense.
    lookup_v <- if (identical(m, "hb")) v * arm_side_sign(hand) else v
    hit <- lg_pctile(ref, lookup_v, m, pt, hand, stand, count_bucket)
    if (!is.finite(hit$pctile)) return(NULL)
    ph <- LEAGUE_PHRASE[[m]]
    # The comparative always names the side he is ON, so the percentage is the
    # share he beats in that direction and never needs a mental flip. A 12th
    # percentile release height reads "lower than 88%", not "higher than 12%".
    above <- hit$pctile >= 50
    list(metric = m, label = ph$label,
         value = v, digits = ph$digits, unit = ph$unit,
         pctile = hit$pctile,
         share = if (above) hit$pctile else 100 - hit$pctile,
         phrase = if (above) ph$high else ph$low,
         n_pitchers = hit$n_pitchers,
         exact = isTRUE(hit$exact), grain = hit$grain)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  do.call(rbind, lapply(rows, function(r) data.frame(r, stringsAsFactors = FALSE)))
}
