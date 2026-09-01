# build_league_ref.R
#
# Step 2 of the daily chain. Precomputes the league context the app compares
# each pitcher against, so no league-wide scan ever runs inside a reactive.
#
# Requires R/theme.R for METRIC_SPEC, COUNT_BUCKETS, and the description sets.
#
# Reference period is the full season the store holds, opening day through its
# max date, not the window the user happens to be viewing. Measured on 2026, the
# league distribution barely moves within a season: splitting at mid-June, the
# whiff% and CSW% medians shift about 1% of the p25-p75 spread and velocity
# about 5%. So a season-long yardstick is valid against any window, and a
# window-matched reference could not be precomputed anyway.

library(dplyr)
library(tidyr)


#' Per-pitcher values for one count bucket
#'
#' CLAUDE.md hard rule 1: pitcher level first, then across pitchers. Never pool
#' pitch by pitch across the league, which would weight a 3000-pitch starter the
#' same as thirty relievers.
#'
#' `by_stand = FALSE` produces the coarser cells the fallback ladder needs, with
#' both batter sides pooled.
pitcher_cells <- function(d, by_stand) {
  grain <- c("pitch_type", "p_throws", if (by_stand) "stand")

  cells <- d |>
    group_by(across(all_of(c("pitcher", grain)))) |>
    summarise(
      # Denominators. Each rate divides by its own, never by pitch count.
      pitches = n(),
      swings  = sum(description %in% swing_only),
      oz      = sum(in_zone == 0),
      pa      = sum(woba_denom, na.rm = TRUE),

      velo = mean(release_speed, na.rm = TRUE),
      ivb  = mean(ivb, na.rm = TRUE),
      # Arm-side normalised, matching arsenal_table(). The reference and the
      # value it contextualises have to be on the same scale or the percentile
      # is meaningless, and this is the surface where HB gets compared. The
      # movement chart converts back before it draws. See arm_side_sign().
      hb   = mean(hb, na.rm = TRUE) * arm_side_sign(p_throws[1]),
      spin = mean(release_spin_rate, na.rm = TRUE),
      ext  = mean(release_extension, na.rm = TRUE),

      # Release traits, added 2026-08-31 with the Characteristics tab split.
      #
      # vaa is derived in add_pitch_features() and arrives as a column, so this
      # is a plain mean like every other line here. Raw, not residualised: see
      # the note on the derivation in features.R.
      vaa = mean(vaa, na.rm = TRUE),

      rel_ht = mean(release_pos_z, na.rm = TRUE),

      # Negated but NOT mirrored, unlike hb above. See the long note in
      # tables.R: hb's sign varies within a hand and release side's does not, so
      # mirroring release side deletes handedness and buys nothing.
      #
      # Nothing is lost by keeping the hands on opposite signs, because every
      # cell in this reference is already keyed by p_throws: a righty is only
      # ever ranked against righties, so the fold in pctile_fill() measures
      # distance from the RIGHT-HANDED median either way. Reference and table
      # must apply this identically or the percentile reads the wrong tail.
      rel_side = -mean(release_pos_x, na.rm = TRUE),

      strike_pct = mean(type %in% c("S", "X"), na.rm = TRUE) * 100,
      csw_pct    = mean(description %in% c("called_strike", whiff_desc)) * 100,
      zone_pct   = mean(in_zone, na.rm = TRUE) * 100,
      whiff_pct  = sum(description %in% whiff_desc) / sum(description %in% swing_only) * 100,
      chase_pct  = sum(in_zone == 0 & description %in% swing_only) / sum(in_zone == 0) * 100,
      xwoba      = sum(estimated_woba_using_speedangle * woba_denom, na.rm = TRUE) /
                     sum(woba_denom, na.rm = TRUE),
      .groups = "drop"
    )

  # Usage is the share of a pitcher's own pitches in this cut, so its
  # denominator is his total rather than the pitch type's.
  cells |>
    group_by(across(all_of(c("pitcher", setdiff(grain, "pitch_type"))))) |>
    # cut_pitches is usage_pct's denominator: the pitcher's total in this cut,
    # which is what the share is measured over. See METRIC_SPEC in theme.R.
    mutate(cut_pitches = sum(pitches),
           usage_pct = pitches / cut_pitches * 100) |>
    ungroup()
}


#' Collapse per-pitcher values into one reference row per cell and metric
#'
#' Stores the mean plus percentiles at 1 point increments, so the app answers a
#' lookup with findInterval() instead of rescanning the season.
summarise_cells <- function(cells, grain) {
  long <- cells |>
    pivot_longer(all_of(METRIC_SPEC$metric), names_to = "metric", values_to = "value") |>
    left_join(METRIC_SPEC, by = "metric") |>
    # Each metric is filtered on its own denominator. A slider with 200 pitches
    # but 30 swings clears the velocity floor and fails the whiff floor, which
    # is the intended behaviour.
    mutate(denom_n = case_when(denom == "cut_pitches" ~ cut_pitches,
                               denom == "pitches" ~ pitches,
                               denom == "swings"  ~ swings,
                               denom == "oz"      ~ oz,
                               denom == "pa"      ~ pa)) |>
    filter(is.finite(value), denom_n >= floor)

  long |>
    group_by(across(all_of(c(grain, "metric")))) |>
    summarise(
      n_pitchers = n(),
      mean       = mean(value),
      # 101 breakpoints, 0 to 100 inclusive.
      q          = list(unname(quantile(value, probs = seq(0, 1, 0.01), na.rm = TRUE))),
      .groups    = "drop"
    )
}


#' Build the full reference, all grains the fallback ladder can reach
#'
#' Three levels. `stand` carries the literal batter side at the fine level and
#' "All" at the coarse one, so a single table serves every rung of the ladder.
#'
#' p_throws is present at every level and is never dropped. hb is not
#' handedness-normalized, `hb = -pfx_x * 12`, so median hb has the opposite sign
#' for lefties and righties in every pitch type. Pooling the hands makes the
#' reference bimodal with almost nothing at its own centre, which no sample
#' floor would catch.
#' The reference has to be keyed by the codes the app will look it up WITH,
#' which are the reconciled ones. This used to filter raw codes against
#' pitch_colors instead, and the two disagreed in both directions: a CS was
#' remapped to CU at query time and then ranked against a CU population that had
#' been built with every CS excluded, and a KC now maps to CU but would have
#' been pooled under its own key. Same function the app calls, so the population
#' a pitch is ranked against always contains pitches like it.
build_league_ref <- function(app_data) {
  d0 <- app_data |>
    filter(!is.na(pitch_type), pitch_type != "") |>
    reconcile_pitch_codes() |>
    mutate(cnt = paste(balls, strikes, sep = "-"))

  out <- lapply(names(COUNT_BUCKETS), function(bk) {
    counts <- COUNT_BUCKETS[[bk]]
    d <- if (is.null(counts)) d0 else filter(d0, cnt %in% counts)

    fine <- summarise_cells(pitcher_cells(d, by_stand = TRUE),
                            c("pitch_type", "p_throws", "stand"))
    coarse <- summarise_cells(pitcher_cells(d, by_stand = FALSE),
                              c("pitch_type", "p_throws")) |>
      mutate(stand = "All")

    bind_rows(fine, coarse) |> mutate(count_bucket = bk)
  })

  bind_rows(out) |>
    select(pitch_type, p_throws, stand, count_bucket, metric, n_pitchers, mean, q) |>
    arrange(pitch_type, p_throws, stand, count_bucket, metric)
}
