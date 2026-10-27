# build_league_ref.R
# Step 2 of the chain: per-pitcher league distributions for every metric, over
# the full season held.

library(dplyr)
library(tidyr)


#' Per-pitcher values for one count bucket (each pitcher counts once).
#' `by_stand = FALSE` pools batter sides.
pitcher_cells <- function(d, by_stand) {
  grain <- c("pitch_type", "p_throws", if (by_stand) "stand")

  cells <- d |>
    group_by(across(all_of(c("pitcher", grain)))) |>
    summarise(
      # Denominators
      pitches = n(),
      swings  = sum(description %in% swing_only),
      oz      = sum(in_zone == 0),
      pa      = sum(woba_denom, na.rm = TRUE),
      z_swings = sum(in_zone == 1 & description %in% swing_only, na.rm = TRUE),
      bbe     = sum(description == "hit_into_play" & nzchar(bb_type) & !is.na(bb_type)),

      velo = mean(release_speed, na.rm = TRUE),
      ivb  = mean(ivb, na.rm = TRUE),
      # Arm side positive for ranking (see arm_side_sign())
      hb   = mean(hb, na.rm = TRUE) * arm_side_sign(p_throws[1]),
      spin = mean(release_spin_rate, na.rm = TRUE),
      ext  = mean(release_extension, na.rm = TRUE),
      vaa = mean(vaa, na.rm = TRUE),

      rel_ht = mean(release_pos_z, na.rm = TRUE),
      # Positive toward third base, as in arsenal_table()
      rel_side = -mean(release_pos_x, na.rm = TRUE),

      strike_pct = mean(type %in% c("S", "X"), na.rm = TRUE) * 100,
      csw_pct    = mean(description %in% c("called_strike", whiff_desc)) * 100,
      zone_pct   = mean(in_zone, na.rm = TRUE) * 100,
      whiff_pct  = sum(description %in% whiff_desc) / sum(description %in% swing_only) * 100,
      chase_pct  = sum(in_zone == 0 & description %in% swing_only) / sum(in_zone == 0) * 100,
      z_whiff_pct = sum(in_zone == 1 & description %in% whiff_desc, na.rm = TRUE) /
                      sum(in_zone == 1 & description %in% swing_only, na.rm = TRUE) * 100,
      xwoba      = sum(estimated_woba_using_speedangle * woba_denom, na.rm = TRUE) /
                     sum(woba_denom, na.rm = TRUE),
      # Same expressions as arsenal_table()
      gb_pct     = { b <- description == "hit_into_play" & nzchar(bb_type) & !is.na(bb_type)
                     sum(b & bb_type == "ground_ball") / sum(b) * 100 },

      .groups = "drop"
    )

  # usage_pct's denominator is the pitcher's total in this cut
  cells |>
    group_by(across(all_of(c("pitcher", setdiff(grain, "pitch_type"))))) |>
    mutate(cut_pitches = sum(pitches),
           usage_pct = pitches / cut_pitches * 100) |>
    ungroup()
}


#' One reference row per cell and metric: pitcher count, mean, and 101 quantiles.
#' Each metric is filtered on its own denominator floor.
summarise_cells <- function(cells, grain) {
  long <- cells |>
    pivot_longer(all_of(METRIC_SPEC$metric), names_to = "metric", values_to = "value") |>
    left_join(METRIC_SPEC, by = "metric") |>
    mutate(denom_n = case_when(denom == "cut_pitches" ~ cut_pitches,
                               denom == "pitches" ~ pitches,
                               denom == "swings"  ~ swings,
                               denom == "oz"      ~ oz,
                               denom == "pa"      ~ pa,
                               denom == "bbe"     ~ bbe,
                               denom == "z_swings" ~ z_swings)) |>
    filter(is.finite(value), denom_n >= floor)

  long |>
    group_by(across(all_of(c(grain, "metric")))) |>
    summarise(
      n_pitchers = n(),
      mean       = mean(value),
      q          = list(unname(quantile(value, probs = seq(0, 1, 0.01), na.rm = TRUE))),
      .groups    = "drop"
    )
}


#' Full reference at every grain the fallback ladder can reach, keyed by
#' reconciled pitch codes. Pitcher hand is pooled only for rate metrics (p_throws "All").
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

    # Both hands together, rates only, for the ladder's last rung
    both <- if (bk != "All Counts") NULL else
      summarise_cells(pitcher_cells(mutate(d, p_throws = "All"), by_stand = FALSE),
                      c("pitch_type", "p_throws")) |>
      filter(metric %in% METRIC_SPEC$metric[METRIC_SPEC$kind == "rate"]) |>
      mutate(stand = "All")

    bind_rows(fine, coarse, both) |> mutate(count_bucket = bk)
  })

  bind_rows(out) |>
    select(pitch_type, p_throws, stand, count_bucket, metric, n_pitchers, mean, q) |>
    arrange(pitch_type, p_throws, stand, count_bucket, metric)
}
