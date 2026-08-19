# tables.R
#
# The four gt outputs. The *_tbl functions return plain data frames and the *_gt
# functions render them, so the numbers can be checked without rendering.
#
# Requires theme.R for swing_only, whiff_desc, pitch_colors, pitch_text_colors.
#
# Handedness note. Both table pairs filter on `stand` internally and so take a
# `hand` argument, unlike three of the four plots.

library(dplyr)
library(tidyr)
library(purrr)
library(gt)


#' Pitch characteristics for one batter side
#'
#' stuff_all arrives as an argument and is never read from disk here. That is
#' the seam that lets the Stuff+ source change later without touching this
#' function, so do not inline a FanGraphs read or add a `source` argument. See
#' CLAUDE.md.
#'
#' Denominators differ on purpose and are the most commonly botched part of this
#' table. whiff_pct is over swings, chase_pct is over out-of-zone pitches,
#' csw_pct is over all pitches, and xwoba is weighted by woba_denom so only
#' PA-ending rows contribute.
arsenal_table <- function(df, hand, stuff_all) {
  df <- df |> filter(stand == hand) |> mutate(pitch_type = droplevels(pitch_type))
  df |>
    group_by(pitch_type) |>
    summarise(
      count = n(),
      pitch_pct = round(n() / nrow(df) * 100, 1),
      velocity = round(mean(release_speed, na.rm = TRUE), 1),
      ivb = round(mean(ivb, na.rm = TRUE), 1),
      hb = round(mean(hb, na.rm = TRUE), 1),
      spin = round(mean(release_spin_rate, na.rm = TRUE), 0),
      strike_pct = round(mean(type %in% c("S", "X"), na.rm = TRUE) * 100, 1),
      whiff_pct = round(sum(description %in% whiff_desc) / sum(description %in% swing_only) * 100, 1),
      csw_pct = round(mean(description %in% c("called_strike", whiff_desc))*100,1),
      zone_pct = round(mean(in_zone, na.rm = TRUE) * 100, 1),
      chase_pct = round(sum(in_zone == 0 & description %in% swing_only) / sum(in_zone == 0) * 100, 1),
      xwoba = round(sum(estimated_woba_using_speedangle * woba_denom, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE), 3),
      .groups = "drop"
    ) |>
    arrange(desc(pitch_pct)) |>
    left_join(stuff_all, by = "pitch_type") |>
    # A pitch with no FanGraphs row is treated as an exact match so it carries no
    # footnote. Only a grade filled from a non-matching code is flagged.
    mutate(fg_exact = replace_na(fg_exact, TRUE))
}


#' Render the pitch characteristics table
#'
#' fg_window is the date window of the FanGraphs export behind the Stuff+
#' column, passed in as free text and printed in the source note. It is an
#' argument rather than an attribute on stuff_all because CLAUDE.md fixes that
#' contract at three columns, and because an attribute would have to survive a
#' dplyr join that drops it.
#'
#' A missing window prints as missing rather than falling back to a generic
#' note. A stale export is invisible on the page otherwise, which is the failure
#' this note exists to prevent.
arsenal_gt <- function(tbl, hand, fg_window = NULL) {
  source_note <- if (is.null(fg_window)) {
    "Stuff+ from FanGraphs. Export window not supplied."
  } else {
    paste0("Stuff+ from FanGraphs. Export window ", fg_window, ".")
  }

  g <- tbl |>
    gt() |>
    cols_label(
      pitch_type = "PITCH", count = "COUNT", pitch_pct = "PITCH%", velocity = "AVG VELO",
      ivb = "IVB", hb = "HB", spin = "SPIN", strike_pct = "STRIKE%", whiff_pct = "WHIFF%", csw_pct = "CSW%",
      zone_pct = "IN-ZONE%", chase_pct = "CHASE%", xwoba = "xwOBA", stuff_plus = "Stuff+"
    ) |>
    tab_header(title = paste0("PITCH CHARACTERISTICS (vs ", if (hand == "R") "RHH" else "LHH", ")")) |>
    cols_align("center") |>
    cols_width(everything() ~ px(80)) |>
    tab_style(cell_borders(sides = c("top", "bottom"), color = "black", weight = px(2)), cells_column_labels()) |>
    tab_style(cell_text(weight = "bold"), cells_body(columns = pitch_pct)) |>
    tab_options(
      table.font.size = 13, heading.title.font.size = 15, heading.align = "left",
      column_labels.font.weight = "bold", column_labels.background.color = "gray95",
      table.border.top.color = "transparent"
    ) |>
    # Drop the leading zero on xwOBA, the usual convention for a rate bounded
    # below one.
    fmt(columns = xwoba, fns = \(x) sub("^0", "", sprintf("%.3f", x))) |>
    cols_hide(fg_exact) |>
    tab_source_note(source_note)

  # Fires automatically off fg_exact rather than being flagged by hand. See
  # CLAUDE.md, do not manually re-flag these.
  if (any(!tbl$fg_exact)) {
    g <- g |> tab_footnote(
      "FanGraphs grades sweepers and slurves under SL, so this grade is not pitch specific.",
      cells_body(columns = stuff_plus, rows = !fg_exact)
    )
  }

  # Row fill at 20 alpha, with the pitch code itself in the full color. gt has
  # no vectorised way to key fill off a value, so each pitch type is folded in
  # as its own tab_style.
  reduce(as.character(tbl$pitch_type), function(gt_tbl, pt) {
    gt_tbl |>
      tab_style(cell_fill(color = paste0(pitch_colors[[pt]], "20")), cells_body(rows = pitch_type == pt)) |>
      tab_style(cell_text(color = pitch_text_colors[[pt]], weight = "bold"), cells_body(columns = pitch_type, rows = pitch_type == pt))
  }, .init = g)
}


#' Usage by count bucket, wide, for one batter side
#'
#' These six buckets overlap by design and are situational views rather than a
#' partition. They do not sum to 100 across a row and must not be presented as
#' if they do. The heatmaps use three coarser buckets instead, since a KDE needs
#' a bigger per-panel sample. See CLAUDE.md, count buckets.
count_usage_tbl <- function(df, hand) {
  df <- df |> filter(stand == hand) |> mutate(pitch_type = droplevels(pitch_type))
  buckets <- list("All Counts"=NULL, "Early Count"=c("0-0","0-1","1-0"),
                  "Pitcher Ahead"=c("0-1","0-2","1-2","2-2"),
                  "Pitcher Behind"=c("1-0","2-0","3-0","2-1","3-1"),
                  "Pre Two Strikes"=c("0-0","0-1","1-0","1-1","2-1","3-1"),
                  "Two Strikes"=c("0-2","1-2","2-2","3-2"))
  base <- df |> mutate(cnt = paste(balls, strikes, sep = "-"))
  bucket_usage <- function(counts) {
    d <- if (is.null(counts)) base else filter(base, cnt %in% counts)
    d |> count(pitch_type, name = "n") |> mutate(pct = round(n / sum(n) * 100, 1)) |>
      select(pitch_type, pct)
  }
  imap(buckets, \(counts, nm) bucket_usage(counts) |> rename(!!nm := pct)) |>
    reduce(full_join, by = "pitch_type") |>
    arrange(pitch_type) |>
    # A pitch absent from a bucket is 0% usage in that situation, not unknown.
    mutate(across(-pitch_type, \(x) replace_na(x, 0)))
}


#' Render the usage by count table
count_usage_gt <- function(wide, hand) {
  hand_label <- if (hand == "R") "vs RHH" else "vs LHH"
  g <- wide |> gt() |> cols_label(pitch_type = "PITCH") |>
    tab_header(title = paste("USAGE BY COUNT", hand_label)) |> cols_align("center") |>
    fmt_number(columns = -pitch_type, decimals = 1, pattern = "{x}%") |>
    tab_style(cell_text(weight = "bold"), cells_body(columns = "All Counts")) |>
    tab_style(cell_borders(sides = c("top","bottom"), color = "black", weight = px(2)),
              cells_column_labels()) |>
    tab_options(table.font.size = 13, heading.title.font.size = 15, heading.align = "left",
                column_labels.font.weight = "bold", column_labels.background.color = "gray95")
  reduce(as.character(wide$pitch_type), function(gt_tbl, pt) {
    gt_tbl |>
      tab_style(cell_fill(color = paste0(pitch_colors[[pt]], "20")),
                cells_body(rows = pitch_type == pt)) |>
      tab_style(cell_text(color = pitch_text_colors[[pt]], weight = "bold"),
                cells_body(columns = pitch_type, rows = pitch_type == pt))
  }, .init = g)
}
