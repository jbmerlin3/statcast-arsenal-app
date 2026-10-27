# tables.R
# gt tables. *_tbl functions return data frames and *_gt functions render them.

library(dplyr)
library(tidyr)
library(purrr)
library(gt)


#' A percentage, or NA when the denominator is zero (never NaN)
pct_or_na <- function(num, den) if (den > 0) num / den * 100 else NA_real_


#' Pitch characteristics for one batter side, one row per pitch type.
#' Denominators: whiff% over swings, chase% over out-of-zone pitches, CSW% over all
#' pitches, xwOBA weighted by woba_denom. Stuff+ comes in as `stuff_all`.
arsenal_table <- function(df, hand, stuff_all) {
  if (hand != "All") df <- filter(df, stand == hand)
  df <- df |> mutate(pitch_type = droplevels(pitch_type))
  df |>
    group_by(pitch_type) |>
    summarise(
      count = n(),
      pitch_pct = round(n() / nrow(df) * 100, 1),
      velocity = round(mean(release_speed, na.rm = TRUE), 1),
      ivb = round(mean(ivb, na.rm = TRUE), 1),
      # Raw sign, as on the movement chart
      hb = round(mean(hb, na.rm = TRUE), 1),
      # Location-adjusted (see add_pitch_features())
      vaa = round(mean(vaa, na.rm = TRUE), 1),
      spin = round(mean(release_spin_rate, na.rm = TRUE), 0),
      ext = round(mean(release_extension, na.rm = TRUE), 1),
      rel_ht = round(mean(release_pos_z, na.rm = TRUE), 2),
      # Positive toward third base; build_league_ref.R must match
      rel_side = round(-mean(release_pos_x, na.rm = TRUE), 2),
      strike_pct = round(mean(type %in% c("S", "X"), na.rm = TRUE) * 100, 1),
      whiff_pct = round(pct_or_na(sum(description %in% whiff_desc),
                                  sum(description %in% swing_only)), 1),
      # Whiffs on in-zone swings, over in-zone swings
      z_whiff_pct = round(pct_or_na(sum(in_zone == 1 & description %in% whiff_desc, na.rm = TRUE),
                                    sum(in_zone == 1 & description %in% swing_only, na.rm = TRUE)), 1),
      csw_pct = round(mean(description %in% c("called_strike", whiff_desc))*100,1),
      zone_pct = round(mean(in_zone, na.rm = TRUE) * 100, 1),
      chase_pct = round(pct_or_na(sum(in_zone == 0 & description %in% swing_only),
                                  sum(in_zone == 0)), 1),
      xwoba = { d <- sum(woba_denom, na.rm = TRUE)
                if (d > 0) round(sum(estimated_woba_using_speedangle * woba_denom,
                                     na.rm = TRUE) / d, 3) else NA_real_ },
      # Ground balls over balls in play (bb_type is "" elsewhere)
      gb_pct = { b <- description == "hit_into_play" & nzchar(bb_type) & !is.na(bb_type)
                 round(pct_or_na(sum(b & bb_type == "ground_ball"), sum(b)), 1) },
      # Run value, pitcher's perspective: positive is good
      rv = round(sum(delta_pitcher_run_exp, na.rm = TRUE), 1),

      .groups = "drop"
    ) |>
    arrange(desc(pitch_pct)) |>
    left_join(stuff_all, by = "pitch_type")
}


#' Denominators behind arsenal_table(), one row per pitch type (same filter)
arsenal_denoms <- function(df, hand) {
  if (hand != "All") df <- filter(df, stand == hand)
  df |>
    mutate(pitch_type = droplevels(pitch_type)) |>
    group_by(pitch_type) |>
    summarise(
      pitches = n(),
      swings  = sum(description %in% swing_only),
      oz      = sum(in_zone == 0),
      pa      = sum(woba_denom, na.rm = TRUE),
      z_swings = sum(in_zone == 1 & description %in% swing_only, na.rm = TRUE),
      # Same denominator as gb_pct
      bbe     = sum(description == "hit_into_play" & nzchar(bb_type) & !is.na(bb_type)),
      .groups = "drop"
    )
}


# ---- Characteristics tab -----------------------------------------------------

# Columns for each Characteristics table, both from one arsenal_table() pass
TRAITS_COLS <- c("pitch_type", "count", "pitch_pct",
                 "velocity", "ivb", "hb", "vaa", "spin",
                 "ext", "rel_ht", "rel_side",
                 "stuff_plus")

RESULTS_COLS <- c("pitch_type",
                  "strike_pct", "whiff_pct", "z_whiff_pct", "csw_pct",
                  "zone_pct", "chase_pct", "gb_pct", "xwoba", "rv")

#' any_of(): stuff_plus is absent when there are no grades
traits_tbl  <- function(tbl) select(tbl, any_of(TRAITS_COLS))
results_tbl <- function(tbl) select(tbl, any_of(RESULTS_COLS))


#' Shared gt frame for both Characteristics tables (TABLE_WIDTH_PX wide)
gt_chassis <- function(tbl, title, n_visible = ncol(tbl)) {
  # Widths are literals in the formula: gt evaluates cols_width() lazily
  base <- TABLE_WIDTH_PX %/% n_visible
  rem  <- TABLE_WIDTH_PX %% n_visible
  # Remainder px on the first column
  wid1 <- stats::as.formula(sprintf("%s ~ px(%d)", names(tbl)[1], base + rem))
  wid  <- stats::as.formula(sprintf("everything() ~ px(%d)", base))
  g <- tbl |>
    gt() |>
    tab_header(title = title) |>
    cols_align("center") |>
    cols_width(wid1, wid) |>
    tab_style(cell_borders(sides = c("top", "bottom"), color = "black", weight = px(2)),
              cells_column_labels())

  # pitch_pct only exists in the traits table
  if ("pitch_pct" %in% names(tbl)) {
    g <- g |> tab_style(cell_text(weight = "bold"), cells_body(columns = pitch_pct))
  }

  g |> tab_options(
    table.font.size = 13, heading.title.font.size = 15, heading.align = "left",
    column_labels.font.weight = "bold", column_labels.background.color = "gray95",
    table.border.top.color = "transparent"
  )
}


#' Color the pitch code. Run before apply_league_ref(): the later tab_style wins.
style_pitch_codes <- function(g, tbl) {
  reduce(as.character(tbl$pitch_type), function(gt_tbl, pt) {
    gt_tbl |>
      tab_style(cell_text(color = pitch_text_colors[[pt]], weight = "bold"),
                cells_body(columns = pitch_type, rows = pitch_type == pt))
  }, .init = g)
}


#' Paint resolved league context onto a table. `notes = FALSE` keeps fills and
#' (n) markers but drops footnotes and the double dagger they explain.
apply_league_ref <- function(g, ref, notes = TRUE) {
  if (is.null(ref)) return(g)

  g <- reduce(seq_len(nrow(ref$cells)), function(gt_tbl, i) {
    ce <- ref$cells[i, ]
    gt_tbl |>
      tab_style(cell_fill(color = ce$fill),
                cells_body(columns = ce$column, rows = ce$row)) |>
      tab_style(cell_text(color = ce$text_color, style = ce$font_style,
                          weight = ce$font_weight),
                cells_body(columns = ce$column, rows = ce$row))
  }, .init = g)

  # text_transform, not fmt: a second fmt would replace xwOBA's format
  mk <- ref$cells[nzchar(ref$cells$marker), , drop = FALSE]
  if (!notes) mk <- mk[mk$marker != "\u2021", , drop = FALSE]
  g <- reduce(seq_len(nrow(mk)), function(gt_tbl, i) {
    col <- mk$column[i]; rw <- mk$row[i]; m <- mk$marker[i]
    gt_tbl |> text_transform(cells_body(columns = col, rows = rw),
                             fn = function(x) paste0(x, m))
  }, .init = g)

  if (!notes) return(g)

  g <- reduce(names(ref$col_notes), function(gt_tbl, col) {
    gt_tbl |> tab_footnote(ref$col_notes[[col]], cells_column_labels(columns = col))
  }, .init = g)

  reduce(ref$notes, function(gt_tbl, n) tab_source_note(gt_tbl, n), .init = g)
}


#' Render the pitch traits table. Stuff+ shades around 100.
traits_gt <- function(tbl, hand, label = hand_label(hand), ref = NULL) {
  g <- tbl |>
    gt_chassis(paste0("PITCH TRAITS (", label, ")")) |>
    cols_label(
      pitch_type = "PITCH", count = "COUNT", pitch_pct = "PITCH%",
      velocity = "AVG VELO", ivb = "IVB", hb = "HB", vaa = "VAA", spin = "SPIN",
      ext = "EXT", rel_ht = "REL HT", rel_side = "REL SIDE",
      stuff_plus = "Stuff+"
    )

  g <- style_pitch_codes(g, tbl)

  # Stuff+ is centered on 100, so it needs no league lookup
  if ("stuff_plus" %in% names(tbl)) {
    sp <- tbl$stuff_plus
    g <- reduce(which(is.finite(sp)), function(gt_tbl, i) {
      p <- 50 + 50 * max(-1, min(1, (sp[i] - 100) / STUFF_PLUS_SPAN))
      gt_tbl |> tab_style(cell_fill(color = pctile_fill(p, "high")),
                          cells_body(columns = "stuff_plus", rows = i))
    }, .init = g)
  }

  apply_league_ref(g, ref, notes = FALSE)
}


#' Render the pitch results table
results_gt <- function(tbl, hand, label = hand_label(hand), ref = NULL) {
  g <- tbl |>
    gt_chassis(paste0("PITCH RESULTS (", label, ")"), n_visible = ncol(tbl)) |>
    cols_label(
      pitch_type = "PITCH",
      strike_pct = "STRIKE%", whiff_pct = "WHIFF%", z_whiff_pct = "Z-WHIFF%", csw_pct = "CSW%",
      zone_pct = "IN-ZONE%", chase_pct = "CHASE%", gb_pct = "GB%",
      xwoba = "xwOBA", rv = "RV"
    ) |>
    # Signed run value
    fmt_number(columns = rv, decimals = 1, force_sign = TRUE) |>
    # Drop the leading zero on xwOBA
    fmt(columns = xwoba, fns = \(x) sub("^0", "", sprintf("%.3f", x)))

  g <- style_pitch_codes(g, tbl)
  apply_league_ref(g, ref)
}


#' Usage by count bucket, wide, for one batter side. The buckets overlap and
#' don't sum to 100.
count_usage_tbl <- function(df, hand, tto = "All") {
  if (hand != "All") df <- filter(df, stand == hand)
  df <- df |> mutate(pitch_type = droplevels(pitch_type))
  # Cut after droplevels so every view lists the same pitches
  df <- filter_tto(df, tto)
  base <- df |> mutate(cnt = paste(balls, strikes, sep = "-"))
  bucket_usage <- function(counts) {
    d <- if (is.null(counts)) base else filter(base, cnt %in% counts)
    # .drop = FALSE keeps a zero row for every pitch type
    d |> count(pitch_type, name = "n", .drop = FALSE) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      select(pitch_type, pct)
  }
  imap(COUNT_BUCKETS, \(counts, nm) bucket_usage(counts) |> rename(!!nm := pct)) |>
    reduce(full_join, by = "pitch_type") |>
    arrange(pitch_type) |>
    # Absent from a bucket means 0%
    mutate(across(-pitch_type, \(x) replace_na(x, 0)))
}


#' Render the usage by count table
count_usage_gt <- function(wide, hand, label = hand_label(hand),
                           heading = "USAGE BY COUNT") {
  g <- wide |> gt() |> cols_label(pitch_type = "PITCH") |>
    tab_header(title = trimws(paste(heading, label))) |> cols_align("center") |>
    fmt_number(columns = -pitch_type, decimals = 1, pattern = "{x}%") |>
    tab_style(cell_text(weight = "bold"), cells_body(columns = "All Counts")) |>
    tab_style(cell_borders(sides = c("top","bottom"), color = "black", weight = px(2)),
              cells_column_labels()) |>
    tab_options(table.font.size = 13, heading.title.font.size = 15, heading.align = "left",
                column_labels.font.weight = "bold", column_labels.background.color = "gray95")
  g <- reduce(as.character(wide$pitch_type), function(gt_tbl, pt) {
    gt_tbl |>
      tab_style(cell_fill(color = paste0(pitch_colors[[pt]], "20")),
                cells_body(rows = pitch_type == pt)) |>
      tab_style(cell_text(color = pitch_text_colors[[pt]], weight = "bold"),
                cells_body(columns = pitch_type, rows = pitch_type == pt))
  }, .init = g)
  g
}
