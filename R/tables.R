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


#' A rate, or NA when nothing was measured
#'
#' 0/0 is NaN in R, and NaN travels all the way to the page: a pitch type nobody
#' swung at rendered the literal text "NaN (0)" in the whiff column. The cell is
#' below floor either way, so the only thing at stake is whether the reader sees
#' an empty sample or a computer error. Measured 2026-08-22: 1.1% of full-season
#' table rows carry at least one empty denominator, 4.5% over a one-week window,
#' which is exactly the window a scout picks.
pct_or_na <- function(num, den) if (den > 0) num / den * 100 else NA_real_


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
  # "All" pools both batter sides rather than selecting a third one, so the
  # filter is skipped entirely. For "L" and "R" this is identical to before.
  if (hand != "All") df <- filter(df, stand == hand)
  df <- df |> mutate(pitch_type = droplevels(pitch_type))
  df |>
    group_by(pitch_type) |>
    summarise(
      count = n(),
      pitch_pct = round(n() / nrow(df) * 100, 1),
      velocity = round(mean(release_speed, na.rm = TRUE), 1),
      ivb = round(mean(ivb, na.rm = TRUE), 1),
      # Arm-side normalised, so a positive number means arm-side run whichever
      # hand threw it. The movement chart keeps the raw sign, because there the
      # direction is the picture. See arm_side_sign() in theme.R.
      hb = round(mean(hb, na.rm = TRUE) * arm_side_sign(p_throws[1]), 1),
      spin = round(mean(release_spin_rate, na.rm = TRUE), 0),
      strike_pct = round(mean(type %in% c("S", "X"), na.rm = TRUE) * 100, 1),
      whiff_pct = round(pct_or_na(sum(description %in% whiff_desc),
                                  sum(description %in% swing_only)), 1),
      csw_pct = round(mean(description %in% c("called_strike", whiff_desc))*100,1),
      zone_pct = round(mean(in_zone, na.rm = TRUE) * 100, 1),
      chase_pct = round(pct_or_na(sum(in_zone == 0 & description %in% swing_only),
                                  sum(in_zone == 0)), 1),
      # Written out rather than through pct_or_na, which is a percentage. Same
      # guard: no PA ended on this pitch type means no xwOBA, not NaN.
      xwoba = { d <- sum(woba_denom, na.rm = TRUE)
                if (d > 0) round(sum(estimated_woba_using_speedangle * woba_denom,
                                     na.rm = TRUE) / d, 3) else NA_real_ },
      .groups = "drop"
    ) |>
    arrange(desc(pitch_pct)) |>
    left_join(stuff_all, by = "pitch_type") |>
    # A pitch with no FanGraphs row is treated as an exact match so it carries no
    # footnote. Only a grade filled from a non-matching code is flagged.
    mutate(fg_exact = replace_na(fg_exact, TRUE))
}


#' The four denominators behind the arsenal table, one row per pitch type
#'
#' Deliberately NOT columns on arsenal_table(). Its output is a compared
#' artifact in the Phase 1 regression, so widening it would make both
#' arsenal_table_R and arsenal_table_L differ and cost two sanctioned entries in
#' EXPECTED_DIFFS. The approved plan said to return them from arsenal_table();
#' the byte-identity invariant outranks that, so they live here instead.
#'
#' The filter and the droplevels have to match arsenal_table() exactly, or a
#' pitch type present in one frame is missing from the other.
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
      .groups = "drop"
    )
}


#' Render the pitch characteristics table
#'
#' `ref` is the resolved context from resolve_table(), not the raw league
#' reference. NULL means no context, and must render exactly what this function
#' rendered before Part B existed. tests/step3_null_identical.R is the guard;
#' scripts/phase1_check.R cannot be, since all three arsenal_gt artifacts are
#' already sanctioned in EXPECTED_DIFFS.
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
arsenal_gt <- function(tbl, hand, fg_window = NULL, label = hand_label(hand),
                       ref = NULL) {
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
    tab_header(title = paste0("PITCH CHARACTERISTICS (", label, ")")) |>
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

  # The pitch code carries the colour and nothing else does. The row used to be
  # washed at 20 alpha as well, which made the whole table a patchwork the moment
  # percentile fills landed on top of it: two colour systems on one surface, one
  # saying WHICH pitch and one saying HOW GOOD, and the eye cannot separate them.
  # White rows leave the percentile fill as the only thing that varies, and the
  # identity survives in the code itself.
  g <- reduce(as.character(tbl$pitch_type), function(gt_tbl, pt) {
    gt_tbl |>
      tab_style(cell_text(color = pitch_text_colors[[pt]], weight = "bold"),
                cells_body(columns = pitch_type, rows = pitch_type == pt))
  }, .init = g)

  # Stuff+ shades itself, on the same ramp as every other cell but with no
  # league lookup: 100 is average by construction, so the anchor lives in the
  # scale rather than in league_ref, which carries no Stuff+ metric. Above 100
  # reddens, below 100 blues, clamped at STUFF_PLUS_SPAN either side.
  #
  # Before the ref early-return, so a table rendered with no league context
  # still gets it. No batching: this is one cell per pitch type, at most eleven.
  if ("stuff_plus" %in% names(tbl)) {
    sp <- tbl$stuff_plus
    g <- reduce(which(is.finite(sp)), function(gt_tbl, i) {
      p <- 50 + 50 * max(-1, min(1, (sp[i] - 100) / STUFF_PLUS_SPAN))
      gt_tbl |> tab_style(cell_fill(color = pctile_fill(p, "high")),
                          cells_body(columns = "stuff_plus", rows = i))
    }, .init = g)
  }

  if (is.null(ref)) return(g)

  # Still after the pitch-code reduce, though the reason has changed. It used to
  # be that a row fill applied later would silently overwrite every percentile,
  # since the later tab_style wins in gt. There is no row fill any more, so what
  # the ordering protects now is narrower: the pitch code's own cell must keep
  # its colour and weight, and a percentile fill landing on that column later
  # would not touch the text but would tint the identity cell.
  #
  # No conditionals below. Every cell carries a concrete fill, colour, style,
  # weight and marker, with white for the unfilled states and "" for no marker,
  # which is what keeps the state decision in resolve_cell() and out of here.
  g <- reduce(seq_len(nrow(ref$cells)), function(gt_tbl, i) {
    ce <- ref$cells[i, ]
    gt_tbl |>
      tab_style(cell_fill(color = ce$fill),
                cells_body(columns = ce$column, rows = ce$row)) |>
      tab_style(cell_text(color = ce$text_color, style = ce$font_style,
                          weight = ce$font_weight),
                cells_body(columns = ce$column, rows = ce$row))
  }, .init = g)

  # text_transform, not fmt. fmt does not compound: a marker fmt stacked on the
  # xwOBA fmt above wins outright, drops the leading-zero rule and renders 0.32
  # where the table has always shown .320. text_transform receives the text gt
  # has already formatted, so it composes with both that rule and gt's own
  # per-column decimal padding.
  mk <- ref$cells[nzchar(ref$cells$marker), , drop = FALSE]
  g <- reduce(seq_len(nrow(mk)), function(gt_tbl, i) {
    col <- mk$column[i]; rw <- mk$row[i]; m <- mk$marker[i]
    gt_tbl |> text_transform(cells_body(columns = col, rows = rw),
                             fn = function(x) paste0(x, m))
  }, .init = g)

  # On the column label, where there is no collision with the cell markers.
  g <- reduce(names(ref$col_notes), function(gt_tbl, col) {
    gt_tbl |> tab_footnote(ref$col_notes[[col]], cells_column_labels(columns = col))
  }, .init = g)

  # In the order resolve_table() fixed them: dagger, double dagger, grey line.
  reduce(ref$notes, function(gt_tbl, n) tab_source_note(gt_tbl, n), .init = g)
}


#' Usage by count bucket, wide, for one batter side
#'
#' These six buckets overlap by design and are situational views rather than a
#' partition. They do not sum to 100 across a row and must not be presented as
#' if they do. The heatmaps use three coarser buckets instead, since a KDE needs
#' a bigger per-panel sample. See CLAUDE.md, count buckets.
count_usage_tbl <- function(df, hand) {
  # "All" pools both batter sides rather than selecting a third one, so the
  # filter is skipped entirely. For "L" and "R" this is identical to before.
  if (hand != "All") df <- filter(df, stand == hand)
  df <- df |> mutate(pitch_type = droplevels(pitch_type))
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


#' Pitches per pitch type per count bucket, the usage table's denominators
#'
#' Long rather than wide, keyed by bucket, because the league reference is keyed
#' by count bucket too and resolve_usage() looks each column up by name.
#'
#' The buckets are read from COUNT_BUCKETS in theme.R rather than redeclared, so
#' this cannot drift from count_usage_tbl().
count_usage_denoms <- function(df, hand) {
  if (hand != "All") df <- filter(df, stand == hand)
  df <- df |> mutate(pitch_type = droplevels(pitch_type), cnt = paste(balls, strikes, sep = "-"))
  map_dfr(names(COUNT_BUCKETS), function(nm) {
    counts <- COUNT_BUCKETS[[nm]]
    d <- if (is.null(counts)) df else filter(df, cnt %in% counts)
    tidyr::complete(count(d, pitch_type, name = "pitches"),
                    pitch_type = levels(df$pitch_type), fill = list(pitches = 0)) |>
      # cut_pitches is the bucket total and is the same for every pitch type in
      # the bucket, since that is what each share was measured over.
      mutate(count_bucket = nm, cut_pitches = sum(pitches),
             swings = 0, oz = 0, pa = 0)
  })
}


#' Render the usage by count table
#'
#' `ref` is the resolved context from resolve_usage(), not the raw league
#' reference. NULL renders exactly what this rendered before Part B, which
#' scripts/phase1_check.R does guard here: count_usage_gt is not in
#' EXPECTED_DIFFS, unlike arsenal_gt.
count_usage_gt <- function(wide, hand, label = hand_label(hand), ref = NULL) {
  g <- wide |> gt() |> cols_label(pitch_type = "PITCH") |>
    tab_header(title = paste("USAGE BY COUNT", label)) |> cols_align("center") |>
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

  if (is.null(ref)) return(g)

  # Identical shape to arsenal_gt: after the pitch-colour reduce, no
  # conditionals, markers through text_transform rather than fmt.
  g <- reduce(seq_len(nrow(ref$cells)), function(gt_tbl, i) {
    ce <- ref$cells[i, ]
    gt_tbl |>
      tab_style(cell_fill(color = ce$fill), cells_body(columns = ce$column, rows = ce$row)) |>
      tab_style(cell_text(color = ce$text_color, style = ce$font_style, weight = ce$font_weight),
                cells_body(columns = ce$column, rows = ce$row))
  }, .init = g)

  mk <- ref$cells[nzchar(ref$cells$marker), , drop = FALSE]
  g <- reduce(seq_len(nrow(mk)), function(gt_tbl, i) {
    col <- mk$column[i]; rw <- mk$row[i]; m <- mk$marker[i]
    gt_tbl |> text_transform(cells_body(columns = col, rows = rw),
                             fn = function(x) paste0(x, m))
  }, .init = g)

  reduce(ref$notes, function(gt_tbl, n) tab_source_note(gt_tbl, n), .init = g)
}
