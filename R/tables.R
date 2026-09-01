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
      # Raw approach angle, always negative, derived per pitch in
      # add_pitch_features() rather than shipped by Savant. One decimal: the
      # league spread across pitch types is about five degrees and within one
      # about one, so a second decimal would be noise on the page.
      vaa = round(mean(vaa, na.rm = TRUE), 1),
      spin = round(mean(release_spin_rate, na.rm = TRUE), 0),
      ext = round(mean(release_extension, na.rm = TRUE), 1),
      # Two decimals, unlike everything else here. These are in feet with a
      # league spread near half a foot, so one decimal would bucket a third of
      # the league onto the same number and make the shading look arbitrary
      # against a value that never moves.
      rel_ht = round(mean(release_pos_z, na.rm = TRUE), 2),
      # Negated, and NOT arm-side mirrored. Deliberately unlike hb directly
      # above, which is mirrored, and the difference is the point.
      #
      # hb's mirror earns its keep because hb's sign varies WITHIN a hand: a
      # righty's slider runs glove side and his sinker runs arm side, so the raw
      # sign means two different things and normalising it is what lets one
      # column be read. Release side never crosses the midline. Its sign is
      # handedness and nothing else, so mirroring it deleted the only thing the
      # sign carried and then spent a footnote saying so.
      #
      # The negation stays. Savant measures release_pos_x from the catcher's
      # view, positive toward first base, which puts a LHP at about +2.1 and a
      # RHP at -1.9. Negating reads positive toward third base, so a RHP is
      # positive and a LHP negative. A display convention, not a correction. The
      # reference applies the identical expression and the two must not drift.
      rel_side = round(-mean(release_pos_x, na.rm = TRUE), 2),
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


# ---- The Characteristics tab: two tables ------------------------------------

#' Which computed column belongs to which table
#'
#' Split on 2026-08-31. One table asked two questions at once, "what is this
#' pitch" and "what did it do", and the reader had to know which columns were
#' which to read either. Splitting them is the whole feature; these two vectors
#' are where the answer lives, so a new column is placed by editing a list
#' rather than by editing a renderer.
#'
#' arsenal_table() still computes both in ONE pass and the projections happen
#' after, so the split costs no extra grouping work per reactive. Do not give
#' these their own summarise.
#'
#' PITCH / COUNT / PITCH% repeat in both. They are the identity block, and a
#' table you cannot read without looking at the other one is not a split.
#'
#' Stuff+ is a trait. It grades shape, and it grades it off a model whose
#' features are release, movement and velocity, so it belongs beside the inputs
#' it is built from rather than beside the outcomes it is trying to predict.
#'
#' xwOBA is a result even though it is the least "resulty" of them, being
#' contact-quality-estimated rather than counted. It is still a thing that
#' happened to the pitch, not a property of the pitch.
TRAITS_COLS <- c("pitch_type", "count", "pitch_pct",
                 "velocity", "ivb", "hb", "vaa", "spin",
                 "ext", "rel_ht", "rel_side",
                 "stuff_plus", "fg_exact")

RESULTS_COLS <- c("pitch_type", "count", "pitch_pct",
                  "strike_pct", "whiff_pct", "csw_pct",
                  "zone_pct", "chase_pct", "xwoba")

#' any_of(), not all_of(), and this is the one place in the file where that is
#' correct rather than sloppy. fg_exact is absent whenever stuff_all carried no
#' rows, and a traits table with no Stuff+ column is a real state the app
#' renders. The pitch-level contract in features.R uses all_of() for the
#' opposite reason: there a missing column IS the bug.
traits_tbl  <- function(tbl) select(tbl, any_of(TRAITS_COLS))
results_tbl <- function(tbl) select(tbl, any_of(RESULTS_COLS))


#' The shared chassis both tables are drawn on
#'
#' Everything below was one function until the split, and the risk of splitting
#' a renderer is that the two halves drift into looking like different products.
#' So the frame, the borders, the type sizes and the pitch-code colouring are
#' factored here and neither renderer restates them. Only what genuinely differs
#' between the two tables lives in the renderers.
#' `col_px` exists because the two tables have different column COUNTS and are
#' stacked one above the other. At a shared 80px the results table finished 240px
#' short of the traits table above it, and two left-aligned tables of visibly
#' different width read as a rendering fault rather than as a design. Each
#' renderer picks a width that lands them on roughly the same total instead.
gt_chassis <- function(tbl, title, col_px = 80) {
  # The width is baked into the formula as a LITERAL rather than passed as a
  # variable. gt evaluates a cols_width() formula lazily in an environment where
  # a local argument does not exist, so both `px(col_px)` and an injected `!!wid`
  # fail at RENDER time with "object not found" -- late, and only on the code
  # path that renders. Building the formula text sidesteps the lookup entirely.
  wid <- stats::as.formula(sprintf("everything() ~ px(%d)", as.integer(col_px)))
  tbl |>
    gt() |>
    tab_header(title = title) |>
    cols_align("center") |>
    cols_width(wid) |>
    tab_style(cell_borders(sides = c("top", "bottom"), color = "black", weight = px(2)),
              cells_column_labels()) |>
    tab_style(cell_text(weight = "bold"), cells_body(columns = pitch_pct)) |>
    tab_options(
      table.font.size = 13, heading.title.font.size = 15, heading.align = "left",
      column_labels.font.weight = "bold", column_labels.background.color = "gray95",
      table.border.top.color = "transparent"
    )
}


#' Colour the pitch code, and nothing else on the row
#'
#' The row used to be washed at 20 alpha as well, which made the table a
#' patchwork the moment percentile fills landed on top of it: two colour systems
#' on one surface, one saying WHICH pitch and one saying HOW GOOD, and the eye
#' cannot separate them. White rows leave the percentile fill as the only thing
#' that varies, and the identity survives in the code itself.
#'
#' Must run BEFORE apply_league_ref(). The later tab_style wins in gt, and a
#' percentile fill landing on the pitch_type column afterwards would not touch
#' the text but would tint the identity cell.
style_pitch_codes <- function(g, tbl) {
  reduce(as.character(tbl$pitch_type), function(gt_tbl, pt) {
    gt_tbl |>
      tab_style(cell_text(color = pitch_text_colors[[pt]], weight = "bold"),
                cells_body(columns = pitch_type, rows = pitch_type == pt))
  }, .init = g)
}


#' Paint the resolved league context onto a table
#'
#' `ref` is the resolved context from resolve_table(), not the raw league
#' reference. NULL means no context and must render exactly what the table
#' rendered before Part B existed.
#'
#' resolve_table() narrows ARSENAL_METRIC_COLS to the columns actually present,
#' so this function needs no knowledge of which of the two tables it is painting.
#' That is the property that made the split cheap, and it is worth preserving:
#' anything added here that names a specific column breaks it.
#'
#' `notes = FALSE` paints the colour and suppresses every piece of prose: the
#' per-column METRIC_NOTES footnotes, the state notes, and the marker glyphs that
#' those state notes are the legend for. The traits table uses it.
#'
#' Dropping the glyphs along with the prose is a judgement, not a mechanical
#' consequence, so it is written down. A dagger whose legend has been deleted is
#' strictly worse than no dagger: it is a symbol the page never defines. The
#' below-floor marker is KEPT, because it is the value's own denominator in
#' parentheses and reads without a legend, and because the grey italic it travels
#' with is the actual signal.
#'
#' No conditionals in the fill loop below. Every cell carries a concrete fill,
#' colour, style, weight and marker, with white for the unfilled states and ""
#' for no marker, which is what keeps the state decision in resolve_cell() and
#' out of here.
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

  # text_transform, not fmt. fmt does not compound: a marker fmt stacked on the
  # xwOBA fmt in results_gt() wins outright, drops the leading-zero rule and
  # renders 0.32 where the table has always shown .320. text_transform receives
  # the text gt has already formatted, so it composes with both that rule and
  # gt's own per-column decimal padding.
  mk <- ref$cells[nzchar(ref$cells$marker), , drop = FALSE]
  # The two legend-bearing glyphs, dropped when there is no legend to bear.
  if (!notes) mk <- mk[!mk$marker %in% c("\u2020", "\u2021"), , drop = FALSE]
  g <- reduce(seq_len(nrow(mk)), function(gt_tbl, i) {
    col <- mk$column[i]; rw <- mk$row[i]; m <- mk$marker[i]
    gt_tbl |> text_transform(cells_body(columns = col, rows = rw),
                             fn = function(x) paste0(x, m))
  }, .init = g)

  if (!notes) return(g)

  # On the column label, where there is no collision with the cell markers.
  g <- reduce(names(ref$col_notes), function(gt_tbl, col) {
    gt_tbl |> tab_footnote(ref$col_notes[[col]], cells_column_labels(columns = col))
  }, .init = g)

  # In the order resolve_table() fixed them: dagger, double dagger, grey line.
  reduce(ref$notes, function(gt_tbl, n) tab_source_note(gt_tbl, n), .init = g)
}


#' Render the pitch TRAITS table
#'
#' What the pitch is: how hard, what shape, from where. Nothing in here is an
#' outcome, which is the line the split draws.
#'
#' Note that the release columns barely move between the vs-LHH and vs-RHH
#' views, because a slot is a slot whoever is standing in the box. That is
#' correct and not a caching bug. They are still filtered by `stand` so the
#' pitch mix behind every column matches the results table beside it.
#'
#' CARRIES NO FOOTNOTES, by request on 2026-08-31. Every one of them explained a
#' convention the number already showed, and five lines of prose under a six-row
#' table was most of what the reader saw. The results table keeps its own.
#'
#' `fg_window` is retained in the signature and is deliberately unused. It is the
#' date window of the FanGraphs export behind Stuff+, and it used to print in a
#' source note so a stale export could not be invisible on the page. That note is
#' gone, so THE PAGE NO LONGER SHOWS WHETHER STUFF+ IS STALE. Keeping the
#' argument keeps the caller in app.R passing it and makes restoring the note a
#' one-line change rather than a re-trace.
#'
#' Two things went with the footnotes and are worth naming, because neither is
#' recoverable by reading the table:
#'
#'   - the sweeper caveat. FanGraphs grades ST and SV under SL, so a sweeper's
#'     Stuff+ is a slider's. The table now shows that grade with nothing marking
#'     it as not pitch specific. fg_exact still carries the fact, so restoring
#'     the footnote needs no new data.
#'   - the fallback and no-reference daggers, dropped in apply_league_ref() with
#'     their legend. Those cells keep their fill and weight, so a percentile off
#'     a coarser league cut is no longer distinguishable from an exact one.
traits_gt <- function(tbl, hand, fg_window = NULL, label = hand_label(hand),
                      ref = NULL) {
  g <- tbl |>
    gt_chassis(paste0("PITCH TRAITS (", label, ")")) |>
    cols_label(
      pitch_type = "PITCH", count = "COUNT", pitch_pct = "PITCH%",
      velocity = "AVG VELO", ivb = "IVB", hb = "HB", vaa = "VAA", spin = "SPIN",
      ext = "EXT", rel_ht = "REL HT", rel_side = "REL SIDE",
      stuff_plus = "Stuff+"
    )

  if ("fg_exact" %in% names(tbl)) g <- g |> cols_hide(fg_exact)

  g <- style_pitch_codes(g, tbl)

  # Stuff+ shades itself, on the same ramp as every other cell but with no
  # league lookup: 100 is average by construction, so the anchor lives in the
  # scale rather than in league_ref, which carries no Stuff+ metric. Above 100
  # reddens, below 100 blues, clamped at STUFF_PLUS_SPAN either side.
  #
  # Before apply_league_ref(), so a table rendered with no league context still
  # gets it. No batching: this is one cell per pitch type, at most eleven.
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


#' Render the pitch RESULTS table
#'
#' What the pitch did. No Stuff+ and no source note of its own: every number
#' here comes from the same Statcast pull the rest of the app runs on, so a
#' provenance note would be saying nothing the page does not already assume.
#' Keeps its footnotes. They explain the percentile scope and the below-floor
#' grey, which are statements about how much the numbers can be trusted rather
#' than restatements of what a column means, and that is the line the traits
#' table's footnotes fell on the wrong side of.
#'
#' RESULTS_COL_PX is set so this table finishes about as wide as the traits table
#' stacked above it. Recompute it if either column list changes: the traits table
#' is 12 columns at 80px, so 960 / ncol here.
results_gt <- function(tbl, hand, label = hand_label(hand), ref = NULL) {
  g <- tbl |>
    gt_chassis(paste0("PITCH RESULTS (", label, ")"),
               col_px = round(960 / ncol(tbl))) |>
    cols_label(
      pitch_type = "PITCH", count = "COUNT", pitch_pct = "PITCH%",
      strike_pct = "STRIKE%", whiff_pct = "WHIFF%", csw_pct = "CSW%",
      zone_pct = "IN-ZONE%", chase_pct = "CHASE%", xwoba = "xwOBA"
    ) |>
    # Drop the leading zero on xwOBA, the usual convention for a rate bounded
    # below one.
    fmt(columns = xwoba, fns = \(x) sub("^0", "", sprintf("%.3f", x)))

  g <- style_pitch_codes(g, tbl)
  apply_league_ref(g, ref)
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
#' EXPECTED_DIFFS, unlike the characteristics renderers.
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

  # Identical shape to apply_league_ref(): after the pitch-colour reduce, no
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
