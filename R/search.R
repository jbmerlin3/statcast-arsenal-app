# search.R
#
# League-wide pitch trait search. The rest of the app answers "show me this
# pitcher"; this answers "who throws this", which is the question behind
# "righty four-seams at 95+ with 18 IVB and 12 HB".
#
# Requires theme.R for SEARCH_TRAITS, swing_only, whiff_desc and MIN_REF_PITCHERS,
# features.R for reconcile_pitch_codes(), tables.R for pct_or_na().
#
# Everything here is a pure function over a frame, like tables.R and plots.R, so
# each one runs from the console with no server around it.
#
# TWO HANDS, AND THEY ARE NOT THE SAME CONTROL. `p_throws` is the pitcher,
# "righty four-seams". `stand` is the batter, "vs RHH". Both narrow a search and
# they do different things: the shape numbers barely move with the batter side
# while the outcome numbers move a lot. Anything here that takes one takes it by
# name, never positionally, because a swap of the two produces a plausible table
# rather than an error.

library(dplyr)


#' Aggregate every pitcher in the frame to one row per pitch type
#'
#' This is the expensive half of the search and the reason the tab is usable at
#' all: measured 0.21 s over the full season, 572,181 pitches and 818 pitchers,
#' 0.05 s over a two-week window. It depends only on the date window and the
#' batter side, so dragging a slider never re-runs it.
#'
#' `hand` is the BATTER side, matching arsenal_table() and count_usage_tbl().
#' It is applied before aggregating, so whiff, chase and xwOBA are the rates
#' against that side.
#'
#' Pitch codes go through reconcile_pitch_codes() rather than shape_arsenal(),
#' which is the single choke point CLAUDE.md requires, minus the parts that are
#' wrong at this grain. shape_arsenal() applies MIN_PITCH_COUNT to the frame it
#' is given and orders the factor by usage within it; given the whole league
#' that would filter on league-wide counts and order by league-wide usage, when
#' what the search needs is a per-pitcher floor it takes as an argument.
search_aggregate <- function(df, hand, team = "All") {
  if (hand != "All") df <- filter(df, stand == hand)
  # Team is filtered here, beside the batter side, and deliberately NOT added to
  # the group_by. 96 pitchers threw for more than one club in 2026, 89 of them
  # for two and 7 for three or more, several with 2,400+ pitches. Grouping by
  # team would split every one of them into two rows even with the filter off,
  # quietly changing the default view and pushing both halves toward the sample
  # floor. Filtering first means "TB sliders" reads as the shape he threw while
  # he was a Ray, and "All teams" is byte for byte what the tab did before the
  # filter existed.
  if (team != "All") df <- filter(df, pitch_team == team)
  df <- df |> filter(!is.na(pitch_type), pitch_type != "")
  df <- reconcile_pitch_codes(df)

  df |>
    group_by(pitcher, player_name, p_throws, pitch_type) |>
    summarise(
      # Named `pitches` rather than `n` because resolve_cell() looks its
      # denominator up by name out of DENOM_COLS. A column called n would
      # resolve to no sample at all and grey every cell in the table.
      pitches   = n(),
      # Every club he threw this pitch for inside the window, not just the last
      # one. A pitcher traded mid-season reads "NYM/TB" under All teams, which is
      # the honest label for a row that averages both.
      team      = paste(sort(unique(pitch_team)), collapse = "/"),
      swings    = sum(description %in% swing_only),
      oz        = sum(in_zone == 0),
      pa        = sum(woba_denom, na.rm = TRUE),
      velo      = mean(release_speed,     na.rm = TRUE),
      ivb       = mean(ivb,               na.rm = TRUE),
      hb        = mean(hb,                na.rm = TRUE),
      spin      = mean(release_spin_rate, na.rm = TRUE),
      ext       = mean(release_extension, na.rm = TRUE),
      whiff_pct = pct_or_na(sum(description %in% whiff_desc),
                            sum(description %in% swing_only)),
      chase_pct = pct_or_na(sum(in_zone == 0 & description %in% swing_only),
                            sum(in_zone == 0)),
      xwoba     = { d <- sum(woba_denom, na.rm = TRUE)
                    if (d > 0) sum(estimated_woba_using_speedangle * woba_denom,
                                   na.rm = TRUE) / d else NA_real_ },
      .groups   = "drop"
    )
}


#' The slider bounds for one pitcher hand and one pitch type
#'
#' Seeded from what that group actually throws rather than from a typed
#' threshold. Both of the queries this feature was built for return zero
#' pitchers read as hard floors: no righty clears 95 mph, 18 IVB and 12 HB at
#' once, and lefty curveball HB tops out near 12.6 against a typed 16. A slider
#' that spans the observed range cannot be dragged into an empty query by
#' accident, and it shows the reader where the population sits.
#'
#' It also disposes of the HB sign trap without normalising anything. HB is
#' arm-side positive for a righty only, so a lefty curveball reads positive and
#' a righty curveball negative. The slider spans the real numbers for the hand on
#' screen, so the reader never has to know the convention.
#'
#' Bounds are rounded OUTWARD to the trait's step, so the extremes stay
#' reachable. Rounding inward would make the hardest thrower in the league
#' unselectable, which is the opposite of the point.
#'
#' Returns one row per trait with lo, hi and mid. A group with one pitcher, or
#' with every pitcher identical, gets one step of width so the slider still has
#' two ends.
search_ranges <- function(pool, p_throws, pitch_type, min_pitches = 25) {
  rows <- pool[pool$p_throws == p_throws &
                 as.character(pool$pitch_type) == pitch_type &
                 pool$pitches >= min_pitches, , drop = FALSE]

  out <- lapply(seq_len(nrow(SEARCH_TRAITS)), function(i) {
    tr <- SEARCH_TRAITS$trait[i]; st <- SEARCH_TRAITS$step[i]
    v  <- rows[[tr]]
    v  <- v[is.finite(v)]
    if (!length(v)) return(data.frame(trait = tr, lo = 0, hi = st, mid = 0,
                                      n = 0L, stringsAsFactors = FALSE))
    lo <- floor(min(v) / st) * st
    hi <- ceiling(max(v) / st) * st
    if (hi <= lo) hi <- lo + st
    data.frame(trait = tr, lo = lo, hi = hi, mid = round(stats::median(v) / st) * st,
               n = length(v), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}


#' Pitchers the sliders can never return, and why
#'
#' A trait with no reading cannot satisfy a range, so search_filter() drops the
#' row. That is the right arithmetic and the wrong silence: at full range the
#' reader has narrowed nothing and still gets fewer pitchers than the population,
#' with nothing on the page to say so. Hard rule 5, report the n.
#'
#' Measured 2026-08-23: three rows league-wide, all of them Ty Blach and all of
#' them spin, and all three sit under the default 25-pitch floor, so the default
#' view is unaffected today. Lower the floor and he disappears.
#'
#' Returns one row per trait that is missing anywhere, so the note can name the
#' reading rather than saying "a trait".
search_missing <- function(pool, p_throws, pitch_type, min_pitches = 25) {
  rows <- pool[pool$p_throws == p_throws &
                 as.character(pool$pitch_type) == pitch_type &
                 pool$pitches >= min_pitches, , drop = FALSE]
  n <- vapply(SEARCH_TRAITS$trait, function(tr) sum(!is.finite(rows[[tr]])), integer(1))
  out <- data.frame(trait = SEARCH_TRAITS$trait, n = unname(n), stringsAsFactors = FALSE)
  out[out$n > 0, , drop = FALSE]
}


#' A slider bound, printed at the trait's own precision
#'
#' Spin wants 2,725 and extension wants 6.5, and one sprintf cannot do both.
#' Its own function because the slider label and the table have to agree: a
#' label reading 101.6 over a column reading 102 is a bug report waiting to
#' happen.
fmt_bound <- function(x, digits) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}


#' Filter the pool down to the pitchers matching a query
#'
#' `bounds` is a named list of two-element numeric vectors, one per trait, as a
#' pair of sliderInput values arrives. A trait missing from the list is not
#' filtered on, so a partial query is a legal query.
#'
#' Both ends are inclusive, which is what a slider implies: dragging to 95.0 and
#' asking for 95+ has to include the pitcher who sits at exactly 95.0.
#'
#' `min_pitches` is an argument rather than a constant because the right floor
#' for a full season and for a two-week window are not the same number. A
#' pitcher with four four-seams in a window has a mean shape that is noise, and
#' at this grain the noise looks exactly like a finding.
#' `sort_by` and `desc` order the WHOLE match set, before anything caps it.
#' Sorting after the cap would re-order the top 50 by pitch count rather than
#' finding the 50 highest whiff rates, which is a different and much less useful
#' table wearing the same header.
search_filter <- function(pool, p_throws, pitch_type, bounds = list(),
                          min_pitches = 25, sort_by = "pitches", desc = TRUE) {
  keep <- pool$p_throws == p_throws &
    as.character(pool$pitch_type) == pitch_type &
    pool$pitches >= min_pitches
  out <- pool[keep, , drop = FALSE]

  for (tr in names(bounds)) {
    b <- bounds[[tr]]
    if (is.null(b) || length(b) != 2 || !all(is.finite(b))) next
    v <- out[[tr]]
    # NA on a trait means the pitch was never measured for it, which is not a
    # match. Keeping it would put a blank row in a table of shapes.
    out <- out[!is.na(v) & v >= b[1] & v <= b[2], , drop = FALSE]
  }

  # order(decreasing =) rather than order(-x). Negation is fine on the numeric
  # columns and an error on PITCHER and TEAM, which are the two the header click
  # makes sortable alongside the rest. NA sorts last in BOTH directions, which is
  # what `na.last = TRUE` buys: a pitcher with no spin reading belongs at the
  # bottom of a spin sort, not at the top of the ascending one.
  if (sort_by %in% names(out)) {
    out <- out[order(out[[sort_by]], decreasing = desc, na.last = TRUE), , drop = FALSE]
  }
  out
}


#' League context for a search table, one column at a time
#'
#' resolve_table() cannot be reused here and the reason is worth stating: it
#' matches its denominator frame to the table BY PITCH TYPE, because an arsenal
#' table has one row per pitch type. A search table has one row per PITCHER and
#' a single shared pitch type, so that match would collapse every row onto the
#' first. resolve_column() is the right grain, and it already takes values,
#' pitch types and denominators as parallel vectors.
#'
#' The percentile means what it looks like here, which it does not in the
#' arsenal table. Every row is the same pitch type and the same pitcher hand, so
#' a green cell and a red cell are ranked against the same population. That is
#' the reason the pitch type dropdown takes one value and has no All.
resolve_search <- function(tbl, ref, p_throws, stand, pitch_type) {
  cols <- SEARCH_METRIC_COLS[names(SEARCH_METRIC_COLS) %in% names(tbl)]
  pt   <- rep(pitch_type, nrow(tbl))
  dn   <- as.data.frame(tbl)[, intersect(DENOM_COLS, names(tbl)), drop = FALSE]

  cells <- do.call(rbind, lapply(seq_along(cols), function(k) {
    col <- names(cols)[k]
    out <- resolve_column(ref, tbl[[col]], unname(cols[k]), pt,
                          p_throws, stand, "All Counts", dn)
    cbind(column = col, metric = unname(cols[k]), row = seq_len(nrow(tbl)),
          out, stringsAsFactors = FALSE)
  }))

  list(cells = cells, notes = state_notes(cells), col_notes = col_notes_for(cols))
}


#' Render a search result as a gt
#'
#' Rows are pitchers and the pitch type is fixed, which flips two things around
#' from arsenal_gt(). There is no per-row pitch colour, because a row is not a
#' pitch type, so the percentile fills own the body outright and the pitch
#' identity lives in the header instead. And the first column is a name rather
#' than a code, so it carries the click.
#'
#' The click is an onclick injected through text_transform, not DT. gt renders
#' HTML and Shiny is already on the page, so `Shiny.setInputValue` is one
#' attribute and no new package. DT would put a second table aesthetic into a UI
#' that is gt everywhere and add a package to renv.lock and to the deploy
#' manifest, to buy a row selection this does in one line.
#'
#' text_transform rather than fmt for the same reason arsenal_gt() uses it: fmt
#' does not compound, so a second fmt on a column would silently win and drop
#' the formatting already applied.
search_gt <- function(tbl, pitch_type, p_throws, hand, ref = NULL,
                      label = hand_label(hand), input_id = "search_pick",
                      max_rows = SEARCH_MAX_ROWS, n_total = nrow(tbl),
                      sort_by = "pitches", desc = TRUE,
                      sort_id = "search_sort") {
  hand_word <- if (p_throws == "R") "RHP" else "LHP"

  # The cap lives here rather than in the caller so no caller can forget it. An
  # unfiltered search is 457 righty four-seams, which renders in 5.5 s and ships
  # 2.7 MB of HTML. The count above the table still reports the true total.
  capped <- n_total > nrow(tbl) || nrow(tbl) > max_rows
  tbl    <- utils::head(tbl, max_rows)

  # Every column header is a sort control, by the same route the name cell uses:
  # an onclick that hands Shiny a column name. The alternative is
  # gt::opt_interactive(), which swaps the whole table for a reactable and takes
  # the percentile fills and footnotes with it, to buy a sort this does in a
  # span. The arrow marks the column in force, so the table says how it is
  # ordered instead of leaving the reader to infer it.
  headers <- c(player_name = "PITCHER", team = "TEAM", pitches = "N", velo = "VELO",
               ivb = "IVB", hb = "HB", spin = "SPIN", ext = "EXT",
               whiff_pct = "WHIFF%", chase_pct = "CHASE%", xwoba = "xwOBA")
  labs <- lapply(names(headers), function(cl) {
    arrow <- if (identical(cl, sort_by)) if (desc) " ▾" else " ▴" else ""
    gt::html(sprintf(
      '<span style="cursor:pointer;" title="Sort by %s" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority:\'event\'})">%s%s</span>',
      headers[[cl]], sort_id, cl, headers[[cl]], arrow))
  })
  names(labs) <- names(headers)

  g <- tbl |>
    dplyr::select(player_name, team, pitches, velo, ivb, hb, spin, ext,
                  whiff_pct, chase_pct, xwoba) |>
    gt() |>
    cols_label(.list = labs) |>
    tab_header(title = paste0(hand_word, " ", pitch_type, " (", label, ")")) |>
    cols_align("center") |>
    cols_align("left", columns = player_name) |>
    cols_width(player_name ~ px(170), team ~ px(64), everything() ~ px(78)) |>
    fmt_number(columns = c(velo, ivb, hb, ext), decimals = 1) |>
    fmt_number(columns = c(spin), decimals = 0) |>
    fmt_number(columns = c(whiff_pct, chase_pct), decimals = 1) |>
    # Leading zero dropped, the usual convention for a rate bounded below one,
    # matching arsenal_gt().
    fmt(columns = xwoba, fns = \(x) sub("^0", "", sprintf("%.3f", x))) |>
    tab_style(cell_borders(sides = c("top", "bottom"), color = "black", weight = px(2)),
              cells_column_labels()) |>
    tab_style(cell_text(weight = "bold"), cells_body(columns = player_name)) |>
    tab_options(
      table.font.size = 13, heading.title.font.size = 15, heading.align = "left",
      column_labels.font.weight = "bold", column_labels.background.color = "gray95",
      table.border.top.color = "transparent"
    ) |>
    # The pitch identity, once, since every row shares it.
    tab_style(cell_text(color = pitch_text_colors[[pitch_type]], weight = "bold"),
              cells_title(groups = "title"))

  if (!is.null(ref)) {
    # After every other style, never before. The later tab_style wins in gt, so
    # anything applied afterwards would overwrite the percentile fills. Same
    # ordering rule as arsenal_gt(), for the same reason.
    #
    # BATCHED by style, one tab_style per distinct look per column, rather than
    # arsenal_gt()'s one per cell. An arsenal table has about 50 cells and the
    # difference does not show; a search table has one row per pitcher, and the
    # per-cell version was measured at 44.7 s and 2.7 MB of HTML on 457 righty
    # four-seams. Grouping identical looks takes the same table to 1.4 s.
    ce <- ref$cells
    ce$key <- paste(ce$column, ce$fill, ce$text_color, ce$font_style, ce$font_weight)
    g <- reduce(unique(ce$key), function(gt_tbl, k) {
      grp  <- ce[ce$key == k, ]
      rows <- grp$row
      gt_tbl |>
        tab_style(cell_fill(color = grp$fill[1]),
                  cells_body(columns = grp$column[1], rows = rows)) |>
        tab_style(cell_text(color = grp$text_color[1], style = grp$font_style[1],
                            weight = grp$font_weight[1]),
                  cells_body(columns = grp$column[1], rows = rows))
    }, .init = g)

    # Batched the same way, by column and marker.
    mk <- ref$cells[nzchar(ref$cells$marker), , drop = FALSE]
    if (nrow(mk)) {
      mk$key <- paste(mk$column, mk$marker)
      g <- reduce(unique(mk$key), function(gt_tbl, k) {
        grp <- mk[mk$key == k, ]
        m   <- grp$marker[1]
        gt_tbl |> text_transform(cells_body(columns = grp$column[1], rows = grp$row),
                                 fn = function(x) paste0(x, m))
      }, .init = g)
    }

    g <- reduce(names(ref$col_notes), function(gt_tbl, col) {
      gt_tbl |> tab_footnote(ref$col_notes[[col]], cells_column_labels(columns = col))
    }, .init = g)
    g <- reduce(ref$notes, function(gt_tbl, n) tab_source_note(gt_tbl, n), .init = g)
  }

  # Applied last so the ids line up with the rows as rendered. The payload is
  # the pitcher id and nothing else: the server looks the name up itself rather
  # than trusting a string that came back from the page.
  ids <- tbl$pitcher
  g <- g |>
    text_transform(
      cells_body(columns = player_name),
      fn = function(x) {
        vapply(seq_along(x), function(i) sprintf(
          '<span style="cursor:pointer; border-bottom:1px dotted #666;" title="Load this pitcher" onclick="Shiny.setInputValue(\'%s\', %d, {priority:\'event\'})">%s</span>',
          input_id, ids[i], format_player_name(x[i])), character(1))
      }) |>
    tab_source_note("Click a name to load that pitcher into the other tabs.")

  if (capped) {
    # The note has to name the column in force. It read "the 50 with the most
    # pitches" whatever the sort, which was true only until the first header
    # click and then quietly described a table nobody was looking at.
    by <- if (!is.null(headers[[sort_by]])) headers[[sort_by]] else sort_by
    g <- g |> tab_source_note(paste0(
      "Showing the ", if (desc) "top " else "bottom ", max_rows, " by ", by,
      ", of ", n_total, " matches. Narrow a slider to see the rest."))
  }
  g
}
