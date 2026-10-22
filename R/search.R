# search.R
# League-wide pitch trait search: "who throws righty four-seams at 95+ with 18 IVB?"
# `p_throws` is the pitcher's hand, `stand` or `hand` the batter's. Pass them by name.

library(dplyr)


# Every column search_aggregate() reads
SEARCH_AGG_COLS <- c("pitcher", "player_name", "p_throws", "pitch_type",
                     "pitch_team", "stand", "description", "in_zone",
                     "woba_denom", "release_speed", "ivb", "hb",
                     "release_spin_rate", "release_extension", "release_pos_z",
                     "vaa", "release_pos_x", "estimated_woba_using_speedangle")


#' One row per pitcher and pitch type for the whole league, for the batter side
#' `hand`. Also feeds the Context tab.
search_aggregate <- function(df, hand, team = "All", from = NULL, to = NULL) {
  # One row mask and one narrow copy (chained filter() copies exhausted the 1 GB instance)
  keep <- !is.na(df$pitch_type) & df$pitch_type != ""
  if (!is.null(from)) keep <- keep & df$game_date >= from
  if (!is.null(to))   keep <- keep & df$game_date <= to
  if (hand != "All")  keep <- keep & df$stand %in% hand
  # Team filters rows but isn't grouped on: a traded pitcher stays one row
  if (team != "All") keep <- keep & df$pitch_team %in% team
  # Reconcile pitch codes on the code column only, then subset once
  idx <- which(keep)
  rc  <- reconcile_pitch_codes(data.frame(pitch_type = df$pitch_type[idx],
                                          .row = idx, stringsAsFactors = FALSE))
  df  <- df[rc$.row, intersect(SEARCH_AGG_COLS, names(df)), drop = FALSE]
  df$pitch_type <- rc$pitch_type

  df |>
    group_by(pitcher, player_name, p_throws, pitch_type) |>
    summarise(
      # `pitches` is the denominator name resolve_cell() looks up
      pitches   = n(),
      # Every club he threw for in the window, e.g. "NYM/TB"
      team      = paste(sort(unique(pitch_team)), collapse = "/"),
      swings    = sum(description %in% swing_only),
      oz        = sum(in_zone == 0),
      pa        = sum(woba_denom, na.rm = TRUE),
      velo      = mean(release_speed,     na.rm = TRUE),
      ivb       = mean(ivb,               na.rm = TRUE),
      hb        = mean(hb,                na.rm = TRUE),
      spin      = mean(release_spin_rate, na.rm = TRUE),
      ext       = mean(release_extension, na.rm = TRUE),
      rel_ht    = mean(release_pos_z,      na.rm = TRUE),
      vaa       = mean(vaa,                na.rm = TRUE),
      # Positive toward third base, as in the traits table
      rel_side  = -mean(release_pos_x,     na.rm = TRUE),
      zone_pct  = pct_or_na(sum(in_zone == 1), n()),
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


#' Slider bounds for one pitcher hand and pitch type, from what that group throws.
#' Rounded outward to the step.
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


#' Pitchers dropped for a missing trait reading, per trait
search_missing <- function(pool, p_throws, pitch_type, min_pitches = 25) {
  rows <- pool[pool$p_throws == p_throws &
                 as.character(pool$pitch_type) == pitch_type &
                 pool$pitches >= min_pitches, , drop = FALSE]
  n <- vapply(SEARCH_TRAITS$trait, function(tr) sum(!is.finite(rows[[tr]])), integer(1))
  out <- data.frame(trait = SEARCH_TRAITS$trait, n = unname(n), stringsAsFactors = FALSE)
  out[out$n > 0, , drop = FALSE]
}


#' Pitchers matching a query. `bounds` holds two-element ranges per trait, both ends
#' inclusive; a missing trait is not filtered. Sorts the full match set before any cap.
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
    # A trait never measured is not a match
    out <- out[!is.na(v) & v >= b[1] & v <= b[2], , drop = FALSE]
  }

  if (sort_by %in% names(out)) {
    # decreasing= works on text columns too; NA sorts last
    out <- out[order(out[[sort_by]], decreasing = desc, na.last = TRUE), , drop = FALSE]
  }
  out
}


#' Search results as a gt table. Names are clickable and every header sorts.
#' Rows beyond `max_rows` are dropped; the note says how many.
search_gt <- function(tbl, pitch_type, p_throws, hand,
                      label = hand_label(hand), input_id = "search_pick",
                      max_rows = SEARCH_MAX_ROWS, n_total = nrow(tbl),
                      sort_by = "pitches", desc = TRUE,
                      sort_id = "search_sort") {
  hand_word <- if (p_throws == "R") "RHP" else "LHP"

  capped <- n_total > nrow(tbl) || nrow(tbl) > max_rows
  tbl    <- utils::head(tbl, max_rows)

  # Clickable sort headers; the arrow marks the active column
  headers <- c(player_name = "PITCHER", team = "TEAM", pitches = "N", velo = "VELO",
               ivb = "IVB", hb = "HB", spin = "SPIN", ext = "EXT", rel_ht = "REL HT",
               whiff_pct = "WHIFF%", chase_pct = "CHASE%", xwoba = "xwOBA")
  labs <- lapply(names(headers), function(cl) {
    arrow <- if (identical(cl, sort_by)) if (desc) " ▾" else " ▴" else ""
    gt::html(sprintf(
      '<span style="cursor:pointer;" title="Sort by %s" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority:\'event\'})">%s%s</span>',
      headers[[cl]], sort_id, cl, headers[[cl]], arrow))
  })
  names(labs) <- names(headers)

  g <- tbl |>
    dplyr::select(player_name, team, pitches, velo, ivb, hb, spin, ext, rel_ht,
                  whiff_pct, chase_pct, xwoba) |>
    gt() |>
    cols_label(.list = labs) |>
    tab_header(title = paste0(hand_word, " ", pitch_type, " (", label, ")")) |>
    cols_align("center") |>
    cols_align("left", columns = player_name) |>
    cols_width(player_name ~ px(170), team ~ px(64), everything() ~ px(78)) |>
    fmt_number(columns = c(velo, ivb, hb, ext), decimals = 1) |>
    # Two decimals, matching the traits table
    fmt_number(columns = c(rel_ht), decimals = 2) |>
    fmt_number(columns = c(spin), decimals = 0) |>
    fmt_number(columns = c(whiff_pct, chase_pct), decimals = 1) |>
    # Drop the leading zero on xwOBA
    fmt(columns = xwoba, fns = \(x) sub("^0", "", sprintf("%.3f", x))) |>
    tab_style(cell_borders(sides = c("top", "bottom"), color = "black", weight = px(2)),
              cells_column_labels()) |>
    tab_style(cell_text(weight = "bold"), cells_body(columns = player_name)) |>
    tab_options(
      table.font.size = 13, heading.title.font.size = 15, heading.align = "left",
      column_labels.font.weight = "bold", column_labels.background.color = "gray95",
      table.border.top.color = "transparent"
    ) |>
    tab_style(cell_text(color = pitch_text_colors[[pitch_type]], weight = "bold"),
              cells_title(groups = "title"))

  # A click sends the pitcher id
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
    # Name the active sort column in the cap note
    by <- if (!is.null(headers[[sort_by]])) headers[[sort_by]] else sort_by
    g <- g |> tab_source_note(paste0(
      "Showing the ", if (desc) "top " else "bottom ", max_rows, " by ", by,
      ", of ", n_total, " matches. Narrow a slider to see the rest."))
  }
  g
}
