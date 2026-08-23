# results.R
#
# The results panel. Two sources that do not support the same cuts, kept
# deliberately separate rather than merged into one row of numbers.
#
#   Statcast, from app_data : TBF, K-BB%, HH%, xwOBA. Respects the date range
#                             AND the batter side.
#   Game logs, from StatsAPI: IP, ERA, WHIP, FIP. Respects the date range only.
#
# A game log has no platoon breakdown at all, so there is no vs-RHH ERA to be
# had. The panel's job is to make that visible rather than to hide it: see
# results_gamelog()'s label, which tracks the dates and deliberately ignores the
# side.
#
# Requires theme.R for swing_only and whiff_desc.

library(dplyr)

# Hard hit is 95 mph or more off the bat, the standard Statcast threshold.
HARD_HIT_MPH <- 95


#' Outs from an innings-pitched string
#'
#' StatsAPI writes IP in the baseball convention, "5.2" meaning five innings and
#' two thirds, NOT five and two tenths. Reading it as a decimal understates a
#' .1 and overstates a .2, and the error compounds across a season. Stored as
#' outs so nothing downstream has to remember this.
ip_to_outs <- function(ip) {
  ip <- as.character(ip)
  whole <- floor(suppressWarnings(as.numeric(ip)))
  frac  <- round((suppressWarnings(as.numeric(ip)) - whole) * 10)
  stopifnot("innings-pitched fraction outside 0, 1, 2" = all(is.na(frac) | frac %in% 0:2))
  as.integer(whole * 3 + frac)
}

#' Format outs back into the baseball convention for display
outs_to_ip <- function(outs) sprintf("%d.%d", outs %/% 3, outs %% 3)


#' The Statcast half of the panel
#'
#' Filters `stand` the same way arsenal_table() does, so this narrows with the
#' toggle exactly as the rest of the app does.
#'
#' K-BB% is one number rather than K% and BB% side by side, matching the PDF
#' header. Both are off TBF, so the difference is well defined.
results_statcast <- function(df, hand) {
  if (hand != "All") df <- filter(df, stand == hand)

  # PA-ending rows only. events is empty string on non-terminal pitches, not NA.
  # See CLAUDE.md hard rule 3.
  pa <- filter(df, events != "" & !is.na(events))
  # Batted balls, not every row carrying an exit velocity. Statcast measures
  # FOULS too, and they are weak, so a tracked-EV denominator silently halves
  # the rate. Measured on Kirby 2026 vs LHH: 435 rows have an EV, 206 of them
  # fouls at a mean 78.1 mph, and HH% reads 28.3 against FanGraphs' 45.4. On
  # type == "X" it is 104 of 229, 45.4, matching FanGraphs' event count exactly
  # on both sides.
  #
  # bb_type would say the same thing and is NOT in app_data, having been dropped
  # by the Phase 7 column trim as unread. type is the in-play flag and is there.
  bip <- filter(df, type == "X")

  tbf <- nrow(pa)
  k   <- sum(pa$events == "strikeout", na.rm = TRUE)
  bb  <- sum(pa$events == "walk", na.rm = TRUE)

  list(
    tbf     = tbf,
    k_bb    = if (tbf > 0) (k - bb) / tbf * 100 else NA_real_,
    # Untracked batted balls stay in the denominator. FanGraphs counts them the
    # same way, 80 of 205 vs RHH reading 39.0 rather than 39.4 over the 203 with
    # an EV, and a number people cross-check against a public source is worth
    # more than the two rows of precision that dropping them would buy.
    hh      = if (nrow(bip) > 0)
                sum(bip$launch_speed >= HARD_HIT_MPH, na.rm = TRUE) / nrow(bip) * 100
              else NA_real_,
    # Weighted by woba_denom, PA-ending rows only. CLAUDE.md hard rule 2.
    xwoba   = { d <- sum(df$woba_denom, na.rm = TRUE)
                if (d > 0) sum(df$estimated_woba_using_speedangle * df$woba_denom,
                               na.rm = TRUE) / d else NA_real_ },
    n_bip   = nrow(bip)
  )
}


#' The FIP constant for this season's pulled logs
#'
#' cFIP is whatever makes league FIP equal league ERA, so it has to come from
#' the same population the panel's FIPs are computed against rather than being
#' hardcoded at 3.10. Computed once from the whole log file.
fip_constant <- function(logs) {
  ip <- sum(logs$ip_outs, na.rm = TRUE) / 3
  if (!is.finite(ip) || ip <= 0) return(NA_real_)
  lg_era <- sum(logs$er, na.rm = TRUE) / ip * 9
  lg_era - ((13 * sum(logs$hr, na.rm = TRUE) +
             3 * (sum(logs$bb, na.rm = TRUE) + sum(logs$hbp, na.rm = TRUE)) -
             2 * sum(logs$so, na.rm = TRUE)) / ip)
}


#' The game-log half of the panel
#'
#' Takes no `hand` argument, and that is the point rather than an omission. A
#' game log carries no platoon split, so a vs-RHH ERA does not exist at this
#' grain. Giving this function a hand argument it silently ignored is exactly
#' the failure the two-row panel is built to prevent.
#'
#' Filters at GAME level on the date window, so a window that cuts through a
#' start includes that whole start. That is the honest unit: you cannot take
#' half a game's earned runs.
#'
#' Returns `have = FALSE` when the pitcher has no game in the window, so the
#' panel can say so rather than rendering zeros.
results_gamelog <- function(logs, pitcher_id, dates, c_fip = NULL) {
  g <- logs[logs$pitcher == pitcher_id &
            logs$game_date >= as.character(dates[1]) &
            logs$game_date <= as.character(dates[2]), , drop = FALSE]

  if (!nrow(g)) {
    return(list(have = FALSE, games = 0L, ip_outs = 0L, ip = NA_character_,
                era = NA_real_, whip = NA_real_, fip = NA_real_,
                through = NA_character_))
  }

  outs <- sum(g$ip_outs, na.rm = TRUE)
  ip   <- outs / 3
  if (is.null(c_fip)) c_fip <- fip_constant(logs)

  list(
    have    = TRUE,
    games   = nrow(g),
    ip_outs = outs,
    # Displayed in the baseball convention, 5.2 meaning five and two thirds.
    ip      = outs_to_ip(outs),
    era     = if (ip > 0) sum(g$er, na.rm = TRUE) / ip * 9 else NA_real_,
    whip    = if (ip > 0) (sum(g$bb, na.rm = TRUE) + sum(g$h, na.rm = TRUE)) / ip else NA_real_,
    fip     = if (ip > 0) (13 * sum(g$hr, na.rm = TRUE) +
                           3 * (sum(g$bb, na.rm = TRUE) + sum(g$hbp, na.rm = TRUE)) -
                           2 * sum(g$so, na.rm = TRUE)) / ip + c_fip else NA_real_,
    through = max(g$game_date)
  )
}


#' Render the two-row results panel as HTML
#'
#' Two rows, each with its own header at the SAME size and weight. The headers
#' are the mechanism, not decoration.
#'
#' The Statcast header names the window and the side. The game-log header names
#' the window and says "both sides", and it updates when the dates change and
#' conspicuously does not when the side changes. A label that never moves stops
#' being read; one that visibly tracks the dates is live, so its failure to
#' respond to the toggle is itself the signal.
#'
#' Every number carries its source on the row it sits in, and the game-log row
#' prints the log's own last game date, the same way the arsenal table prints
#' the FanGraphs export window.
results_panel <- function(sc, gl, dates, hand, log_through = NULL) {
  win <- paste(format(as.Date(dates[1]), "%b %e"), "to",
               format(as.Date(dates[2]), "%b %e"))
  win <- gsub("  ", " ", win)

  num <- function(x, fmt) if (!is.finite(x)) "--" else sprintf(fmt, x)
  cell <- function(lab, val) shiny::tags$div(
    class = "rp-cell",
    shiny::tags$div(class = "rp-val", val),
    shiny::tags$div(class = "rp-lab", lab))

  sc_row <- shiny::tags$div(
    class = "rp-row",
    shiny::tags$div(class = "rp-head",
      paste0(win, "  ·  ", hand_label(hand))),
    shiny::tags$div(class = "rp-cells",
      cell("TBF",   format(sc$tbf, big.mark = ",")),
      cell("K-BB%", num(sc$k_bb,  "%.1f")),
      cell("HH%",   num(sc$hh,    "%.1f")),
      cell("xwOBA", if (!is.finite(sc$xwoba)) "--" else sub("^0", "", sprintf("%.3f", sc$xwoba)))),
    shiny::tags$div(class = "rp-src", "Statcast, this window and this batter side."))

  gl_cells <- if (!gl$have) {
    # Absence, said out loud. Not zeros, not blanks: a pitcher with no start in
    # the window has no ERA, which is a different statement from an ERA of 0.00.
    shiny::tags$div(class = "rp-none",
      "No game log in this window. IP, ERA, WHIP and FIP need a completed game.")
  } else {
    shiny::tags$div(class = "rp-cells",
      cell("IP",   gl$ip),
      cell("ERA",  num(gl$era,  "%.2f")),
      cell("WHIP", num(gl$whip, "%.2f")),
      cell("FIP",  num(gl$fip,  "%.2f")))
  }

  gl_row <- shiny::tags$div(
    class = "rp-row",
    # Same size and weight as the Statcast header. Tracks the dates, ignores the
    # side, by construction: `hand` is not read here.
    shiny::tags$div(class = "rp-head",
      paste0(win, "  ·  both sides")),
    gl_cells,
    shiny::tags$div(class = "rp-src",
      paste0("MLB game logs, whole games in this window. No platoon split exists at this grain.",
             if (gl$have) paste0(" ", gl$games, " game", if (gl$games != 1) "s" else "",
                                 ", through ", gl$through, ".") else "",
             if (!is.null(log_through)) paste0(" Log file through ", log_through, ".") else "")))

  shiny::tags$div(class = "rp", sc_row, gl_row)
}

RESULTS_PANEL_CSS <- "
.rp { margin: 0 0 18px 0; }
.rp-row { border: 1px solid #d8d8d8; border-radius: 4px; padding: 10px 14px; margin-bottom: 8px; background: #fff; }
.rp-head { font-size: 15px; font-weight: 700; color: #111; margin-bottom: 8px; letter-spacing: .01em; }
.rp-cells { display: flex; gap: 34px; flex-wrap: wrap; }
.rp-cell { min-width: 62px; }
.rp-val { font-size: 21px; font-weight: 700; font-variant-numeric: tabular-nums; color: #111; }
.rp-lab { font-size: 11px; font-weight: 700; color: #666; letter-spacing: .06em; }
.rp-src { font-size: 11px; color: #767676; margin-top: 8px; }
.rp-none { font-size: 13px; color: #767676; font-style: italic; padding: 4px 0; }
"
