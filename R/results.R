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
    n_bip   = nrow(bip),
    # xwOBA's own denominator, returned so the panel can decide whether the
    # number is worth ranking against the league. Every other metric's
    # denominator was already here; this one was computed and thrown away.
    pa      = sum(df$woba_denom, na.rm = TRUE)
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
#' @param ctx optional, one row per metric from results_context(). NULL renders
#'   the panel exactly as it rendered before league context existed, which is
#'   what keeps every other caller and the whole of phase6 working unchanged.
results_panel <- function(sc, gl, dates, hand, log_through = NULL, ctx = NULL) {
  win <- paste(format(as.Date(dates[1]), "%b %e"), "to",
               format(as.Date(dates[2]), "%b %e"))
  win <- gsub("  ", " ", win)

  # The fill for one metric, or nothing. PCTILE_UNFILLED is white, and painting
  # white would draw a box around a number the app is declining to rank, so the
  # unfilled state paints nothing at all.
  fill_of <- function(metric) {
    if (is.null(ctx)) return(NULL)
    hit <- ctx[ctx$metric == metric, , drop = FALSE]
    if (!nrow(hit) || identical(hit$fill[1], PCTILE_UNFILLED)) return(NULL)
    hit$fill[1]
  }

  num <- function(x, fmt) if (!is.finite(x)) "--" else sprintf(fmt, x)
  cell <- function(lab, val, metric = NULL) {
    f <- if (is.null(metric)) NULL else fill_of(metric)
    shiny::tags$div(
      class = if (is.null(f)) "rp-cell" else "rp-cell rp-filled",
      style = if (is.null(f)) NULL else paste0("background:", f, ";"),
      shiny::tags$div(class = "rp-val", val),
      shiny::tags$div(class = "rp-lab", lab))
  }

  sc_row <- shiny::tags$div(
    class = "rp-row",
    shiny::tags$div(class = "rp-head",
      paste0(win, "  ·  ", hand_label(hand))),
    shiny::tags$div(class = "rp-cells",
      # TBF takes no fill on purpose. It is the sample the other three are
      # measured over, not a result, and colouring it would say a busy reliever
      # is better than a good one.
      cell("TBF",   format(sc$tbf, big.mark = ",")),
      cell("K-BB%", num(sc$k_bb,  "%.1f"), "k_bb"),
      cell("HH%",   num(sc$hh,    "%.1f"), "hh"),
      cell("xwOBA", if (!is.finite(sc$xwoba)) "--" else sub("^0", "", sprintf("%.3f", sc$xwoba)),
           "xwoba")),
    shiny::tags$div(class = "rp-src", "Statcast, this window and this batter side."))

  gl_cells <- if (!gl$have) {
    # Absence, said out loud. Not zeros, not blanks: a pitcher with no start in
    # the window has no ERA, which is a different statement from an ERA of 0.00.
    shiny::tags$div(class = "rp-none",
      "No game log in this window. IP, ERA, WHIP and FIP need a completed game.")
  } else {
    shiny::tags$div(class = "rp-cells",
      cell("IP",   gl$ip,               "ip"),
      cell("ERA",  num(gl$era,  "%.2f"), "era"),
      cell("WHIP", num(gl$whip, "%.2f"), "whip"),
      cell("FIP",  num(gl$fip,  "%.2f"), "fip"))
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

  # One line, once, under both rows rather than under each. It says what the
  # colour means AND what it was measured against, because "vs league" is
  # ambiguous the moment the date range moves: the reference is this window, not
  # the season.
  key <- if (is.null(ctx)) NULL else shiny::tags$div(
    class = "rp-key",
    "Red better, blue worse, against the league over these same dates. ",
    "Unshaded means too small a sample in this window to rank.")

  shiny::tags$div(class = "rp", sc_row, gl_row, key)
}

RESULTS_PANEL_CSS <- "
.rp { margin: 0; }
.rp-row { border: 1px solid #d8d8d8; border-radius: 4px; padding: 10px 12px; margin-bottom: 8px; background: #fff; }
.rp-head { font-size: 13px; font-weight: 700; color: #111; margin-bottom: 8px; letter-spacing: .01em; }
/* A grid rather than a flex row with a fixed gap. The panel now lives in a
   290px sidebar column, where four cells at 62px plus a 34px gap needed 350px
   and wrapped into a ragged 3-plus-1. Two even columns wrap deliberately, and
   the same rule still fills a wide container. */
.rp-cells { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 6px; }
.rp-cell { min-width: 0; padding: 5px 7px; border-radius: 3px; }
/* Filled cells sit on a tinted block, so the unfilled ones must keep the same
   box to stay aligned with them. The tint is the only difference. */
.rp-filled { box-shadow: inset 0 0 0 1px rgba(0,0,0,.05); }
.rp-val { font-size: 19px; font-weight: 700; font-variant-numeric: tabular-nums; color: #111; line-height: 1.15; }
.rp-lab { font-size: 10px; font-weight: 700; color: #666; letter-spacing: .06em; }
.rp-src { font-size: 10px; color: #767676; margin-top: 8px; line-height: 1.35; }
.rp-none { font-size: 12px; color: #767676; font-style: italic; padding: 4px 0; }
.rp-key { font-size: 10px; color: #767676; line-height: 1.35; margin-top: 2px; }
"


# ---- League context for the panel --------------------------------------------

#' The league, over the same window and batter side the panel is showing
#'
#' Forced by IP. It is a counting stat, so a season reference paints every
#' pitcher deep blue in any short window: the league median IP over the last 14
#' days is 5.7 against a season median of 29.7. Recomputing the league over the
#' selected window fixes IP and makes every rate a like-for-like comparison.
#'
#' The Statcast half narrows by batter side and the game-log half does not, for
#' the same structural reason results_gamelog() takes no hand argument: a game
#' log has no platoon split, so a vs-RHH league ERA does not exist.
#'
#' Returns one entry per metric holding the qualified pitchers' values, the floor
#' that qualified them, and the count behind it. The floor is returned rather
#' than recomputed downstream so the panel and the reference cannot disagree
#' about who was included.
results_league <- function(ad, gl, dates, hand, fip_const) {
  from <- as.character(dates[1]); to <- as.character(dates[2])

  a <- ad |> filter(game_date >= from, game_date <= to)
  if (hand != "All") a <- filter(a, stand == hand)

  # Computed here rather than by looping results_statcast() over 818 pitchers,
  # which would be the same arithmetic 818 times. The definitions still come
  # from one place: any change to a rate below has to change results_statcast()
  # too, and tests/phase6_results.R asserts the two agree on a real pitcher.
  sc <- a |>
    group_by(pitcher) |>
    summarise(
      tbf   = sum(events != "" & !is.na(events)),
      bbe   = sum(type == "X"),
      pa    = sum(woba_denom, na.rm = TRUE),
      k_bb  = pct_or_na(sum(events == "strikeout", na.rm = TRUE) -
                          sum(events == "walk", na.rm = TRUE),
                        sum(events != "" & !is.na(events))),
      hh    = pct_or_na(sum(type == "X" & launch_speed >= HARD_HIT_MPH, na.rm = TRUE),
                        sum(type == "X")),
      xwoba = { d <- sum(woba_denom, na.rm = TRUE)
                if (d > 0) sum(estimated_woba_using_speedangle * woba_denom,
                               na.rm = TRUE) / d else NA_real_ },
      .groups = "drop")

  # Written the same shape as results_gamelog() rather than through a rate
  # helper, so the league formula and the pitcher formula can be read side by
  # side and seen to match. tests/phase6_results.R asserts they agree on a real
  # pitcher, which is what actually holds them together.
  g <- if (is.null(gl)) NULL else gl |>
    filter(game_date >= from, game_date <= to) |>
    group_by(pitcher) |>
    summarise(games = n(),
              outs  = sum(ip_outs, na.rm = TRUE),
              ip    = outs / 3,
              era   = if (outs > 0) sum(er, na.rm = TRUE) / (outs / 3) * 9 else NA_real_,
              whip  = if (outs > 0) (sum(bb, na.rm = TRUE) + sum(h, na.rm = TRUE)) /
                                      (outs / 3) else NA_real_,
              fip   = if (outs > 0) (13 * sum(hr, na.rm = TRUE) +
                                     3 * (sum(bb, na.rm = TRUE) + sum(hbp, na.rm = TRUE)) -
                                     2 * sum(so, na.rm = TRUE)) / (outs / 3) + fip_const
                      else NA_real_,
              .groups = "drop")

  out <- lapply(seq_len(nrow(RESULTS_METRIC_SPEC)), function(i) {
    s <- RESULTS_METRIC_SPEC[i, ]
    df <- if (s$source == "sc") sc else g
    if (is.null(df) || !nrow(df)) {
      return(list(values = numeric(0), floor = s$min_denom, n = 0L))
    }
    dn <- df[[s$denom]]
    # Half the median among everyone who appeared, floored at the absolute
    # minimum. The median is taken over the same filtered frame, so it scales
    # with the window, with the season's pace, and with a hand filter halving
    # everyone's TBF at once.
    fl <- if (s$scales) {
      max(s$min_denom, round(stats::median(dn[is.finite(dn)], na.rm = TRUE) *
                               RESULTS_FLOOR_FRACTION, 1))
    } else s$min_denom
    v <- df[[s$metric]][is.finite(dn) & dn >= fl]
    list(values = v[is.finite(v)], floor = fl, n = sum(is.finite(v)))
  })
  names(out) <- RESULTS_METRIC_SPEC$metric
  out
}


#' Where this pitcher sits, one row per metric
#'
#' Below its floor a metric gets no percentile and no fill, matching the arsenal
#' and usage tables: a K-BB% off twelve batters is not a rank, and colouring it
#' is the failure hard rule 5 exists to prevent. The number still shows.
#'
#' The percentile is a share of the qualified league at or below this value.
#' pctile_fill() inverts it for a low-is-good metric, so a .240 xwOBA and a 28%
#' K-BB% both read red without either one having its own colour rule.
results_context <- function(sc, gl, league) {
  # results_gamelog() carries IP as OUTS plus a formatted string, never as a
  # decimal, because IP is written in thirds and a decimal IP is the trap
  # CLAUDE.md opens with. So the numeric one is derived here rather than added
  # to that contract, which tests/phase6_results.R pins.
  gl_ip <- if (isTRUE(gl$have)) gl$ip_outs / 3 else NA_real_

  own <- function(nm) {
    if (nm %in% c("k_bb", "hh", "xwoba")) sc[[nm]]
    else if (!isTRUE(gl$have)) NA_real_
    else if (nm == "ip") gl_ip else gl[[nm]]
  }
  own_denom <- function(nm) {
    s <- RESULTS_METRIC_SPEC$denom[RESULTS_METRIC_SPEC$metric == nm]
    # bbe is called n_bip on the pitcher side and bbe on the league side. Mapped
    # here in one place rather than renaming a field phase6 already pins.
    if (s == "bbe") return(sc$n_bip)
    if (s %in% c("tbf", "pa")) return(sc[[s]])
    if (!isTRUE(gl$have)) return(0)
    if (s == "games") gl$games else gl_ip
  }

  rows <- lapply(seq_len(nrow(RESULTS_METRIC_SPEC)), function(i) {
    s   <- RESULTS_METRIC_SPEC[i, ]
    lg  <- league[[s$metric]]
    val <- own(s$metric); dn <- own_denom(s$metric)
    ok  <- is.finite(val) && is.finite(dn) && dn >= lg$floor && length(lg$values) > 0
    p   <- if (ok) 100 * mean(lg$values <= val, na.rm = TRUE) else NA_real_
    data.frame(metric = s$metric, pctile = p, floor = lg$floor, n_league = lg$n,
               fill = if (ok) pctile_fill(p, s$direction) else PCTILE_UNFILLED,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
