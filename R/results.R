# results.R
# The results panel. Two rows from two sources:
#   Statcast (app_data): TBF, K-BB%, HH%, xwOBA. Follows dates and batter side.
#   MLB game logs:       IP, ERA, WHIP, FIP.   Follows dates only (no platoon split).

library(dplyr)

HARD_HIT_MPH <- 95


#' Outs from an innings-pitched string ("5.2" = five innings and two outs)
ip_to_outs <- function(ip) {
  ip <- as.character(ip)
  whole <- floor(suppressWarnings(as.numeric(ip)))
  frac  <- round((suppressWarnings(as.numeric(ip)) - whole) * 10)
  stopifnot("innings-pitched fraction outside 0, 1, 2" = all(is.na(frac) | frac %in% 0:2))
  as.integer(whole * 3 + frac)
}

#' Outs back to the "5.2" display form
outs_to_ip <- function(outs) sprintf("%d.%d", outs %/% 3, outs %% 3)


#' Is this row a batter faced? `events` is "" (not NA) mid-PA, and truncated_pa
#' (inning ended mid-PA) doesn't count. Intentional walks throw no pitch, so TBF
#' reads one low per IBB.
is_bf <- function(events) {
  !is.na(events) & events != "" & events != "truncated_pa"
}


#' The Statcast half of the panel
results_statcast <- function(df, hand) {
  if (hand != "All") df <- filter(df, stand == hand)

  pa <- filter(df, is_bf(events))
  # Hard hit% over batted balls (Statcast also measures exit velocity on fouls)
  bip <- filter(df, type == "X")

  tbf <- nrow(pa)
  k   <- sum(pa$events == "strikeout", na.rm = TRUE)
  bb  <- sum(pa$events == "walk", na.rm = TRUE)

  list(
    tbf     = tbf,
    k_bb    = if (tbf > 0) (k - bb) / tbf * 100 else NA_real_,
    # Untracked batted balls stay in the denominator, as on FanGraphs
    hh      = if (nrow(bip) > 0)
                sum(bip$launch_speed >= HARD_HIT_MPH, na.rm = TRUE) / nrow(bip) * 100
              else NA_real_,
    # Weighted by woba_denom, PA-ending rows only
    xwoba   = { d <- sum(df$woba_denom, na.rm = TRUE)
                if (d > 0) sum(df$estimated_woba_using_speedangle * df$woba_denom,
                               na.rm = TRUE) / d else NA_real_ },
    n_bip   = nrow(bip),
    # xwOBA's denominator
    pa      = sum(df$woba_denom, na.rm = TRUE)
  )
}


#' FIP constant from the same game logs (league FIP = league ERA)
fip_constant <- function(logs) {
  ip <- sum(logs$ip_outs, na.rm = TRUE) / 3
  if (!is.finite(ip) || ip <= 0) return(NA_real_)
  lg_era <- sum(logs$er, na.rm = TRUE) / ip * 9
  lg_era - ((13 * sum(logs$hr, na.rm = TRUE) +
             3 * (sum(logs$bb, na.rm = TRUE) + sum(logs$hbp, na.rm = TRUE)) -
             2 * sum(logs$so, na.rm = TRUE)) / ip)
}


#' The game-log half of the panel. No `hand` argument: game logs have no platoon split.
#' Filters whole games on the date window.
results_gamelog <- function(logs, pitcher_id, dates, c_fip = NULL) {
  g <- logs[logs$pitcher == pitcher_id &
            logs$game_date >= as.character(dates[1]) &
            logs$game_date <= as.character(dates[2]), , drop = FALSE]

  if (!nrow(g)) {
    return(list(have = FALSE, games = 0L, ip_outs = 0L, ip = NA_character_,
                era = NA_real_, whip = NA_real_, fip = NA_real_,
                through = NA_character_, ibb = 0L))
  }

  outs <- sum(g$ip_outs, na.rm = TRUE)
  ip   <- outs / 3
  if (is.null(c_fip)) c_fip <- fip_constant(logs)

  list(
    have    = TRUE,
    games   = nrow(g),
    ip_outs = outs,
    ip      = outs_to_ip(outs),
    era     = if (ip > 0) sum(g$er, na.rm = TRUE) / ip * 9 else NA_real_,
    whip    = if (ip > 0) (sum(g$bb, na.rm = TRUE) + sum(g$h, na.rm = TRUE)) / ip else NA_real_,
    fip     = if (ip > 0) (13 * sum(g$hr, na.rm = TRUE) +
                           3 * (sum(g$bb, na.rm = TRUE) + sum(g$hbp, na.rm = TRUE)) -
                           2 * sum(g$so, na.rm = TRUE)) / ip + c_fip else NA_real_,
    through = max(g$game_date),
    # Explains the TBF gap in the Statcast row's note
    ibb     = if (is.null(g$ibb)) 0L else sum(g$ibb, na.rm = TRUE)
  )
}


#' Render the two-row results panel as HTML. `ctx` (from results_context()) adds
#' league shading; NULL renders without it.
results_panel <- function(sc, gl, dates, hand, log_through = NULL, ctx = NULL) {
  win <- paste(format(as.Date(dates[1]), "%b %e"), "to",
               format(as.Date(dates[2]), "%b %e"))
  win <- gsub("  ", " ", win)

  # Unranked metrics get no fill
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

  # Note intentional walks only when the window has one
  n_ibb <- if (is.null(gl$ibb) || !isTRUE(gl$ibb > 0)) 0L else as.integer(gl$ibb)
  sc_src <- paste0(
    "Statcast, this window and this batter side.",
    if (n_ibb > 0)
      sprintf(" Excludes %d automatic intentional walk%s, which %s no pitch and so %s no Statcast row.",
              n_ibb,
              if (n_ibb == 1) ""      else "s",
              if (n_ibb == 1) "throws" else "throw",
              if (n_ibb == 1) "leaves" else "leave"))

  sc_row <- shiny::tags$div(
    class = "rp-row",
    shiny::tags$div(class = "rp-head",
      paste0(win, "  ·  ", hand_label(hand))),
    shiny::tags$div(class = "rp-cells",
      # TBF is the sample, not a result: no fill
      cell("TBF",   format(sc$tbf, big.mark = ",")),
      cell("K-BB%", num(sc$k_bb,  "%.1f"), "k_bb"),
      cell("HH%",   num(sc$hh,    "%.1f"), "hh"),
      cell("xwOBA", if (!is.finite(sc$xwoba)) "--" else sub("^0", "", sprintf("%.3f", sc$xwoba)),
           "xwoba")),
    shiny::tags$div(class = "rp-src", sc_src))

  gl_cells <- if (!gl$have) {
    # No game in the window
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
    shiny::tags$div(class = "rp-head",
      paste0(win, "  ·  both sides")),
    gl_cells,
    shiny::tags$div(class = "rp-src",
      paste0("MLB game logs, whole games in this window. No platoon split exists at this grain.",
             if (gl$have) paste0(" ", gl$games, " game", if (gl$games != 1) "s" else "",
                                 ", through ", gl$through, ".") else "",
             if (!is.null(log_through)) paste0(" Log file through ", log_through, ".") else "")))

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
/* Two even columns fit the narrow sidebar */
.rp-cells { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 6px; }
.rp-cell { min-width: 0; padding: 5px 7px; border-radius: 3px; }
/* Same box for filled and unfilled cells, so they align */
.rp-filled { box-shadow: inset 0 0 0 1px rgba(0,0,0,.05); }
.rp-val { font-size: 19px; font-weight: 700; font-variant-numeric: tabular-nums; color: #111; line-height: 1.15; }
.rp-lab { font-size: 10px; font-weight: 700; color: #666; letter-spacing: .06em; }
.rp-src { font-size: 10px; color: #767676; margin-top: 8px; line-height: 1.35; }
.rp-none { font-size: 12px; color: #767676; font-style: italic; padding: 4px 0; }
.rp-key { font-size: 10px; color: #767676; line-height: 1.35; margin-top: 2px; }
"


# ---- League context for the panel --------------------------------------------

#' League values over the panel's window and batter side (IP is a counting stat).
#' Floor = RESULTS_FLOOR_FRACTION of the median denominator. Game-log metrics
#' ignore batter side.
results_league <- function(ad, gl, dates, hand, fip_const) {
  from <- as.character(dates[1]); to <- as.character(dates[2])

  a <- ad |> filter(game_date >= from, game_date <= to)
  if (hand != "All") a <- filter(a, stand == hand)

  # Same definitions as results_statcast(); test_results_panel.R checks they agree
  sc <- a |>
    group_by(pitcher) |>
    summarise(
      tbf   = sum(is_bf(events)),
      bbe   = sum(type == "X"),
      pa    = sum(woba_denom, na.rm = TRUE),
      k_bb  = pct_or_na(sum(events == "strikeout", na.rm = TRUE) -
                          sum(events == "walk", na.rm = TRUE),
                        sum(is_bf(events))),
      hh    = pct_or_na(sum(type == "X" & launch_speed >= HARD_HIT_MPH, na.rm = TRUE),
                        sum(type == "X")),
      xwoba = { d <- sum(woba_denom, na.rm = TRUE)
                if (d > 0) sum(estimated_woba_using_speedangle * woba_denom,
                               na.rm = TRUE) / d else NA_real_ },
      .groups = "drop")

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


#' Percentile and fill per metric for this pitcher (none below the floor)
results_context <- function(sc, gl, league) {
  # IP is stored as outs
  gl_ip <- if (isTRUE(gl$have)) gl$ip_outs / 3 else NA_real_

  own <- function(nm) {
    if (nm %in% c("k_bb", "hh", "xwoba")) sc[[nm]]
    else if (!isTRUE(gl$have)) NA_real_
    else if (nm == "ip") gl_ip else gl[[nm]]
  }
  own_denom <- function(nm) {
    s <- RESULTS_METRIC_SPEC$denom[RESULTS_METRIC_SPEC$metric == nm]
    # Pitcher side calls it n_bip, league side bbe
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
