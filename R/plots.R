# plots.R
# ggplot charts. Each takes one pitcher's pitch-level frame and returns a plot.
# Write ggplot2::margin() in full: randomForest::margin() can mask it.

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)


#' Pitch types too rare to chart, counted over both batter sides
chart_hidden_types <- function(df, min_share = CHART_MIN_SHARE, min_n = CHART_MIN_N) {
  n <- table(as.character(df$pitch_type))
  sort(names(n)[n / sum(n) < min_share & n < min_n])
}


#' "4 SV left off the charts...", or NULL
chart_hidden_note <- function(df, hidden) {
  if (!length(hidden)) return(NULL)
  n <- table(as.character(df$pitch_type))[hidden]
  paste0(paste(sprintf("%d %s", as.integer(n), hidden), collapse = ", "),
         " left off the charts, under 1% of pitches; still in the tables")
}


#' Two-sided usage bars, LHH left and RHH right. Hidden types are dropped after
#' shares are computed.
plot_usage <- function(df, hide = character()) {
  usage <- df |>
    count(pitch_type, stand) |>
    complete(pitch_type, stand, fill = list(n = 0)) |>
    group_by(stand) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup() |>
    # LHH bars run left
    mutate(plot_pct = if_else(stand == "L", -pct, pct))
  if (length(hide)) {
    usage <- usage |> filter(!pitch_type %in% hide) |> mutate(pitch_type = droplevels(pitch_type))
  }
  # Axis fits the longest bar plus its label
  lim <- max(50, ceiling((max(usage$pct, na.rm = TRUE) + 15) / 25) * 25)
  ggplot(usage, aes(plot_pct, fct_rev(pitch_type), fill = pitch_type)) +
    geom_vline(xintercept = setdiff(seq(-lim, lim, 25), c(-lim, lim)), linetype = "dashed", color = "gray80", linewidth = 0.4) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(round(abs(plot_pct), 1), "%"),
                  hjust = if_else(stand == "L", 1.15, -0.15)),
              size = 5, fontface = "bold", color = "gray20") +
    scale_x_continuous(limits = c(-lim, lim), breaks = seq(-100, 100, 25),
                       labels = \(x) paste0(abs(x), "%")) +
    scale_y_discrete(expand = expansion(add = c(1.1, 0.5))) +
    scale_fill_manual(values = pitch_colors) +
    annotate("label", x = -lim, y = 0.2, label = "vs LHH", hjust = 0, size = 4,
             fontface = "bold", fill = "white", label.size = 0.3) +
    annotate("label", x =  lim, y = 0.2, label = "vs RHH", hjust = 1, size = 4,
             fontface = "bold", fill = "white", label.size = 0.3) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    # No legend: the count table beside it is the key
    theme(legend.position = "none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(color = "gray40"))
}


#' Movement scatter with the arm slot line: a usage-weighted sample of about
#' 100 pitches, with a fixed seed.
plot_movement <- function(df, hide = character()) {
  pitch_order <- setdiff(levels(df$pitch_type), hide)
  usage <- df |> count(pitch_type) |> mutate(k = pmax(1, round(n / sum(n) * 100)))
  set.seed(3)
  mv <- map_dfr(pitch_order, function(pt) {
    d <- df |> filter(pitch_type == pt)
    slice_sample(d, n = min(nrow(d), usage$k[usage$pitch_type == pt]))
  })
  # Arm angle can be all NA in a window (Savant fills it in late)
  mean_or_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) mean(x) else NA_real_ }
  arm_angle_val <- round(mean_or_na(df$arm_angle), 1)
  extension_val <- round(mean_or_na(df$release_extension), 1)

  has_slot <- is.finite(arm_angle_val)
  slope <- if (has_slot) tan(arm_angle_val * pi / 180) else NA_real_
  arm_label <- if (has_slot) paste0("Arm Angle: ", arm_angle_val, "\u00b0") else
                 "Arm Angle: not yet published"
  ext_label <- if (is.finite(extension_val)) paste0("Avg Extension: ", extension_val, " ft") else
                 "Avg Extension: not available"
  # Slot line runs to the arm side
  hand_sign <- if (df$p_throws[1] == "R") 1 else -1

  g <- ggplot(mv, aes(hb, ivb, fill = pitch_type))
  if (has_slot) {
    g <- g + annotate("segment", x = hand_sign * 22, y = slope * 22, xend = 0, yend = 0,
                      linetype = "dashed", color = "black", linewidth = 0.9)
  }
  g <- g +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_vline(xintercept = 0, linewidth = 0.6)

  g <- g +
    geom_point(shape = 21, color = "white", stroke = 0.5, size = 5, key_glyph = "polygon") +
    scale_fill_manual(values = pitch_colors) +
    coord_cartesian(xlim = c(-22, 22), ylim = c(-22, 22), clip = "off") +
    annotate("label", x = -20, y = 24, label = arm_label,
             size = 4, fontface = "bold", fill = "white", label.size = 0.4, hjust = 0) +
    annotate("label", x = 6, y = 24, label = ext_label,
             size = 4, fontface = "bold", fill = "white", label.size = 0.4, hjust = 0) +
    labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
    theme_minimal(base_size = 13) +
    # Square legend keys, as on the usage chart
    guides(fill = guide_legend(override.aes = list(linewidth = 0.5, colour = NA))) +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.text = element_text(face = "bold", size = 15), legend.key.size = unit(1.5, "cm"),
          panel.grid.major = element_line(color = "gray90"),
          aspect.ratio = 1, plot.margin = ggplot2::margin(t = 20, r = 5, b = 5, l = 5))

  # Return visibly: renderPlot() needs auto-printing
  g
}


#' Location heat maps by count bucket (Pre-2K, 2K) and pitch type, for one batter
#' side. Panels under KDE_MIN_N show raw dots.
plot_heatmap <- function(df, hand, hide = character()) {
  situations <- list("Pre-2K" = c("0-0","1-0","2-0","3-0","0-1","1-1","2-1","3-1"),
                     "2K"     = c("0-2","1-2","2-2","3-2"))
  sit_levels <- names(situations)
  if (hand != "All") df <- filter(df, stand == hand)
  base <- df |>
    filter(!is.na(plate_x), !is.na(plate_z)) |>
    # Catcher's view
    mutate(plate_x = -plate_x, cnt = paste(balls, strikes, sep = "-"), pitch_type = droplevels(pitch_type))
  hm <- imap(situations, \(counts, nm) base |> filter(cnt %in% counts) |> mutate(situation = nm)) |>
    bind_rows() |> mutate(situation = factor(situation, levels = sit_levels))
  strips <- hm |> group_by(situation, pitch_type) |>
    summarise(n = n(), iz = mean(in_zone, na.rm = TRUE) * 100, .groups = "drop") |>
    group_by(situation) |> mutate(usage = n / sum(n) * 100) |> ungroup() |>
    mutate(strip = sprintf("Usage %.0f%%   IZ %.0f%%", usage, iz))
  # A pitch never thrown in a count gets a 0% label, not a blank panel
  strips <- tidyr::complete(strips, situation, pitch_type) |>
    mutate(strip = if_else(is.na(n), "Usage 0%", strip))
  hm <- hm |> add_count(situation, pitch_type, name = "panel_n")
  if (length(hide)) {
    hm     <- hm     |> filter(!pitch_type %in% hide) |> mutate(pitch_type = droplevels(pitch_type))
    strips <- strips |> filter(!pitch_type %in% hide) |>
      mutate(pitch_type = factor(as.character(pitch_type), levels = levels(hm$pitch_type)))
  }
  dense  <- filter(hm, panel_n >= KDE_MIN_N)
  sparse <- filter(hm, panel_n <  KDE_MIN_N)
  # Drawing coordinates only; in_zone uses in_zone_flag()
  sz <- data.frame(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5)
  plate <- data.frame(x = c(-0.71,0.71,0.71,0,-0.71), y = c(0.05,0.05,0.20,0.30,0.20))
  ggplot(hm, aes(plate_x, plate_z)) +
    stat_density_2d_filled(data = dense, contour_var = "ndensity", bins = 10, h = KDE_BW) +
    geom_text(data = strips, aes(x = 0, y = 4.7, label = strip), inherit.aes = FALSE,
              color = "white", fontface = "bold", size = 4.2) +
    geom_polygon(data = plate, aes(x, y), inherit.aes = FALSE, fill = "white", color = "black", linewidth = 0.4) +
    geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.6) +
    # Dots over the zone outline
    geom_point(data = sparse, color = "white", size = 1.8, alpha = 0.9) +
    scale_fill_viridis_d(option = "viridis", guide = "none") +
    coord_fixed(xlim = c(-2.2, 2.2), ylim = c(0, 5)) +
    facet_grid(situation ~ pitch_type, switch = "y") +
    theme_void(base_size = 12) +
    theme(plot.margin = ggplot2::margin(t = 14, r = 8, b = 8, l = 8),
          strip.text.x = element_text(face = "bold", size = 13, margin = ggplot2::margin(b = 5)),
          strip.text.y.left = element_text(face = "bold", size = 13, angle = 90, margin = ggplot2::margin(r = 3)),
          panel.spacing = unit(0.6, "lines"),
          panel.background = element_rect(fill = "#440154", color = NA))
}


# ---- Trends tab --------------------------------------------------------------
# One pitch at a time, one game per point, against the window average.

# Fewest pitches of a type in a game to plot that game
TREND_MIN_N <- 3

# Usage line pools at least this many outings and pitches
TREND_USAGE_ROLL <- 3
TREND_USAGE_POOL <- 50

# "Last N games" in the titles
TREND_LAST_N <- 5

TREND_INK <- "#1f3a5f"

# Minimum y span per panel
TREND_MIN_SPAN <- c(velo = 4, spin = 200, ivb = 8, hb = 8, arm_angle = 8)

# Index of the first outing in each trailing pool
pool_start <- function(tot, k = TREND_USAGE_ROLL, min_tot = TREND_USAGE_POOL) {
  vapply(seq_along(tot), function(i) {
    j <- max(1, i - k + 1)
    while (j > 1 && sum(tot[j:i]) < min_tot) j <- j - 1
    j
  }, numeric(1))
}

# Pooled share per outing
pool_share <- function(n, tot, ...) {
  st <- pool_start(tot, ...)
  vapply(seq_along(n), function(i) sum(n[st[i]:i]) / sum(tot[st[i]:i]) * 100, numeric(1))
}


#' Pitch types for the selector, most used first, as named pitch counts
trend_pitch_choices <- function(df, hide = character()) {
  n <- sort(table(as.character(df$pitch_type)), decreasing = TRUE)
  n <- n[!names(n) %in% hide]
  stats::setNames(as.integer(n), names(n))
}


#' Per-game series and window vs last-games summary for one pitch type. Usage
#' follows the batter side; arm angle covers every pitch.
trend_series <- function(df, pt, hand) {
  one <- function(metric, rows, value) {
    g <- rows |> mutate(v = value) |> filter(!is.na(v)) |>
      group_by(game_date) |> summarise(n = n(), v = mean(v), .groups = "drop") |>
      filter(n >= TREND_MIN_N) |> arrange(game_date)
    recent <- tail(g$game_date, TREND_LAST_N)
    vals <- value[!is.na(value)]; days <- rows$game_date[!is.na(value)]
    has_last <- nrow(g) > TREND_LAST_N
    list(points = mutate(g, metric = metric, line = v),
         summ = data.frame(metric = metric, season = mean(vals),
                           last = if (has_last) mean(vals[days %in% recent]) else NA_real_,
                           from = if (has_last) min(recent) else NA_character_,
                           sd = if (nrow(g) >= 2) stats::sd(g$v) else NA_real_))
  }
  p <- df[df$pitch_type == pt, ]
  parts <- list(one("velo", p, p$release_speed), one("spin", p, p$release_spin_rate),
                one("ivb", p, p$ivb), one("hb", p, p$hb))

  u <- if (hand != "All") df[df$stand == hand, ] else df
  if (length(unique(u$game_date)) >= 2) {
    g <- u |> group_by(game_date) |>
      summarise(n = sum(pitch_type == pt), tot = n(), .groups = "drop") |>
      arrange(game_date) |>
      mutate(v = n / tot * 100, line = pool_share(n, tot), metric = "usage")
    recent <- tail(g, TREND_LAST_N)
    parts[[5]] <- list(points = g, summ = data.frame(metric = "usage",
      season = sum(g$n) / sum(g$tot) * 100,
      last = if (nrow(g) > TREND_LAST_N) sum(recent$n) / sum(recent$tot) * 100 else NA_real_,
      from = if (nrow(g) > TREND_LAST_N) min(recent$game_date) else NA_character_,
      sd = stats::sd(g$v)))
  }
  parts[[length(parts) + 1]] <- one("arm_angle", df, df$arm_angle)

  list(points = bind_rows(lapply(parts, `[[`, "points")),
       summ   = bind_rows(lapply(parts, `[[`, "summ")))
}


#' Panel titles: the metric, then the change in words
trend_titles <- function(summ, hand) {
  name <- c(velo = "Velocity", spin = "Spin", ivb = "Induced vertical break", hb = "Horizontal break",
            usage = paste0("Usage", switch(hand, L = " vs LHH", R = " vs RHH", "")),
            arm_angle = "Arm angle, all pitches")
  unit <- c(velo = " mph", spin = " rpm", ivb = "\"", hb = "\"", usage = "%", arm_angle = "\u00b0")
  f <- function(x, m) paste0(formatC(x, format = "f", digits = if (m == "spin") 0 else 1,
                                      big.mark = ","), unit[[m]])
  vapply(seq_len(nrow(summ)), function(i) {
    m <- summ$metric[i]
    if (is.na(summ$last[i])) paste0(name[[m]], ":  avg ", f(summ$season[i], m))
    else paste0(name[[m]], ":  avg ", f(summ$season[i], m), "   |   last ", TREND_LAST_N,
                " games ", f(summ$last[i], m))
  }, character(1))
}


#' Build the Trends panels once, for the plot and the hover tooltip
trend_panels <- function(df, pt, hand) {
  s <- trend_series(df, pt, hand)
  titles <- stats::setNames(trend_titles(s$summ, hand), s$summ$metric)
  lv <- factor(titles[s$points$metric], levels = titles)
  list(pts  = mutate(s$points, panel = lv, game_date = as.Date(game_date)),
       base = mutate(s$summ, panel = factor(titles[metric], levels = titles)),
       pt = pt, hand = hand)
}

plot_trends <- function(tp) {
  pts <- tp$pts; base <- tp$base
  # Usage plots a pooled line over faint per-game points
  faint <- pts$metric == "usage"
  # Invisible points widen each panel to its minimum span; usage starts at 0
  span <- base |> filter(metric %in% names(TREND_MIN_SPAN)) |>
    mutate(h = TREND_MIN_SPAN[metric] / 2)
  pad <- bind_rows(transmute(span, panel, y = season - h),
                   transmute(span, panel, y = season + h),
                   transmute(filter(base, metric == "usage"), panel, y = 0)) |>
    mutate(game_date = min(pts$game_date))

  # Shade the last TREND_LAST_N games
  recent <- base |> filter(!is.na(from)) |>
    transmute(panel, xmin = as.Date(from) - 0.5, xmax = max(pts$game_date) + 0.5)

  ggplot(pts, aes(game_date)) +
    geom_rect(data = recent, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "#dce6f2", alpha = 0.7) +
    geom_blank(data = pad, aes(y = y)) +
    # Band: average +/- one SD of his game values
    geom_rect(data = filter(base, is.finite(sd)),
              aes(xmin = -Inf, xmax = Inf, ymin = season - sd, ymax = season + sd),
              inherit.aes = FALSE, fill = "gray50", alpha = 0.10) +
    geom_hline(data = base, aes(yintercept = season), linetype = "dashed",
               color = "gray55", linewidth = 0.5) +
    # Usage points sized by pitches thrown
    geom_point(data = pts[faint, ], aes(y = v, size = tot), color = TREND_INK, alpha = 0.3) +
    scale_size_area(max_size = 3.4, guide = "none") +
    geom_line(aes(y = line), color = TREND_INK, linewidth = 0.8) +
    geom_point(data = pts[!faint, ], aes(y = v), color = TREND_INK, size = 1.8) +
    facet_wrap(~panel, ncol = 2, scales = "free_y", drop = TRUE) +
    scale_x_date(date_labels = "%b %d") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 12.5, hjust = 0),
          panel.spacing = unit(1.4, "lines"),
          axis.text = element_text(color = "gray40"))
}


#' Pitch mix by outing: one stacked bar per outing, as wide as its pitch count.
#' Follows the batter side.
plot_mix_outings <- function(df, hand, hide = character()) {
  if (hand != "All") df <- df[df$stand == hand, ]
  if (nrow(df) == 0) return(NULL)
  g <- df |> count(game_date, name = "tot") |> arrange(game_date) |>
    mutate(x1 = cumsum(tot), x0 = x1 - tot)
  # Velo and break for the tooltip
  m <- df |> group_by(game_date, pitch_type) |>
    summarise(n = n(), velo = mean(release_speed, na.rm = TRUE),
              ivb = mean(ivb, na.rm = TRUE), hb = mean(hb, na.rm = TRUE), .groups = "drop") |>
    left_join(g, by = "game_date") |>
    mutate(share = n / tot * 100)
  if (length(hide)) m <- m |> filter(!pitch_type %in% hide)
  m <- m |> mutate(pitch_type = droplevels(pitch_type)) |>
    arrange(game_date, pitch_type) |> group_by(game_date) |>
    mutate(y1 = cumsum(share), y0 = y1 - share) |> ungroup()
  # Pooled shares need every type in every outing, zeros included
  g$npool <- seq_len(nrow(g)) - pool_start(g$tot) + 1
  pl <- m |> distinct(pitch_type) |> tidyr::crossing(g) |>
    left_join(select(m, game_date, pitch_type, n), by = c("game_date", "pitch_type")) |>
    mutate(n = coalesce(n, 0L)) |> arrange(pitch_type, game_date) |>
    group_by(pitch_type) |> mutate(pooled = pool_share(n, tot)) |> ungroup()
  m <- m |> left_join(select(pl, game_date, pitch_type, pooled, npool), by = c("game_date", "pitch_type"))
  mo <- g |> mutate(m = format(as.Date(game_date), "%b")) |>
    group_by(m) |> summarise(x = min(x0), d = min(game_date), .groups = "drop") |> arrange(d)
  # Skip a month label that would overlap the next
  mo <- mo[c(diff(mo$x) >= 0.04 * max(g$x1), TRUE), ]
  # Thinner bar edges for many outings
  edge <- if (nrow(g) > 40) 0.1 else 0.3
  ggplot(m) +
    # Slightly faded fills
    geom_rect(aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = pitch_type),
              color = "white", linewidth = edge, alpha = 0.8) +
    scale_fill_manual(values = pitch_colors) +
    scale_x_continuous(breaks = mo$x, labels = mo$m, expand = c(0, 0)) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100), labels = \(x) paste0(x, "%"),
                       expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.text = element_text(face = "bold", size = 13),
          panel.grid = element_blank(), axis.text = element_text(color = "gray40"),
          axis.ticks.x = element_line(color = "gray60"))
}


#' Tooltip lines for one hovered segment of the pitch mix chart
mix_tip_text <- function(row, hand, tto = "All") {
  f <- function(x, unit) if (is.na(x) || is.nan(x)) "\u2013" else paste0(formatC(x, format = "f", digits = 1), unit)
  side <- switch(hand, L = " vs LHH", R = " vs RHH", "")
  if (!identical(tto, "All")) side <- paste0(side, ", ", TTO_TITLE[[tto]])
  nm <- PITCH_NAMES[as.character(row$pitch_type)]
  recent <- if (!is.null(row$pooled) && !is.na(row$pooled) && row$npool > 1)
    paste0("Last ", row$npool, " outings: ", formatC(row$pooled, format = "f", digits = 1), "%")
  c(format(as.Date(row$game_date), "%b %d, %Y"),
    paste0(row$pitch_type, if (!is.na(nm)) paste0(" \u00b7 ", nm)),
    paste0("Velo ", f(row$velo, " mph"), "  \u00b7  IVB ", f(row$ivb, "\""), "  \u00b7  HB ", f(row$hb, "\"")),
    paste0(row$n, " of ", row$tot, " pitches", side, " (", formatC(row$share, format = "f", digits = 1), "%)"),
    recent)
}

PITCH_NAMES <- c(FF = "Four-seam", SI = "Sinker", FC = "Cutter", SL = "Slider", ST = "Sweeper",
                 SV = "Slurve", CU = "Curveball", KC = "Knuckle curve", CS = "Slow curve",
                 CH = "Changeup", FS = "Splitter", FO = "Forkball", SC = "Screwball",
                 KN = "Knuckleball", EP = "Eephus")


#' Tooltip lines for one hovered point on the Trends chart
trend_tip_text <- function(row, tp) {
  m <- row$metric
  avg <- tp$base$season[tp$base$metric == m]
  unit <- c(velo = " mph", spin = " rpm", ivb = "\"", hb = "\"", usage = "%", arm_angle = "\u00b0")
  name <- c(velo = "Velocity", spin = "Spin", ivb = "IVB", hb = "HB", usage = "Usage",
            arm_angle = "Arm angle")
  f <- function(x) paste0(formatC(x, format = "f", digits = if (m == "spin") 0 else 1,
                                  big.mark = ","), unit[[m]])
  date <- format(row$game_date, "%b %d, %Y")
  side <- switch(tp$hand, L = " vs LHH", R = " vs RHH", "")
  sample <- switch(m,
    usage     = paste0(row$n, " of ", row$tot, " pitches", side),
    arm_angle = paste0(row$n, " pitches, all types"),
    paste0(row$n, " ", tp$pt, if (row$n == 1) "" else "s"))
  c(date, sample,
    paste0(name[[m]], " ", f(row$v), " (avg ", f(avg), ")"),
    if (m == "usage") paste0("Pooled ", f(row$line), " (last ", TREND_USAGE_ROLL, "+ outings, ",
                             TREND_USAGE_POOL, "+ pitches)"))
}


# ---- What changed: last 5 games against his average --------------------------
# Flag a change when the last-5 difference is at least TREND_CHANGE_Z standard
# errors and at least TREND_CHANGE_MIN. Shuffling game order puts the chance
# rate at about one flag in six.
TREND_CHANGE_Z   <- 2
TREND_CHANGE_MIN <- c(velo = 0.5, spin = 40, ivb = 1, hb = 1, usage = 3, arm_angle = 1)


#' Every flagged change, one row per pitch and metric. Arm angle is tested once.
trend_changes <- function(df, hand, hide = character()) {
  pts <- names(trend_pitch_choices(df, hide))
  rows <- lapply(seq_along(pts), function(i) {
    s <- trend_series(df, pts[i], hand)$summ
    s$pitch <- ifelse(s$metric == "arm_angle", "All", pts[i])
    if (i > 1) s <- s[s$metric != "arm_angle", ]
    s
  })
  out <- bind_rows(rows) |>
    filter(is.finite(last), is.finite(sd), sd > 0) |>
    mutate(diff = last - season,
           z = diff / (sd / sqrt(TREND_LAST_N)),
           min_size = TREND_CHANGE_MIN[metric]) |>
    filter(abs(z) >= TREND_CHANGE_Z, abs(diff) >= min_size)
  out[order(match(out$metric, names(TREND_CHANGE_MIN)), -abs(out$z)), ]
}


#' One line per metric and direction, e.g. "Spin down: FF -222 rpm, ST -83 rpm"
trend_change_text <- function(ch, hand) {
  if (nrow(ch) == 0) return(character())
  name <- c(velo = "Velocity", spin = "Spin", ivb = "IVB", hb = "HB",
            usage = paste0("Usage", switch(hand, L = " vs LHH", R = " vs RHH", "")),
            arm_angle = "Arm angle")
  unit <- c(velo = " mph", spin = " rpm", ivb = "\"", hb = "\"", usage = " pts", arm_angle = "\u00b0")
  ch$dir <- ifelse(ch$diff > 0, "up", "down")
  # Describe HB as more or less break
  hb <- ch$metric == "hb"
  ch$dir[hb] <- ifelse(sign(ch$diff[hb]) == sign(ch$season[hb]), "more break", "less break")
  keys <- unique(ch[, c("metric", "dir")])
  vapply(seq_len(nrow(keys)), function(k) {
    r <- ch[ch$metric == keys$metric[k] & ch$dir == keys$dir[k], ]
    d <- formatC(r$diff, format = "f", digits = if (keys$metric[k] == "spin") 0 else 1, flag = "+")
    items <- if (keys$metric[k] == "arm_angle") paste0(d, unit[[keys$metric[k]]])
             else paste0(r$pitch, " ", d, unit[[keys$metric[k]]])
    paste0(name[[keys$metric[k]]], " ", keys$dir[k], ": ", paste(items, collapse = ", "))
  }, character(1))
}
