# plots.R
#
# The four ggplot outputs. Each takes an already-trimmed pitch-level frame from
# features.R and returns a plot object, so nothing here reads a file or filters
# to a pitcher.
#
# Requires theme.R for pitch_colors, KDE_BW, and KDE_MIN_N.
#
# margin() is called as ggplot2::margin() everywhere below, deliberately.
# randomForest exports margin(x, observed, ...), and attaching it after ggplot2
# in a long-lived console session masks the ggplot2 one. That killed exactly the
# two plots that set a plot margin, with `argument "observed" is missing, with
# no default`, while the other two rendered fine. app.R calling library(ggplot2)
# does not protect against it: library() on an already-attached package does not
# re-order the search path. Verified 2026-08-22 by masking margin in a test.
#
# Handedness note. plot_usage, plot_movement, and plot_velo take no `hand`
# argument. plot_usage splits on `stand` internally to build its two-sided bars,
# and the other two ignore `stand` entirely. Only plot_heatmap filters to one
# side, so only it needs the argument.

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)


#' Pitch types too rare in this window to draw, per CHART_MIN_SHARE and CHART_MIN_N
#'
#' Measured over the whole frame, both batter sides, so the Movement chart, the
#' two-sided usage chart and the heat maps hide the same types whichever batter
#' side is selected, and a times-through view hides what Overall hides. One
#' definition of "in the arsenal" on every chart.
chart_hidden_types <- function(df, min_share = CHART_MIN_SHARE, min_n = CHART_MIN_N) {
  n <- table(as.character(df$pitch_type))
  sort(names(n)[n / sum(n) < min_share & n < min_n])
}


#' "4 SV", or NULL when nothing was left off
chart_hidden_note <- function(df, hidden) {
  if (!length(hidden)) return(NULL)
  n <- table(as.character(df$pitch_type))[hidden]
  paste0(paste(sprintf("%d %s", as.integer(n), hidden), collapse = ", "),
         " left off the charts, under 1% of pitches; still in the tables")
}


#' Two-sided usage bars, LHH left and RHH right
#'
#' complete() fills pitch types a hitter side never saw with zero. Without it the
#' bar is absent rather than empty, and a pitch he simply never throws to lefties
#' reads the same as one he does not throw at all.
#'
#' `hide` drops bars AFTER the shares are computed, so a hidden type stays in
#' the denominator and every other bar reads the same as it did with it drawn.
plot_usage <- function(df, hide = character()) {
  usage <- df |>
    count(pitch_type, stand) |>
    complete(pitch_type, stand, fill = list(n = 0)) |>
    group_by(stand) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup() |>
    # Negative values mirror the left half of the diverging bar. Labels use
    # abs() below so the axis reads as a percentage on both sides.
    mutate(plot_pct = if_else(stand == "L", -pct, pct))
  if (length(hide)) {
    usage <- usage |> filter(!pitch_type %in% hide) |> mutate(pitch_type = droplevels(pitch_type))
  }
  # The label sits OUTSIDE the bar end, so a bar near 100% pushed it past the
  # old fixed limit of 105 and it rendered clipped, "100'". Rare on a season,
  # common on one outing or one time through the order, where a reliever can
  # throw one pitch to one side. The axis widens only when a label needs it,
  # so every chart that fit before is drawn exactly as before.
  lim <- max(105, max(usage$pct, na.rm = TRUE) + 25)
  ggplot(usage, aes(plot_pct, fct_rev(pitch_type), fill = pitch_type)) +
    geom_vline(xintercept = seq(-75, 75, 25), linetype = "dashed", color = "gray80", linewidth = 0.4) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(round(abs(plot_pct), 1), "%"),
                  hjust = if_else(stand == "L", 1.15, -0.15)),
              size = 7, fontface = "bold", color = "gray20") +
    scale_x_continuous(limits = c(-lim, lim), breaks = seq(-100, 100, 25),
                       labels = \(x) paste0(abs(x), "%")) +
    scale_y_discrete(expand = expansion(add = c(0.9, 0.6))) +
    scale_fill_manual(values = pitch_colors) +
    annotate("label", x = -90, y = 0.45, label = "vs LHH", size = 5, fontface = "bold", fill = "white") +
    annotate("label", x =  90, y = 0.45, label = "vs RHH", size = 5, fontface = "bold", fill = "white") +
    labs(x = "Usage (%)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.text = element_text(face = "bold", size = 15), legend.key.size = unit(1.5, "cm"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(color = "gray40"))
}


#' Movement scatter with the arm slot line
#'
#' Plots a usage-weighted subsample of roughly 100 pitches rather than all of
#' them, so a 600-pitch fastball does not bury a 60-pitch curveball under
#' overplotting. Each type contributes points in proportion to its usage.
#'
#' The seed is fixed so the same pitcher and window redraw identically. In the
#' app this matters more than it did for a one-off report, since a redraw that
#' shuffles the points reads as the data having changed.
#'
#' The value 3 is a deliberate choice, not a leftover. The source script used 42
#' and it was changed on purpose, so do not tidy it back. Which seed does not
#' matter, but changing it reshuffles every movement chart, so it stays put.
#'
#' `hide` names types not to draw. Arm angle, extension and every type's share
#' of the ~100 dots are still computed over the whole frame. The hidden types
#' were the ones pmax(1, ...) below over-weighted: 4 slurves in 1,203 pitches
#' got one dot in a hundred, about three times their share.
plot_movement <- function(df, hide = character()) {
  pitch_order <- setdiff(levels(df$pitch_type), hide)
  usage <- df |> count(pitch_type) |> mutate(k = pmax(1, round(n / sum(n) * 100)))
  set.seed(3)
  mv <- map_dfr(pitch_order, function(pt) {
    d <- df |> filter(pitch_type == pt)
    slice_sample(d, n = min(nrow(d), usage$k[usage$pitch_type == pt]))
  })
  # mean(x, na.rm = TRUE) over an ALL-NA vector is NaN, not NA, and NaN pastes
  # into a label as the literal text "NaN". Observed 2026-08-31: Clay Holmes over
  # 2026-08-16 to 08-26 rendered "Arm Angle: NaN°".
  #
  # Not a Holmes problem and not a bug in the pull. Savant stopped publishing
  # arm_angle on 2026-08-16 and every date since is 100% NA, against 0.3% before
  # August. It is a derived pose metric and its backfill lags the pitch data. So
  # ANY pitcher, in ANY window sitting entirely inside that gap, hits this.
  #
  # Same species as the NaN the arsenal table guards with pct_or_na(). This chart
  # never got the equivalent, and it failed worse: the label showed a computer
  # error, and slope = tan(NaN) silently dropped the dashed slot line too, so the
  # chart lost a feature without saying anything.
  mean_or_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) mean(x) else NA_real_ }
  arm_angle_val <- round(mean_or_na(df$arm_angle), 1)
  extension_val <- round(mean_or_na(df$release_extension), 1)

  # Drawn only when there is an angle to draw. A segment with a NaN slope
  # disappears, which reads as "this pitcher has no slot" rather than "this
  # number is not available yet".
  has_slot <- is.finite(arm_angle_val)
  slope <- if (has_slot) tan(arm_angle_val * pi / 180) else NA_real_
  arm_label <- if (has_slot) paste0("Arm Angle: ", arm_angle_val, "\u00b0") else
                 "Arm Angle: not yet published"
  ext_label <- if (is.finite(extension_val)) paste0("Avg Extension: ", extension_val, " ft") else
                 "Avg Extension: not available"
  # The dashed slot line runs out to the pitcher's arm side, so it mirrors for
  # a lefty.
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
    # Same legend as the Usage tab, squares and all, so the colors read without
    # switching tabs. The square keys come from key_glyph on geom_point above;
    # linewidth 0.5 and no outline are geom_col's defaults, which give the Usage
    # keys their inset and the thin white gap between them.
    guides(fill = guide_legend(override.aes = list(linewidth = 0.5, colour = NA))) +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.text = element_text(face = "bold", size = 15), legend.key.size = unit(1.5, "cm"),
          panel.grid.major = element_line(color = "gray90"),
          aspect.ratio = 1, plot.margin = ggplot2::margin(t = 20, r = 5, b = 5, l = 5))

  # A bare `g`, not the assignment above. A function ending in an assignment
  # returns INVISIBLY, and renderPlot() relies on auto-printing, so an invisible
  # return draws a blank white device with no error. Every test here calls
  # ggplot_build() or print() on the returned object, both of which work fine on
  # an invisible value, so nothing caught it for four commits.
  g
}


#' Stacked velocity densities, one row per pitch type
#'
#' Free y scales because each type is its own density and the shapes matter more
#' than their relative heights, which usage already covers.
plot_velo <- function(df) {
  meds <- df |> group_by(pitch_type) |> summarise(med = median(release_speed, na.rm = TRUE), .groups = "drop")
  ggplot(df, aes(release_speed, fill = pitch_type)) +
    geom_density(alpha = 0.7) +
    geom_vline(data = meds, aes(xintercept = med), linetype = "dashed", color = "gray30", linewidth = 0.6) +
    facet_wrap(~ pitch_type, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_fill_manual(values = pitch_colors) +
    labs(x = "Velocity (mph)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", panel.grid = element_blank(), axis.text.y = element_blank(),
          strip.text = element_text(face = "bold", size = 16, hjust = 0), strip.placement = "outside")
}


#' Location heat maps, situation by pitch type, for one batter side
#'
#' Two count panels, Pre-2K and 2K, not the six buckets from the usage tables.
#' A KDE needs a bigger per-panel sample than a usage percentage does, so the
#' buckets are deliberately wide, and together they cover every count. Until
#' 2026-09-25 this used three (0-0, Hitter Ahead, Two Strikes), which left 0-1
#' and 1-1 out of the chart entirely. See CLAUDE.md, count buckets.
#'
#' Panels below KDE_MIN_N fall back to a white-dot scatter. A density surface
#' fitted to a handful of pitches invents structure, so the thin panel is shown
#' as what it is rather than smoothed.
#'
#' `hide` names pitch types to leave out, per chart_hidden_types(). They are
#' dropped AFTER the per-panel usage strips are computed, so "Usage 24%" on
#' every remaining panel still divides by all of his pitches in that count.
plot_heatmap <- function(df, hand, hide = character()) {
  situations <- list("Pre-2K" = c("0-0","1-0","2-0","3-0","0-1","1-1","2-1","3-1"),
                     "2K"     = c("0-2","1-2","2-2","3-2"))
  sit_levels <- names(situations)
  # "All" pools both batter sides. That roughly doubles per-panel n, so more
  # panels clear KDE_MIN_N and get a density surface instead of the white-dot
  # fallback. Safe here because KDE_BW is 1.0 ft in x while the measured gap
  # between the two sides' mean locations runs 0.19 to 0.79 ft, so the bandwidth
  # smooths over the separation rather than showing two false modes. It does
  # blur the platoon pattern, which is usually the point of this chart.
  if (hand != "All") df <- filter(df, stand == hand)
  base <- df |>
    filter(!is.na(plate_x), !is.na(plate_z)) |>
    # Negate plate_x to draw from the catcher's view, which is how a hitting
    # coach reads a location chart.
    mutate(plate_x = -plate_x, cnt = paste(balls, strikes, sep = "-"), pitch_type = droplevels(pitch_type))
  hm <- imap(situations, \(counts, nm) base |> filter(cnt %in% counts) |> mutate(situation = nm)) |>
    bind_rows() |> mutate(situation = factor(situation, levels = sit_levels))
  strips <- hm |> group_by(situation, pitch_type) |>
    summarise(n = n(), iz = mean(in_zone, na.rm = TRUE) * 100, .groups = "drop") |>
    group_by(situation) |> mutate(usage = n / sum(n) * 100) |> ungroup() |>
    mutate(strip = sprintf("Usage %.0f%%   IZ %.0f%%", usage, iz))
  # A pitch he never threw in a count got no strip, so its panel rendered as a
  # blank frame, which reads as a rendering fault. It is a finding: Harris
  # throws no cutter with two strikes. Found on the page 2026-09-23. Label it
  # 0%, with no IZ, since a zone rate over no pitches does not exist.
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
  # Drawing coordinates for the zone outline and the plate. These are rendering
  # constants and are not the in_zone classification, which uses 0.8291 in
  # features.R. Kept separate so a cosmetic nudge here cannot move a rate stat.
  sz <- data.frame(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5)
  plate <- data.frame(x = c(-0.71,0.71,0.71,0,-0.71), y = c(0.05,0.05,0.20,0.30,0.20))
  ggplot(hm, aes(plate_x, plate_z)) +
    stat_density_2d_filled(data = dense, contour_var = "ndensity", bins = 10, h = KDE_BW) +
    geom_text(data = strips, aes(x = 0, y = 4.7, label = strip), inherit.aes = FALSE,
              color = "white", fontface = "bold", size = 4.2) +
    geom_polygon(data = plate, aes(x, y), inherit.aes = FALSE, fill = "white", color = "black", linewidth = 0.4) +
    geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.6) +
    # The dots paint LAST, over the zone outline. Drawn before it, a pitch on
    # the edge had the black line through it and read as a glyph: Harris's
    # FC at x -0.84 against an edge at -0.83 rendered as a quote mark. A pitch
    # on the black is the one a coach most wants to see whole.
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


# ---- Trends tab: one pitch, one line per panel ------------------------------
#
# Every other chart is one total over the window, so a pitcher who dropped his
# slot four degrees in August reads the same as one who never moved. This puts
# each game on the x axis against the window average.
#
# ONE pitch at a time, by request, 2026-10-06. The first build drew every pitch
# type on every panel and was too busy to read at a glance: five coloured lines
# crossing, and the legend had to be read before the chart said anything. Now
# each panel is a single line, and its title states the change in words, so a
# coach reads the title and only looks at the line to see when.

# Fewest pitches of a type in one game to plot that game's point. A two-pitch
# game average is one pitch's noise. Per game rather than per window, so it is
# much lower than MIN_PITCH_COUNT's job of deciding what is in the arsenal.
TREND_MIN_N <- 3

# Every outing gets a usage point, sized by its pitches, but the line pools
# back over at least TREND_USAGE_ROLL outings AND at least TREND_USAGE_POOL
# pitches. A reliever's 14-pitch inning swings a share 7 points per pitch, so
# one outing is noise; pooled to 50 pitches it is a plan.
#
# Until 2026-10-09 outings under 50 pitches were dropped instead, which left a
# reliever with no usage panel at all (Bryan Hudson, 70 outings, longest 33)
# and a swingman with his starts only. For a starter nothing moved: three starts
# are well past 50 pitches to either side, so his line is the same last-3 pool.
TREND_USAGE_ROLL <- 3
TREND_USAGE_POOL <- 50

# "Recent" in the panel titles: the last 5 plotted games against the window.
TREND_LAST_N <- 5

TREND_INK <- "#1f3a5f"

# Smallest y range each panel draws, centred on the window average. Without it
# free scales stretch whatever is there to the full panel, and a starter's
# ordinary 1 mph game-to-game wobble looks like a collapse. These spans are
# wider than normal game-to-game noise, so a real change still fills the panel.
TREND_MIN_SPAN <- c(velo = 4, spin = 200, ivb = 8, hb = 8, arm_angle = 8)

# Index of the first outing in each trailing pool: back at least k outings and
# until the pool holds `min_tot` pitches, or to the first outing.
pool_start <- function(tot, k = TREND_USAGE_ROLL, min_tot = TREND_USAGE_POOL) {
  vapply(seq_along(tot), function(i) {
    j <- max(1, i - k + 1)
    while (j > 1 && sum(tot[j:i]) < min_tot) j <- j - 1
    j
  }, numeric(1))
}

# Pooled share per outing: pitches of the type over all pitches in the pool.
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


#' Per-game series and the window-vs-recent summary for one pitch type
#'
#' Velo, spin, IVB and HB are the selected pitch, both batter sides, like the
#' movement chart; HB keeps the movement chart's raw sign. Usage follows the
#' batter side selector, because that is where a platoon plan shows. Arm angle
#' is the whole delivery, so it ignores the pitch selection.
#'
#' `season` is the window value the traits table prints: a mean over pitches,
#' or for usage a share pooled over every outing. `last` is the
#' same computed over the last TREND_LAST_N plotted games, NA when the window
#' holds no more games than that. `sd` is the spread of the per-game values, his
#' normal game-to-game range, which the band draws and trend_changes() tests.
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


#' The Trends chart for one pitch type
#'
#' Takes trend_panels() rather than the pitch frame, so the app builds the
#' panels once and the plot and the click tooltip read the same rows.
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
  # Usage plots its rolling line over faint single-game points; every other
  # panel's line IS the per-game points.
  faint <- pts$metric == "usage"
  # Invisible points that widen each panel to its minimum span. Usage is pinned
  # at zero instead, so a share is always read against nothing thrown.
  span <- base |> filter(metric %in% names(TREND_MIN_SPAN)) |>
    mutate(h = TREND_MIN_SPAN[metric] / 2)
  pad <- bind_rows(transmute(span, panel, y = season - h),
                   transmute(span, panel, y = season + h),
                   transmute(filter(base, metric == "usage"), panel, y = 0)) |>
    mutate(game_date = min(pts$game_date))

  # The last TREND_LAST_N games, shaded, so "last 5" in the title points at
  # something on a chart that otherwise shows the whole window. Starts half a
  # day early so the first shaded point is not cut in half.
  recent <- base |> filter(!is.na(from)) |>
    transmute(panel, xmin = as.Date(from) - 0.5, xmax = max(pts$game_date) + 0.5)

  ggplot(pts, aes(game_date)) +
    geom_rect(data = recent, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "#dce6f2", alpha = 0.7) +
    geom_blank(data = pad, aes(y = y)) +
    # His normal game-to-game range, average +/- one SD of the game values.
    # Roughly two games in three land inside it, so a point outside is unusual
    # for him and a run of them is a change.
    geom_rect(data = filter(base, is.finite(sd)),
              aes(xmin = -Inf, xmax = Inf, ymin = season - sd, ymax = season + sd),
              inherit.aes = FALSE, fill = "gray50", alpha = 0.10) +
    geom_hline(data = base, aes(yintercept = season), linetype = "dashed",
               color = "gray55", linewidth = 0.5) +
    # Sized by pitches, so a 9-pitch inning at 100% reads as the small thing it
    # is next to a 90-pitch start.
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


#' Pitch mix by outing: one stacked bar per outing, width = pitches thrown
#'
#' The usage panel follows one pitch; this shows what it traded with. Bars sit
#' end to end on a pitch-count axis rather than dates, so a 90-pitch start is
#' wide, a 9-pitch inning is a sliver, and the off days between outings take no
#' room. Month labels mark where each month's first outing begins.
#'
#' Follows the Batter side selector like the usage panel. `hide` drops types
#' AFTER the shares are computed, as plot_usage() does, so a hidden type leaves
#' its sliver of white rather than inflating the rest.
plot_mix_outings <- function(df, hand, hide = character()) {
  if (hand != "All") df <- df[df$stand == hand, ]
  if (nrow(df) == 0) return(NULL)
  g <- df |> count(game_date, name = "tot") |> arrange(game_date) |>
    mutate(x1 = cumsum(tot), x0 = x1 - tot)
  m <- df |> count(game_date, pitch_type) |>
    left_join(g, by = "game_date") |>
    mutate(share = n / tot * 100)
  if (length(hide)) m <- m |> filter(!pitch_type %in% hide)
  m <- m |> mutate(pitch_type = droplevels(pitch_type)) |>
    arrange(game_date, pitch_type) |> group_by(game_date) |>
    mutate(y1 = cumsum(share), y0 = y1 - share) |> ungroup()
  mo <- g |> mutate(m = format(as.Date(game_date), "%b")) |>
    group_by(m) |> summarise(x = min(x0), d = min(game_date), .groups = "drop") |> arrange(d)
  # One outing is a thin bar whose white edge would swallow it, so the edge
  # thins as the outing count grows.
  edge <- if (nrow(g) > 40) 0.1 else 0.3
  ggplot(m) +
    geom_rect(aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = pitch_type),
              color = "white", linewidth = edge) +
    # Reversed so the legend reads top to bottom in the order the bars stack,
    # most-used type at the bottom on the 0% baseline.
    scale_fill_manual(values = pitch_colors, guide = guide_legend(reverse = TRUE)) +
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


#' The tooltip for one hovered point on the Trends chart
#'
#' `row` is one row of trend_panels()$pts, as nearPoints() returns it. Says the
#' game, the sample behind the point, and the point against the window average,
#' since the average is the thing the reader is comparing it to.
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


# ---- What changed: last 5 games against his average, every pitch ------------
#
# The chart shows one pitch, so a change that runs through the whole arsenal
# (Skenes lost 250 to 300 rpm on every pitch from March to July while the
# league stayed flat) is only visible by clicking through every button. This
# lists every pitch and metric whose last-5 value sits outside chance.
#
# Two bars, both required. Statistical: the last-5 difference is at least
# TREND_CHANGE_Z standard errors, the SE being his game-to-game SD over the
# root of TREND_LAST_N. Practical: at least TREND_CHANGE_MIN, so a pitch he
# throws identically every night is not flagged for a 0.2 mph wobble that is
# significant only because he is so consistent.
#
# Calibrated 2026-10-06 on 80 starters by shuffling each one's game order,
# which keeps his values and destroys any real trend: 3.1 flags per pitcher on
# real order against 0.5 on shuffled, so about one flag in six is chance. At
# 2.5 that falls to one in eleven but real flags halve (3.1 to 1.25). A flag is
# a prompt to look at the chart, not a verdict, so 2 keeps the real ones.
TREND_CHANGE_Z   <- 2
TREND_CHANGE_MIN <- c(velo = 0.5, spin = 40, ivb = 1, hb = 1, usage = 3, arm_angle = 1)


#' Every flagged change, one row per pitch and metric
#'
#' Arm angle is one delivery, so it is tested once, from the first pitch's
#' series, under pitch "All".
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


#' The strip's sentences, one per metric and direction
#'
#' "Spin down: FF -222 rpm, ST -83 rpm". Grouped this way because a change
#' that shows on several pitches at once is the finding the strip exists for.
trend_change_text <- function(ch, hand) {
  if (nrow(ch) == 0) return(character())
  name <- c(velo = "Velocity", spin = "Spin", ivb = "IVB", hb = "HB",
            usage = paste0("Usage", switch(hand, L = " vs LHH", R = " vs RHH", "")),
            arm_angle = "Arm angle")
  unit <- c(velo = " mph", spin = " rpm", ivb = "\"", hb = "\"", usage = " pts", arm_angle = "\u00b0")
  ch$dir <- ifelse(ch$diff > 0, "up", "down")
  # HB's sign is raw, so "up" would mean toward third base, which reads as
  # nothing. Say what it means for this pitch instead: more or less break.
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
