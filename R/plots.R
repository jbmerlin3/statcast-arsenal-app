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


#' Two-sided usage bars, LHH left and RHH right
#'
#' complete() fills pitch types a hitter side never saw with zero. Without it the
#' bar is absent rather than empty, and a pitch he simply never throws to lefties
#' reads the same as one he does not throw at all.
plot_usage <- function(df) {
  usage <- df |>
    count(pitch_type, stand) |>
    complete(pitch_type, stand, fill = list(n = 0)) |>
    group_by(stand) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup() |>
    # Negative values mirror the left half of the diverging bar. Labels use
    # abs() below so the axis reads as a percentage on both sides.
    mutate(plot_pct = if_else(stand == "L", -pct, pct))
  ggplot(usage, aes(plot_pct, fct_rev(pitch_type), fill = pitch_type)) +
    geom_vline(xintercept = seq(-75, 75, 25), linetype = "dashed", color = "gray80", linewidth = 0.4) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(round(abs(plot_pct), 1), "%"),
                  hjust = if_else(stand == "L", 1.15, -0.15)),
              size = 7, fontface = "bold", color = "gray20") +
    scale_x_continuous(limits = c(-105, 105), breaks = seq(-100, 100, 25),
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
plot_movement <- function(df, ref = NULL) {
  pitch_order <- levels(df$pitch_type)
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

  # League marks for the same pitch types and the same pitcher hand. Built from
  # the precomputed reference, so nothing here scans app_data and this stays
  # cheap inside a reactive.
  #
  # p_throws is passed through and never pooled. hb is not arm-side normalised,
  # so a righty's and a lefty's sliders sit on opposite sides of zero and their
  # pooled mean lands at a point neither hand throws.
  lg <- if (is.null(ref)) NULL else movement_ref(ref, pitch_order, df$p_throws[1])

  g <- ggplot(mv, aes(hb, ivb, fill = pitch_type))
  if (has_slot) {
    g <- g + annotate("segment", x = hand_sign * 22, y = slope * 22, xend = 0, yend = 0,
                      linetype = "dashed", color = "black", linewidth = 0.9)
  }
  g <- g +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_vline(xintercept = 0, linewidth = 0.6)

  if (!is.null(lg)) {
    # A cross rather than a filled dot, so a league mark can never be mistaken
    # for one of the pitcher's own pitches, and the n rides beside it: a mark
    # built on 22 pitchers and one built on 356 are not the same claim.
    g <- g +
      geom_point(data = lg, aes(hb, ivb, color = pitch_type), inherit.aes = FALSE,
                 shape = 3, size = 4.5, stroke = 1.4) +
      geom_text(data = lg, aes(hb, ivb, color = pitch_type, label = paste0("n=", n_pitchers)),
                inherit.aes = FALSE, vjust = -1.1, size = 3, fontface = "bold",
                show.legend = FALSE) +
      scale_color_manual(values = pitch_colors, guide = "none")
  }

  g <- g +
    geom_point(shape = 21, color = "white", stroke = 0.5, size = 5) +
    scale_fill_manual(values = pitch_colors) +
    coord_cartesian(xlim = c(-22, 22), ylim = c(-22, 22), clip = "off") +
    annotate("label", x = -20, y = 24, label = arm_label,
             size = 4, fontface = "bold", fill = "white", label.size = 0.4, hjust = 0) +
    annotate("label", x = 6, y = 24, label = ext_label,
             size = 4, fontface = "bold", fill = "white", label.size = 0.4, hjust = 0) +
    labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", panel.grid.major = element_line(color = "gray90"),
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
#' Uses the three coarse count buckets, not the six from the usage tables. A KDE
#' needs a bigger per-panel sample than a usage percentage does, so the buckets
#' are deliberately wider here. See CLAUDE.md, count buckets.
#'
#' Panels below KDE_MIN_N fall back to a white-dot scatter. A density surface
#' fitted to a handful of pitches invents structure, so the thin panel is shown
#' as what it is rather than smoothed.
plot_heatmap <- function(df, hand) {
  situations <- list("0-0"=c("0-0"), "Hitter Ahead"=c("1-0","2-0","3-0","2-1","3-1"),
                     "Two Strikes"=c("0-2","1-2","2-2","3-2"))
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
  hm <- hm |> add_count(situation, pitch_type, name = "panel_n")
  dense  <- filter(hm, panel_n >= KDE_MIN_N)
  sparse <- filter(hm, panel_n <  KDE_MIN_N)
  # Drawing coordinates for the zone outline and the plate. These are rendering
  # constants and are not the in_zone classification, which uses 0.8291 in
  # features.R. Kept separate so a cosmetic nudge here cannot move a rate stat.
  sz <- data.frame(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5)
  plate <- data.frame(x = c(-0.71,0.71,0.71,0,-0.71), y = c(0.05,0.05,0.20,0.30,0.20))
  ggplot(hm, aes(plate_x, plate_z)) +
    stat_density_2d_filled(data = dense, contour_var = "ndensity", bins = 10, h = KDE_BW) +
    geom_point(data = sparse, color = "white", size = 1.8, alpha = 0.9) +
    geom_text(data = strips, aes(x = 0, y = 4.7, label = strip), inherit.aes = FALSE,
              color = "white", fontface = "bold", size = 4.2) +
    geom_polygon(data = plate, aes(x, y), inherit.aes = FALSE, fill = "white", color = "black", linewidth = 0.4) +
    geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.6) +
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
