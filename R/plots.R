# plots.R
#
# The four ggplot outputs. Each takes an already-trimmed pitch-level frame from
# features.R and returns a plot object, so nothing here reads a file or filters
# to a pitcher.
#
# Requires theme.R for pitch_colors, KDE_BW, and KDE_MIN_N.
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
plot_movement <- function(df) {
  pitch_order <- levels(df$pitch_type)
  usage <- df |> count(pitch_type) |> mutate(k = pmax(1, round(n / sum(n) * 100)))
  set.seed(42)
  mv <- map_dfr(pitch_order, function(pt) {
    d <- df |> filter(pitch_type == pt)
    slice_sample(d, n = min(nrow(d), usage$k[usage$pitch_type == pt]))
  })
  arm_angle_val <- round(mean(df$arm_angle, na.rm = TRUE), 1)
  extension_val <- round(mean(df$release_extension, na.rm = TRUE), 1)
  slope <- tan(arm_angle_val * pi / 180)
  # The dashed slot line runs out to the pitcher's arm side, so it mirrors for
  # a lefty.
  hand_sign <- if (df$p_throws[1] == "R") 1 else -1

  ggplot(mv, aes(hb, ivb, fill = pitch_type)) +
    annotate("segment", x = hand_sign * 22, y = slope * 22, xend = 0, yend = 0,
             linetype = "dashed", color = "black", linewidth = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_vline(xintercept = 0, linewidth = 0.6) +
    geom_point(shape = 21, color = "white", stroke = 0.5, size = 5) +
    scale_fill_manual(values = pitch_colors) +
    coord_cartesian(xlim = c(-22, 22), ylim = c(-22, 22), clip = "off") +
    annotate("label", x = -20, y = 24, label = paste0("Arm Angle: ", arm_angle_val, "°"),
             size = 4, fontface = "bold", fill = "white", label.size = 0.4, hjust = 0) +
    annotate("label", x = 6, y = 24, label = paste0("Avg Extension: ", extension_val, " ft"),
             size = 4, fontface = "bold", fill = "white", label.size = 0.4, hjust = 0) +
    labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", panel.grid.major = element_line(color = "gray90"),
          aspect.ratio = 1, plot.margin = margin(t = 20, r = 5, b = 5, l = 5))
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
  base <- df |>
    filter(stand == hand, !is.na(plate_x), !is.na(plate_z)) |>
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
              color = "white", fontface = "bold", size = 3.2) +
    geom_polygon(data = plate, aes(x, y), inherit.aes = FALSE, fill = "white", color = "black", linewidth = 0.4) +
    geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.6) +
    scale_fill_viridis_d(option = "viridis", guide = "none") +
    coord_fixed(xlim = c(-2.2, 2.2), ylim = c(0, 5)) +
    facet_grid(situation ~ pitch_type, switch = "y") +
    theme_void(base_size = 12) +
    theme(strip.text.x = element_text(face = "bold", size = 13, margin = margin(b = 3)),
          strip.text.y.left = element_text(face = "bold", size = 13, angle = 90, margin = margin(r = 3)),
          panel.spacing = unit(0.6, "lines"),
          panel.background = element_rect(fill = "#440154", color = NA))
}
