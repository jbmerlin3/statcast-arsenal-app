# report.R
#
# The one-page coach sheet behind the "Generate report" button. One landscape
# letter page: movement, release and usage by count across the top, the
# characteristics table in the middle, heat maps along the bottom. The Usage
# tab's times-through view and the Context tab are left off: a sheet a coach
# reads in the dugout has room for what a pitch is, how often, and where.
#
# Drawn with grid and ggplot2 alone, no gt and no browser. A gt table becomes a
# PDF only through a headless Chrome (webshot2 / chromote), which shinyapps.io
# does not have, and patchwork or gridExtra would be a new entry in renv.lock
# for a layout grid already ships. So the tables here are redrawn by
# grid_table() below, from the SAME frames and the SAME resolve_table() fills
# the Characteristics tab renders, which is what keeps the page and the PDF
# from disagreeing about a number or a colour.
#
# Every function takes frames and returns or draws; nothing reads input$ or a
# file, so the page can be built from the console exactly as the app builds it.

library(grid)


# ---- Fitting the app's plots onto a sheet -------------------------------------

#' Shrink a plot's drawn text and points by a factor
#'
#' The app's plots are sized for a 620px panel, and ggplot text is in absolute
#' points, so the same plot dropped into a 3-inch box keeps 13pt type and 5pt
#' dots and crowds itself out. This scales the layers' own sizes; the theme is
#' scaled by the caller with theme(), since that part is a plain override.
shrink_layers <- function(p, k) {
  p$layers <- lapply(p$layers, function(l) {
    if (!is.null(l$aes_params$size)) l$aes_params$size <- l$aes_params$size * k
    if (!is.null(l$aes_params$stroke)) l$aes_params$stroke <- l$aes_params$stroke * k
    if (!is.null(l$aes_params$linewidth)) l$aes_params$linewidth <- l$aes_params$linewidth * k
    l
  })
  p
}


#' Release point, catcher's view, one dot per sampled pitch and a ringed mean
#'
#' Catcher's view to match the movement chart and the heat maps on the same
#' sheet. The axis reads unsigned feet with the base named at each end rather
#' than Savant's signed release_pos_x, because the characteristics table on
#' this page prints release side with the OPPOSITE sign (positive toward third,
#' see arsenal_table()). Two signed conventions for one number on one sheet is
#' the Tolle HB mismatch again; naming the sides sidesteps it.
#'
#' Sampled the way plot_movement() samples, about 100 dots split by usage, so a
#' 1,500-pitch starter is not a solid blob.
plot_release <- function(df, hide = character()) {
  pitch_order <- setdiff(levels(df$pitch_type), hide)
  df <- filter(df, pitch_type %in% pitch_order, !is.na(release_pos_x), !is.na(release_pos_z))
  usage <- df |> count(pitch_type) |> mutate(k = pmax(1, round(n / sum(n) * 100)))
  set.seed(3)
  pts <- map_dfr(pitch_order, function(pt) {
    d <- filter(df, pitch_type == pt)
    slice_sample(d, n = min(nrow(d), usage$k[usage$pitch_type == pt]))
  })
  means <- df |> group_by(pitch_type) |>
    summarise(x = mean(release_pos_x), z = mean(release_pos_z), .groups = "drop")
  lim <- 4
  ggplot(pts, aes(release_pos_x, release_pos_z, fill = pitch_type)) +
    # The rubber, at the pitcher's feet. Release height is measured from the
    # field, so the mound's ten inches sit under the rubber, not under zero.
    annotate("rect", xmin = -1, xmax = 1, ymin = 0.78, ymax = 0.88, fill = "gray70") +
    geom_vline(xintercept = 0, color = "gray85", linewidth = 0.4) +
    geom_point(shape = 21, color = "white", stroke = 0.3, size = 2, alpha = 0.85) +
    geom_point(data = means, aes(x, z), shape = 21, color = "black", stroke = 0.7, size = 3.2) +
    scale_fill_manual(values = pitch_colors) +
    scale_x_continuous(limits = c(-lim, lim), breaks = seq(-lim, lim, 2),
                       labels = function(x) abs(x)) +
    scale_y_continuous(limits = c(0.5, 7.5), breaks = 1:7) +
    coord_fixed() +
    annotate("text", x = -lim, y = 7.4, label = "3B side", hjust = 0, size = 2.3, color = "gray45") +
    annotate("text", x = lim, y = 7.4, label = "1B side", hjust = 1, size = 2.3, color = "gray45") +
    labs(x = "Release side (ft)", y = "Release height (ft)") +
    theme_minimal(base_size = 7.5) +
    theme(legend.position = "none", panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray92"),
          plot.margin = ggplot2::margin(2, 4, 2, 2))
}


# ---- Tables, drawn with grid --------------------------------------------------

#' Draw a table into the current viewport
#'
#' `cells` is the frame of display strings, one column per table column.
#' `fill` and `col` are matrices of the same shape, or NULL for white and
#' near-black. The first column is the pitch code and takes its pitch colour
#' as the app's tables do. Column widths are relative weights and fill the
#' viewport's width; row height is fixed so a short arsenal does not stretch.
grid_table <- function(cells, header, widths = rep(1, ncol(cells)),
                       fill = NULL, col = NULL, bold = NULL,
                       row_h = unit(0.17, "in"), fontsize = 7,
                       group_header = NULL) {
  nr <- nrow(cells); nc <- ncol(cells)
  w  <- widths / sum(widths)
  x0 <- c(0, cumsum(w))[seq_len(nc)]
  if (is.null(fill)) fill <- matrix("white", nr, nc)
  if (is.null(col))  col  <- matrix("#1a1a1a", nr, nc)
  if (is.null(bold)) bold <- matrix(FALSE, nr, nc)
  pt <- as.character(cells[[1]])

  top <- unit(1, "npc")
  # Optional spanning labels over runs of columns, list(label = c(from, to)).
  if (!is.null(group_header)) {
    for (nm in names(group_header)) {
      r <- group_header[[nm]]
      gx <- x0[r[1]]; gw <- sum(w[r[1]:r[2]])
      grid.text(nm, x = unit(gx + gw / 2, "npc"), y = top - row_h * 0.5,
                gp = gpar(fontsize = fontsize - 0.5, fontface = "bold", col = "#555555"))
      grid.lines(unit(c(gx + 0.004, gx + gw - 0.004), "npc"), top - row_h * c(0.95, 0.95),
                 gp = gpar(col = "#999999", lwd = 0.6))
    }
    top <- top - row_h
  }

  # Header band, ruled top and bottom in black like the app's gt chassis.
  grid.rect(x = unit(0, "npc"), y = top, width = unit(1, "npc"), height = row_h,
            just = c("left", "top"), gp = gpar(fill = "gray95", col = NA))
  grid.lines(unit(c(0, 1), "npc"), unit.c(top, top), gp = gpar(lwd = 1.2))
  grid.lines(unit(c(0, 1), "npc"), unit.c(top - row_h, top - row_h), gp = gpar(lwd = 1.2))
  for (j in seq_len(nc)) {
    grid.text(header[j], x = unit(x0[j] + w[j] / 2, "npc"), y = top - row_h * 0.5,
              gp = gpar(fontsize = fontsize - 0.5, fontface = "bold"))
  }

  for (i in seq_len(nr)) {
    yt <- top - row_h * i
    for (j in seq_len(nc)) {
      grid.rect(x = unit(x0[j], "npc"), y = yt, width = unit(w[j], "npc"), height = row_h,
                just = c("left", "top"), gp = gpar(fill = fill[i, j], col = "white", lwd = 0.8))
      tc <- if (j == 1) pitch_text_colors[[pt[i]]] else col[i, j]
      grid.text(cells[[j]][i], x = unit(x0[j] + w[j] / 2, "npc"), y = yt - row_h * 0.5,
                gp = gpar(fontsize = fontsize, col = tc,
                          fontface = if (j == 1 || bold[i, j]) "bold" else "plain"))
    }
    grid.lines(unit(c(0, 1), "npc"), unit.c(yt - row_h, yt - row_h),
               gp = gpar(col = "#e3e3e3", lwd = 0.5))
  }
  invisible(NULL)
}


#' The characteristics table as display strings, with its league fills
#'
#' One row per pitch, traits then results, the same two column lists the
#' Characteristics tab splits on. `tbl` is arsenal_table() output and `ref` is
#' resolve_table() over that same frame, so every fill here is the fill on the
#' tab. Of the markers, only a thin cell's own denominator is kept; see below.
report_chars <- function(tbl, ref) {
  fmt <- function(x, d, pct = FALSE, plus = FALSE) {
    # Rounded first so -0.04 prints +0.0 rather than a signed zero.
    x <- round(x, d) + 0
    s <- formatC(x, format = "f", digits = d, flag = if (plus) "+" else "")
    s[is.na(x)] <- "–"
    s
  }
  xw <- ifelse(is.na(tbl$xwoba), "–", sub("^0", "", sprintf("%.3f", tbl$xwoba)))
  has_stuff <- "stuff_plus" %in% names(tbl) && any(is.finite(tbl$stuff_plus))

  cols <- list(
    pitch_type = as.character(tbl$pitch_type),
    count      = format(tbl$count, big.mark = ","),
    pitch_pct  = fmt(tbl$pitch_pct, 1),
    velocity   = fmt(tbl$velocity, 1),
    ivb        = fmt(tbl$ivb, 1),
    hb         = fmt(tbl$hb, 1),
    vaa        = fmt(tbl$vaa, 1),
    spin       = format(tbl$spin, big.mark = ","),
    ext        = fmt(tbl$ext, 1),
    rel_ht     = fmt(tbl$rel_ht, 2),
    rel_side   = fmt(tbl$rel_side, 2),
    stuff_plus = if (has_stuff) fmt(tbl$stuff_plus, 0) else NULL,
    strike_pct = fmt(tbl$strike_pct, 1),
    whiff_pct  = fmt(tbl$whiff_pct, 1),
    z_whiff_pct = if ("z_whiff_pct" %in% names(tbl)) fmt(tbl$z_whiff_pct, 1) else NULL,
    csw_pct    = fmt(tbl$csw_pct, 1),
    zone_pct   = fmt(tbl$zone_pct, 1),
    chase_pct  = fmt(tbl$chase_pct, 1),
    gb_pct     = fmt(tbl$gb_pct, 1),
    xwoba      = xw,
    rv         = fmt(tbl$rv, 1, plus = TRUE))
  cols <- cols[!vapply(cols, is.null, logical(1))]
  cells <- as.data.frame(cols, stringsAsFactors = FALSE)

  labels <- c(pitch_type = "PITCH", count = "#", pitch_pct = "USE%", velocity = "VELO",
              ivb = "IVB", hb = "HB", vaa = "VAA", spin = "SPIN", ext = "EXT",
              rel_ht = "REL HT", rel_side = "REL SIDE", stuff_plus = "STUFF+",
              strike_pct = "STR%", whiff_pct = "WHIFF%", z_whiff_pct = "Z-WHIFF%", csw_pct = "CSW%",
              zone_pct = "ZONE%", chase_pct = "CHASE%", gb_pct = "GB%",
              xwoba = "xwOBA", rv = "RV")

  fill <- matrix("white", nrow(cells), ncol(cells))
  col  <- matrix("#1a1a1a", nrow(cells), ncol(cells))
  bold <- matrix(FALSE, nrow(cells), ncol(cells))
  if (!is.null(ref)) {
    for (i in seq_len(nrow(ref$cells))) {
      ce <- ref$cells[i, ]
      j <- match(ce$column, names(cells))
      if (is.na(j)) next
      fill[ce$row, j] <- ce$fill
      col[ce$row, j]  <- ce$text_color
      bold[ce$row, j] <- identical(ce$font_weight, "bold")
      # The below-floor denominator, "(2)", is the one marker kept. Without it a
      # one-start sheet prints GB% 100.0 off two balls in play, filled like any
      # other cell. The dagger glyphs go, with the legend they point to.
      if (startsWith(ce$marker, " (")) cells[[j]][ce$row] <- paste0(cells[[j]][ce$row], ce$marker)
    }
  }
  n_traits <- sum(names(cells) %in% TRAITS_COLS)
  list(cells = cells, header = unname(labels[names(cells)]), fill = fill, col = col,
       bold = bold, n_traits = n_traits)
}


# ---- The page -----------------------------------------------------------------

#' Write the one-page coach sheet to `path`
#'
#' d        shape_arsenal() frame for the pitcher and window, both batter sides
#' hand     "All", "L" or "R", the app's batter side selector
#' hide     chart_hidden_types(d), so the sheet leaves off what the tabs leave off
#' tbl, ref arsenal_table() and resolve_table() for the same d and hand
#' meta     list(name, dates, data_through)
report_pdf <- function(path, d, hand, hide, tbl, ref, meta) {
  # Plain pdf(), not cairo_pdf(): capabilities("cairo") reports TRUE on a Mac
  # without XQuartz and the device then fails to load. The cost is Latin-1
  # text, so the sheet uses "-" where the app's tables use an en dash.
  grDevices::pdf(path, width = 11, height = 8.5, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid.newpage()
  # Opaque, so a viewer that composites on black (Preview's thumbnails, sips)
  # does not swallow the heat maps' black strip labels.
  grid.rect(gp = gpar(fill = "white", col = NA))

  navy <- "#1f3a5f"
  sec_title <- function(txt, sub = NULL) {
    grid.text(txt, x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"),
              gp = gpar(fontsize = 9.5, fontface = "bold", col = "#111111"))
    if (!is.null(sub))
      grid.text(sub, x = unit(1, "npc"), y = unit(1, "npc"), just = c("right", "top"),
                gp = gpar(fontsize = 6.5, col = "#888888"))
    grid.rect(x = unit(-0.07, "in"), y = unit(1, "npc"), width = unit(0.035, "in"),
              height = unit(0.15, "in"), just = c("left", "top"),
              gp = gpar(fill = navy, col = NA))
  }
  # A section viewport with its title on top and a body viewport under it.
  section <- function(x, y, w, h, title, sub = NULL, body) {
    pushViewport(viewport(x = unit(x, "in"), y = unit(y, "in"), width = unit(w, "in"),
                          height = unit(h, "in"), just = c("left", "top")))
    sec_title(title, sub)
    pushViewport(viewport(y = unit(0, "npc"), height = unit(1, "npc") - unit(0.2, "in"),
                          just = "bottom"))
    body()
    popViewport(2)
  }
  draw_plot <- function(p) print(p, newpage = FALSE)

  # ---- Header ----
  hand_txt <- if (identical(hand, "All")) "vs all batters" else hand_label(hand)
  throws   <- if (identical(d$p_throws[1], "R")) "RHP" else "LHP"
  team     <- paste(sort(unique(as.character(d$pitch_team))), collapse = "/")
  grid.rect(x = 0, y = 1, width = 1, height = unit(0.62, "in"), just = c("left", "top"),
            gp = gpar(fill = navy, col = NA))
  grid.text(meta$name, x = unit(0.4, "in"), y = unit(1, "npc") - unit(0.27, "in"),
            just = "left", gp = gpar(fontsize = 18, fontface = "bold", col = "white"))
  grid.text(paste(throws, team, sep = "  ·  "),
            x = unit(0.4, "in"), y = unit(1, "npc") - unit(0.49, "in"),
            just = "left", gp = gpar(fontsize = 9, col = "#c8d3e3"))
  n_pitch <- if (identical(hand, "All")) nrow(d) else sum(d$stand == hand)
  grid.text(sprintf("%s to %s  ·  %s  ·  %s pitches",
                    meta$dates[1], meta$dates[2], hand_txt, format(n_pitch, big.mark = ",")),
            x = unit(1, "npc") - unit(0.4, "in"), y = unit(1, "npc") - unit(0.27, "in"),
            just = "right", gp = gpar(fontsize = 10, fontface = "bold", col = "white"))
  grid.text(paste("Pitcher Arsenal  ·  data through", meta$data_through),
            x = unit(1, "npc") - unit(0.4, "in"), y = unit(1, "npc") - unit(0.49, "in"),
            just = "right", gp = gpar(fontsize = 7.5, col = "#c8d3e3"))

  # ---- Top row: movement | release | usage by count ----
  top_y <- 8.5 - 0.78
  section(0.4, top_y, 2.75, 2.85, "Movement", "catcher's view, both sides", function() {
    p <- plot_movement(d, hide = hide) |> shrink_layers(0.45)
    # No legend at this size: the pitch codes in both tables carry the colours.
    p <- p + theme(legend.position = "none",
                   text = element_text(size = 7.5), axis.text = element_text(size = 6.5),
                   plot.margin = ggplot2::margin(10, 2, 2, 2))
    draw_plot(p)
  })
  section(3.35, top_y, 2.35, 2.85, "Release", "catcher's view, both sides", function() {
    draw_plot(plot_release(d, hide = hide))
  })

  usage <- count_usage_tbl(d, hand)
  if (length(hide)) usage <- filter(usage, !pitch_type %in% hide)
  # vs LHH and vs RHH sit in front of the count buckets because the side split
  # is the first thing a coach plans around, and the usage chart that shows it
  # in the app has no room here. Both sides are shown whatever the selector says.
  side_share <- function(h) {
    s <- count_usage_tbl(d, h)
    s$`All Counts`[match(usage$pitch_type, s$pitch_type)]
  }
  u <- data.frame(pitch_type = as.character(usage$pitch_type),
                  L = side_share("L"), R = side_share("R"), check.names = FALSE)
  u <- cbind(u, as.data.frame(usage[, -1], check.names = FALSE))
  pct <- function(x) ifelse(is.na(x), "–", sprintf("%.1f", x))
  u_cells <- data.frame(lapply(u, function(x) if (is.numeric(x)) pct(x) else x),
                        check.names = FALSE, stringsAsFactors = FALSE)
  u_head  <- c("PITCH", "vs LHH", "vs RHH", "ALL", "EARLY", "AHEAD", "BEHIND", "<2K", "2K")
  # Each bucket shaded by its own share, a light wash so the eye finds the
  # pitch he leans on in each count without reading every number.
  ramp   <- grDevices::colorRamp(c("#ffffff", "#4a6f9c"))
  u_fill <- matrix("white", nrow(u), ncol(u))
  u_col  <- matrix("#1a1a1a", nrow(u), ncol(u))
  for (j in 2:ncol(u)) {
    v  <- pmin(1, u[[j]] / 80)
    ok <- !is.na(v)
    u_fill[ok, j] <- grDevices::rgb(ramp(v[ok]), maxColorValue = 255)
    u_col[ok & v > 0.6, j] <- "white"
  }
  section(5.95, top_y, 4.65, 2.85, "Usage by count",
          paste("count columns", hand_txt), function() {
    grid_table(u_cells, u_head, widths = c(1, 1.15, 1.15, 0.9, 0.95, 0.95, 1, 0.8, 0.8),
               fill = u_fill, col = u_col, fontsize = 7.5, row_h = unit(0.2, "in"),
               bold = matrix(names(u_cells) == "All Counts", nrow(u), ncol(u), byrow = TRUE),
               group_header = list("Overall" = c(2, 3),
                                   "By count (share of pitches in that count)" = c(4, 9)))
    n_rows <- nrow(u) + 2
    grid.text(paste0("Early 0-0, 0-1, 1-0.  Ahead 0-1, 0-2, 1-2, 2-2.  Behind 1-0, 2-0, 3-0, ",
                     "2-1, 3-1.  Buckets overlap, so rows do not sum to 100."),
              x = 0, y = unit(1, "npc") - unit(0.2, "in") * (n_rows + 0.6), just = c("left", "top"),
              gp = gpar(fontsize = 6, col = "#888888"))
  })

  # ---- Middle: characteristics ----
  ch <- report_chars(tbl, ref)
  mid_y <- top_y - 2.98
  ch_h  <- 0.2 + 0.17 * (nrow(ch$cells) + 2) + 0.2
  section(0.4, mid_y, 10.2, ch_h, "Pitch characteristics",
          paste(hand_txt, "· shaded by percentile vs league, same pitch type and hand"),
          function() {
    nc <- ncol(ch$cells)
    wts <- rep(1, nc); wts[1] <- 0.9; wts[names(ch$cells) %in% c("rel_ht", "rel_side", "z_whiff_pct")] <- 1.2
    grid_table(ch$cells, ch$header, widths = wts, fill = ch$fill, col = ch$col,
               bold = ch$bold, fontsize = 7,
               group_header = list("Pitch traits" = c(2, ch$n_traits),
                                   "Pitch results" = c(ch$n_traits + 1, nc)))
  })

  # ---- Bottom: heat maps ----
  hm_y <- mid_y - ch_h - 0.12
  hm_h <- hm_y - 0.3
  n_types <- length(setdiff(levels(droplevels(d$pitch_type)), hide))
  section(0.4, hm_y, 10.2, hm_h, "Location",
          paste(hand_txt, "· catcher's view · Pre-2K and 2K counts"), function() {
    p <- plot_heatmap(d, hand, hide = hide) |> shrink_layers(0.5)
    p <- p + theme(strip.text.x = element_text(face = "bold", size = 8, margin = ggplot2::margin(b = 2)),
                   strip.text.y.left = element_text(face = "bold", size = 8, angle = 90,
                                                    margin = ggplot2::margin(r = 2)),
                   panel.spacing = unit(0.25, "lines"),
                   plot.margin = ggplot2::margin(2, 2, 2, 2))
    # Narrower than the box when the arsenal is small, so each panel stays
    # square at the box's height rather than stretching a two-pitch pitcher.
    w_in <- min(10.2, (hm_h - 0.35) / 2 * n_types + 0.3)
    pushViewport(viewport(x = 0, width = unit(w_in, "in"), just = "left"))
    draw_plot(p)
    popViewport()
  })

  # ---- Footer ----
  foot <- c(
    chart_hidden_note(d, hide),
    paste("# is the sample behind each row; a number in parentheses is that rate's own",
          "sample (swings, out-of-zone pitches, PA or balls in play) where it is thin."))
  grid.text(paste(foot, collapse = "  ·  "), x = unit(0.4, "in"), y = unit(0.18, "in"),
            just = "left", gp = gpar(fontsize = 6, col = "#999999"))
  invisible(path)
}
