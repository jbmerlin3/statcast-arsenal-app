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


#' Release point, pitcher's view, one small dot per pitch
#'
#' Pitcher's view, standing behind the mound: first base on the left, third on
#' the right, the way TruMedia draws it. That puts a righty's release on the
#' same side as his arm-side run on the movement chart beside it. It is also
#' the characteristics table's convention: x is -release_pos_x, exactly the
#' REL SIDE column (positive toward third, see arsenal_table()), so the axis
#' and the table agree in sign. Changed 2026-10-27; it was catcher's view.
#'
plot_release <- function(df, hide = character()) {
  pitch_order <- setdiff(levels(df$pitch_type), hide)
  # Every pitch, drawn small, rather than a usage-weighted sample with a ringed
  # mean per type: the TruMedia release chart. The spread IS the read here,
  # whether the types come out of one slot or drift apart, and a mean bubble
  # covered exactly that. Most-used type drawn first so a rare pitch's dots
  # land on top instead of under the fastball.
  df <- filter(df, pitch_type %in% pitch_order, !is.na(release_pos_x), !is.na(release_pos_z)) |>
    mutate(rel_side = -release_pos_x,
           pitch_type = factor(as.character(pitch_type), levels = pitch_order)) |>
    arrange(pitch_type)
  lim <- 4
  ggplot(df, aes(rel_side, release_pos_z, color = pitch_type)) +
    geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35, linetype = "dashed") +
    geom_point(size = 0.45, alpha = 0.8) +
    scale_color_manual(values = pitch_colors) +
    scale_x_continuous(limits = c(-lim, lim), breaks = seq(-lim, lim, 2)) +
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
    coord_fixed() +
    annotate("text", x = -lim, y = 7.85, label = "1B side", hjust = 0, size = 2.3, color = "gray45") +
    annotate("text", x = lim, y = 7.85, label = "3B side", hjust = 1, size = 2.3, color = "gray45") +
    labs(x = "Release side (ft)", y = "Release height (ft)") +
    theme_minimal(base_size = 7.5) +
    theme(legend.position = "none", panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray92"),
          panel.border = element_rect(fill = NA, color = "gray75", linewidth = 0.4),
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
  section(0.4, top_y, 2.75, 2.85, "Movement", "pitcher's view, both sides", function() {
    p <- plot_movement(d, hide = hide) |> shrink_layers(0.45)
    # The app draws with clip off so the arm-angle and extension labels can sit
    # above the panel, which also lets a steep slot line run on into the title
    # above. Clipped here, with the panel raised to hold the labels.
    p$coordinates$limits$y <- c(-22, 26)
    p$coordinates$clip <- "on"
    # No legend at this size: the pitch codes in both tables carry the colours.
    p <- p + theme(legend.position = "none",
                   text = element_text(size = 7.5), axis.text = element_text(size = 6.5),
                   plot.margin = ggplot2::margin(10, 2, 2, 2))
    draw_plot(p)
  })
  section(3.35, top_y, 2.35, 2.85, "Release", "pitcher's view, both sides", function() {
    draw_plot(plot_release(d, hide = hide))
  })

  both <- identical(hand, "All")
  pct <- function(x) ifelse(is.na(x), "-", sprintf("%.1f", x))
  # Each bucket shaded by its own share, a light wash so the eye finds the
  # pitch he leans on in each count without reading every number.
  ramp <- grDevices::colorRamp(c("#ffffff", "#4a6f9c"))
  shade <- function(u) {
    fill <- matrix("white", nrow(u), ncol(u)); col <- matrix("#1a1a1a", nrow(u), ncol(u))
    for (j in 2:ncol(u)) {
      v  <- pmin(1, u[[j]] / 80); ok <- !is.na(v)
      fill[ok, j] <- grDevices::rgb(ramp(v[ok]), maxColorValue = 255)
      col[ok & v > 0.6, j] <- "white"
    }
    list(cells = data.frame(lapply(u, function(x) if (is.numeric(x)) pct(x) else x),
                            check.names = FALSE, stringsAsFactors = FALSE),
         fill = fill, col = col,
         bold = matrix(names(u) == "All Counts", nrow(u), ncol(u), byrow = TRUE))
  }
  side_usage <- function(h) {
    u <- count_usage_tbl(d, h)
    if (length(hide)) u <- filter(u, !pitch_type %in% hide)
    u$pitch_type <- as.character(u$pitch_type)
    # Early Count left off the sheet by request: 0-0, 0-1 and 1-0 overlap Ahead
    # and Behind, and the column added width without a separate read.
    u$`Early Count` <- NULL
    as.data.frame(u, check.names = FALSE)
  }
  # OVERALL, not ALL: it is his usage against that side across every count,
  # and "ALL" read as a count bucket beside the others.
  bucket_head <- c("OVERALL", "AHEAD", "BEHIND", "<2K", "2K")
  side_n <- function(h) format(sum(d$stand == h), big.mark = ",")

  if (both) {
    # Two tables, one per batter side, because a count plan is a per-side plan:
    # Ragans' 2K slider is 19% overall and mostly a lefty pitch. Rows are the
    # pitches he threw to that side, so a pitch he never shows lefties is
    # absent from the vs LHH table rather than a row of zeroes.
    uL <- side_usage("L"); uR <- side_usage("R")
    # Shared row height, sized so both tables fit the box at their own lengths.
    row_in <- min(0.19, (2.85 - 0.2 - 2 * 0.36) / (nrow(uL) + nrow(uR) + 2))
    section(5.95, top_y, 4.65, 2.85, "Usage by count",
            "share of pitches in that count", function() {
      y <- 0
      for (h in c("L", "R")) {
        u <- if (h == "L") uL else uR
        sh <- shade(u)
        pushViewport(viewport(y = unit(1, "npc") - unit(y, "in"), just = "top",
                              height = unit(0.2 + row_in * (nrow(u) + 1), "in")))
        grid.text(sprintf("%s  \u00b7  %s pitches", hand_label(h), side_n(h)),
                  x = 0, y = unit(1, "npc"), just = c("left", "top"),
                  gp = gpar(fontsize = 7.5, fontface = "bold", col = "#1f3a5f"))
        pushViewport(viewport(y = 0, height = unit(1, "npc") - unit(0.17, "in"), just = "bottom"))
        if (nrow(u)) {
          grid_table(sh$cells, c("PITCH", bucket_head), widths = c(1, 1.15, 0.95, 1, 0.8, 0.8),
                     fill = sh$fill, col = sh$col, bold = sh$bold,
                     fontsize = 7.5, row_h = unit(row_in, "in"))
        } else {
          grid.text("No pitches to this side in the window.", x = 0, y = unit(1, "npc"),
                    just = c("left", "top"), gp = gpar(fontsize = 7, col = "#888888"))
        }
        popViewport(2)
        y <- y + 0.2 + row_in * (nrow(u) + 1) + 0.16
      }
    })
  } else {
    usage <- side_usage(hand)
    # One side selected: vs LHH and vs RHH still sit in front of the count
    # buckets, because the platoon split is the first thing a coach plans
    # around and the count columns below describe only the selected side.
    side_share <- function(h) {
      s <- count_usage_tbl(d, h)
      s$`All Counts`[match(usage$pitch_type, as.character(s$pitch_type))]
    }
    # The selected side's overall share is already its own column under
    # Overall, so the OVERALL bucket would print it twice. Dropped here and the
    # selected side's column is bolded in its place.
    u <- cbind(data.frame(pitch_type = usage$pitch_type, L = side_share("L"),
                          R = side_share("R"), check.names = FALSE),
               usage[, setdiff(names(usage), c("pitch_type", "All Counts"))])
    sh <- shade(u)
    sh$bold[, names(u) == hand] <- TRUE
    section(5.95, top_y, 4.65, 2.85, "Usage by count",
            paste("count columns", hand_txt), function() {
      grid_table(sh$cells, c("PITCH", "vs LHH", "vs RHH", bucket_head[-1]),
                 widths = c(1, 1.15, 1.15, 0.95, 1, 0.8, 0.8),
                 fill = sh$fill, col = sh$col, bold = sh$bold, fontsize = 7.5,
                 row_h = unit(0.2, "in"),
                 group_header = list("Overall" = c(2, 3),
                                     "By count (share of pitches in that count)" = c(4, 7)))
    })
  }

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
  side_types <- function(h) {
    x <- if (h == "All") d else d[d$stand == h, ]
    length(setdiff(unique(as.character(x$pitch_type)), hide))
  }
  # `panel` is the panel edge in inches. The in-panel "Usage 33%  IZ 57%"
  # label is sized for a one-inch panel and scales down with it, or six
  # pitches a side push it past the panel edge.
  hm_plot <- function(h, panel = 1) {
    p <- plot_heatmap(d, h, hide = hide) |> shrink_layers(0.5 * min(1, panel / 0.85))
    p + theme(strip.text.x = element_text(face = "bold", size = 8, margin = ggplot2::margin(b = 2)),
              strip.text.y.left = element_text(face = "bold", size = 8, angle = 90,
                                               margin = ggplot2::margin(r = 2)),
              panel.spacing = unit(0.25, "lines"),
              plot.margin = ggplot2::margin(2, 2, 2, 2))
  }
  section(0.4, hm_y, 10.2, hm_h, "Location",
          paste(if (both) "vs LHH and vs RHH" else hand_txt,
                "\u00b7 catcher's view \u00b7 Pre-2K and 2K counts"), function() {
    body_h <- hm_h - 0.2
    if (!both) {
      # Narrower than the box when the arsenal is small, so each panel stays
      # square at the box's height rather than stretching a two-pitch pitcher.
      w_in <- min(10.2, (body_h - 0.15) / 2 * side_types(hand) + 0.3)
      pushViewport(viewport(x = 0, width = unit(w_in, "in"), just = "left"))
      draw_plot(hm_plot(hand, (body_h - 0.15) / 2))
      popViewport()
      return(invisible())
    }
    # Two blocks side by side, one panel size for both so a lefty panel and a
    # righty panel read at the same scale. Sized by the wider block, and by
    # height, whichever binds first.
    gap <- 0.25; block_w <- (10.2 - gap) / 2
    n_max <- max(side_types("L"), side_types("R"), 1)
    panel <- min((body_h - 0.2 - 0.4) / 2, (block_w - 0.35) / n_max)
    for (k in 1:2) {
      h <- c("L", "R")[k]
      x0 <- (k - 1) * (block_w + gap)
      n  <- side_types(h)
      grid.text(sprintf("%s  \u00b7  %s pitches", hand_label(h), side_n(h)),
                x = unit(x0 + 0.27, "in"), y = unit(1, "npc"), just = c("left", "top"),
                gp = gpar(fontsize = 7.5, fontface = "bold", col = "#1f3a5f"))
      if (!n) next
      pushViewport(viewport(x = unit(x0, "in"), y = unit(1, "npc") - unit(0.18, "in"),
                            width = unit(panel * n + 0.35, "in"),
                            height = unit(panel * 2 + 0.4, "in"), just = c("left", "top")))
      draw_plot(hm_plot(h, panel))
      popViewport()
    }
  })

  # ---- Footer ----
  foot <- c(
    chart_hidden_note(d, hide),
    "Ahead 0-1, 0-2, 1-2, 2-2. Behind 1-0, 2-0, 3-0, 2-1, 3-1; buckets overlap, so rows do not sum to 100",
    paste("# is the sample behind each row; a number in parentheses is that rate's own",
          "sample (swings, out-of-zone pitches, PA or balls in play) where it is thin."))
  grid.text(paste(foot, collapse = "\n"), x = unit(0.4, "in"), y = unit(0.08, "in"),
            just = c("left", "bottom"), gp = gpar(fontsize = 6, col = "#999999", lineheight = 1.15))
  invisible(path)
}
