# context_tables.R
#
# Renderers for the Context tab. Presentation only: every number arrives
# computed from R/cohorts.R or R/release_context.R, and nothing here decides a
# value. Same split as tables.R against features.R.
#
# ---- What this tab is for ----------------------------------------------------
#
# Search finds a pitcher by shape. This one finds a pitcher whose shape or
# release is unusual GIVEN his slot and his body, which is a question no filter
# on raw values can express. The league explorer is the headline and the
# single-pitcher blocks below it are the drill-down, because the discovery
# direction is what the tab is for.


#' The nested baselines, both metrics, as one table
#'
#' Two metrics as row groups rather than a metric dropdown. The dropdown made
#' the reader choose before he had seen anything, and the two he would have
#' chosen between are the two that fit on the page, so they are both always
#' shown. One fewer control, one more fact.
#'
#' A baseline under min_n keeps its row and is greyed, and its percentile is
#' already NA from cohort_delta(). Dropping the row would hide the most
#' informative case: a pitcher with no peer group at all.
baselines_gt <- function(fb, pitch_type, hand_word, min_n = COHORT_MIN_N) {
  lab <- c(velo = "VELOCITY", ivb = "INDUCED VERTICAL BREAK", hb = "HORIZONTAL BREAK",
           spin = "SPIN", whiff_pct = "WHIFF%", chase_pct = "CHASE%", xwoba = "xwOBA")
  d <- fb
  d$group <- unname(ifelse(d$metric %in% names(lab), lab[d$metric], toupper(d$metric)))
  d$group <- factor(d$group, levels = unname(lab[intersect(
    c("velo","ivb","hb","spin","whiff_pct","chase_pct","xwoba"), unique(d$metric))]))
  d <- d[order(d$group), , drop = FALSE]

  # z arrives computed from cohort_delta(), which divides an ADJUSTED delta by
  # the within-cohort fit's residual SD rather than by the raw cohort SD. It was
  # the raw SD until 2026-09-12, which made z depend on how much velocity
  # variation happened to sit in that particular cohort: the one comparison
  # across baselines the column exists to support was the one it could not
  # carry. The statistics live in the engine; this file only formats them.

  g <- d |>
    dplyr::select(group, baseline, cohort_n, cohort_mean, cohort_sd, z,
                  target, delta, pctile) |>
    gt::gt(groupname_col = "group") |>
    gt::cols_label(baseline = "BASELINE", cohort_n = "COHORT N",
                   cohort_mean = "COHORT MEAN", cohort_sd = "COHORT SD",
                   z = "Z", target = "HIS VALUE",
                   delta = "DELTA", pctile = "PCTILE") |>
    gt::tab_header(title = paste0(hand_word, " ", pitch_type,
                                  " against release-matched peers")) |>
    gt::cols_align("center") |>
    gt::cols_align("left", columns = baseline) |>
    gt::cols_width(baseline ~ gt::px(190), everything() ~ gt::px(104)) |>
    gt::fmt_number(columns = c(cohort_mean, target, cohort_sd), decimals = 1) |>
    gt::fmt_number(columns = c(delta, z), decimals = 1, force_sign = TRUE) |>
    # xwOBA lives on a different scale from everything above it, so one decimal
    # would render every row as .3 and every delta as +0.0. Three decimals and
    # two for the delta, matching how the Characteristics table prints it.
    gt::fmt_number(columns = c(cohort_mean, target, cohort_sd), decimals = 3,
                   rows = which(d$metric == "xwoba")) |>
    gt::fmt_number(columns = c(delta, z), decimals = 2, force_sign = TRUE,
                   rows = which(d$metric == "xwoba")) |>
    gt::sub_missing(missing_text = "—") |>
    gt::tab_options(table.width = gt::px(TABLE_WIDTH_PX),
                    table.font.size = gt::px(13),
                    row_group.font.weight = "bold")

  thin <- which(d$cohort_n < min_n)
  if (length(thin)) {
    g <- g |> gt::tab_style(gt::cell_text(color = PCTILE_GREY, style = "italic"),
                            gt::cells_body(rows = thin))
  }
  # A rate off too few swings, out-of-zone pitches or plate appearances is
  # greyed the same way it is everywhere else in the app, and its own
  # denominator is printed beside the value rather than left to be guessed.
  under <- which(is.finite(d$target_denom) & is.finite(d$denom_floor) &
                 d$target_denom < d$denom_floor)
  if (length(under)) {
    g <- g |> gt::tab_style(gt::cell_text(color = PCTILE_GREY, style = "italic"),
                            gt::cells_body(columns = c(target, delta, z, pctile),
                                           rows = under))
  }
  adj <- unique(stats::na.omit(d$controls))
  mode_note <- if (!is.null(attr(fb, "mode")) && identical(attr(fb, "mode"), "fixed_k"))
    sprintf(paste0("Each matched baseline is the **%d nearest** peers on that axis, so the ",
                   "rows are the same size and can be read against one another. Fixed windows ",
                   "gave 22 and 61, and half the spread between them was window width."),
            attr(fb, "k") %||% COHORT_K)
  else "Each matched baseline is everyone inside the window set above, so the Ns differ by design."

  # How far the two matched baselines are the same men. Two rows built from 20
  # of the same 25 are not describing two peer groups, and nothing in the Ns
  # would show it.
  ov <- baseline_overlap(fb)
  ov_note <- if (is.null(ov)) "" else paste0(" ", paste(vapply(seq_len(nrow(ov)),
    function(i) sprintf("**%s** and **%s** share **%d** of their %d and %d pitchers.",
                        ov$a[i], ov$b[i], ov$shared[i], ov$n_a[i], ov$n_b[i]),
    character(1)), collapse = " "))

  den_note <- if (!length(under)) "" else paste0(
    " Greyed rates sit under their sample floor: ",
    paste(unique(sprintf("%s off %g %s", d$group[under], d$target_denom[under],
                         c(whiff_pct = "swings", chase_pct = "out-of-zone pitches",
                           xwoba = "plate appearances")[d$metric[under]])),
          collapse = ", "), ".")

  g |> gt::tab_source_note(gt::md(paste0(
    ov_note, den_note, " ",
    mode_note, " **Z** is the delta in cohort SDs, which is what makes two deltas ",
    "comparable: the same +2.6 means very different things against a spread of 0.8 ",
    "and a spread of 2.5. Every delta carries its cohort N; rows below ", min_n,
    " peers are greyed and their percentile withheld. ",
    if (length(adj)) sprintf(paste0("Delta is adjusted for **%s** within the cohort, because a ",
                                    "release-height cohort is partly a velocity cohort and a raw ",
                                    "difference would double count."), paste(adj, collapse = ", "))
    else "Delta is the raw difference of means, unadjusted.")))
}


#' The cohort members, nearest first
#'
#' Distance is in tolerance units, so one window width on each matched axis is
#' distance 1 whatever the axes are and the ordering means the same thing across
#' different match_on choices.
#' Sorted by distance to the target, with the distance itself hidden. The
#' ordering is the useful part of that number; the value is in tolerance widths
#' or pooled SDs depending on mode, which is a unit nobody reading a comparables
#' list wants to hold in their head.
#'
#' HB, listed height and DIST were dropped 2026-09-12. The question this table
#' answers is what the peers' fastballs do, and three columns that answer a
#' different question were making it wider than the screen.
comparables_gt <- function(cohort, max_rows = 15L) {
  m <- cohort$members
  capped <- nrow(m) > max_rows
  n_all  <- nrow(m)
  m <- utils::head(m, max_rows)

  # The target rides on top of his own comparables. Reading a peer list while
  # scrolling back to the baseline table for the one line you are comparing
  # against is the kind of friction that makes a table go unread. He is the
  # same columns, so he can simply be a row.
  tgt <- cohort$target
  tp  <- cohort$target_profile
  keep <- c("player_name", "pitch_team", "pitches", "velo", "ivb", "rel_z", "arm",
            "whiff_pct", "chase_pct", "xwoba", "swings", "oz", "pa")
  grab <- function(d, cols) {
    out <- lapply(cols, function(cl) if (cl %in% names(d)) d[[cl]][1] else NA)
    names(out) <- cols
    as.data.frame(out, stringsAsFactors = FALSE)
  }
  target_row <- grab(cbind(tgt[1, , drop = FALSE],
                           rel_z = tp$rel_z[1], arm = tp$arm[1]), keep)
  body  <- as.data.frame(m, stringsAsFactors = FALSE)[, keep, drop = FALSE]
  shown <- rbind(target_row, body)

  # Rates carry their own denominator and grey below the floor, the same rule
  # and the same floors as the Characteristics and Search tables. A 60% whiff
  # off 5 swings renders, greyed, with "(5)" beside it: shown, marked, never
  # suppressed.
  rate_cell <- function(v, den, floor, digits = 1, strip_zero = FALSE) {
    txt <- if (strip_zero) sub("^0", "", sprintf(paste0("%.", digits, "f"), v))
           else sprintf(paste0("%.", digits, "f"), v)
    txt[!is.finite(v)] <- "\u2014"
    thin <- is.finite(den) & den < floor
    txt[thin] <- paste0(txt[thin], " (", den[thin], ")")
    txt
  }
  fl <- function(metric) METRIC_SPEC$floor[METRIC_SPEC$metric == metric]
  shown$whiff_cell <- rate_cell(shown$whiff_pct, shown$swings, fl("whiff_pct"))
  shown$chase_cell <- rate_cell(shown$chase_pct, shown$oz,     fl("chase_pct"))
  shown$xwoba_cell <- rate_cell(shown$xwoba,     shown$pa,     fl("xwoba"), 3, TRUE)
  thin_rows <- list(
    whiff_cell = which(is.finite(shown$swings) & shown$swings < fl("whiff_pct")),
    chase_cell = which(is.finite(shown$oz)     & shown$oz     < fl("chase_pct")),
    xwoba_cell = which(is.finite(shown$pa)     & shown$pa     < fl("xwoba")))

  g <- shown |>
    dplyr::select(player_name, pitch_team, pitches, velo, ivb, rel_z, arm,
                  whiff_cell, chase_cell, xwoba_cell) |>
    gt::gt() |>
    gt::cols_label(player_name = "PITCHER", pitch_team = "TEAM", pitches = "N",
                   velo = "VELO", ivb = "IVB", rel_z = "REL HT",
                   arm = "ARM ANGLE", whiff_cell = "WHIFF%",
                   chase_cell = "CHASE%", xwoba_cell = "xwOBA") |>
    gt::tab_header(title = sprintf("Comparables: matched on %s",
                                   paste(gsub("_", " ", cohort$match_on), collapse = " + "))) |>
    gt::cols_align("center") |>
    gt::cols_align("left", columns = player_name) |>
    gt::cols_width(player_name ~ gt::px(178), pitch_team ~ gt::px(76),
                   everything() ~ gt::px(88)) |>
    gt::fmt_number(columns = c(velo, ivb, arm), decimals = 1) |>
    gt::fmt_number(columns = rel_z, decimals = 2) |>
    gt::tab_options(table.width = gt::px(TABLE_WIDTH_PX),
                    table.font.size = gt::px(13)) |>
    # Row 1 is the target, not a peer. Bold plus a rule under it plus a tinted
    # ground: three channels, because a single bold row reads as an accident in
    # a table whose other rows are also names.
    gt::tab_style(gt::cell_text(weight = "bold"),
                  gt::cells_body(rows = 1)) |>
    gt::tab_style(gt::cell_fill(color = "#F2F4F7"),
                  gt::cells_body(rows = 1)) |>
    gt::tab_style(gt::cell_borders(sides = "bottom", color = "#333", weight = gt::px(2)),
                  gt::cells_body(rows = 1))
  for (cl in names(thin_rows)) {
    if (length(thin_rows[[cl]])) g <- g |> gt::tab_style(
      gt::cell_text(color = PCTILE_GREY, style = "italic"),
      gt::cells_body(columns = dplyr::all_of(cl), rows = thin_rows[[cl]]))
  }
  g <- g |> gt::tab_source_note(gt::md(paste0(
    "Rates are over the selected date range and batter side. A figure in ",
    "parentheses is that value's own denominator where it sits under the sample ",
    "floor: swings for WHIFF%, out-of-zone pitches for CHASE%, plate appearances ",
    "for xwOBA. Same floors as the Characteristics and Search tables.")))
  if (capped) g <- g |> gt::tab_header(
    title = sprintf("Comparables: matched on %s",
                    paste(gsub("_", " ", cohort$match_on), collapse = " + ")),
    subtitle = sprintf("nearest %d of %d members", nrow(m), n_all))
  g
}


#' Arm angle against release height, with the target called out
#'
#' The one picture that shows why this tab exists. The cloud is 0.82 correlated,
#' so the interesting pitchers are the ones off the line, and the residual
#' column in the explorer is the vertical distance to it.
#'
#' Cohort members are drawn on top in the pitch colour when a cohort is live, so
#' the scatter answers "who did I just compare him to" without reading the table.
plot_release_space <- function(prof, target_id = NULL, cohort = NULL,
                               pitch_type = NULL) {
  d <- prof[is.finite(prof$arm) & is.finite(prof$rel_z), , drop = FALSE]
  col <- if (!is.null(pitch_type) && pitch_type %in% names(pitch_colors))
           pitch_colors[[pitch_type]] else "#C0392B"

  ylim <- stats::quantile(d$rel_z, c(0.01, 0.99), na.rm = TRUE)
  n_out <- sum(d$rel_z < ylim[1] | d$rel_z > ylim[2], na.rm = TRUE)
  # The target is never clipped out of his own chart. If he sits outside the
  # 1st-99th, the axis opens to hold him: a panel that silently omits the one
  # pitcher it is about is worse than a compressed one.
  if (!is.null(target_id)) {
    t <- d[d$pitcher == target_id, , drop = FALSE]
    if (nrow(t) && is.finite(t$rel_z[1]))
      ylim <- c(min(ylim[1], t$rel_z[1] - 0.15), max(ylim[2], t$rel_z[1] + 0.15))
  }
  sub_txt <- paste0("Dashed line is the league trend. Above it means a higher release ",
                    "than the slot implies. Coloured points are his comparables.",
                    if (n_out > 0) sprintf(" Axis clipped to the 1st-99th percentile; %d pitcher%s outside it.",
                                           n_out, if (n_out == 1) "" else "s") else "")

  g <- ggplot2::ggplot(d, ggplot2::aes(arm, rel_z)) +
    ggplot2::geom_point(color = "gray75", size = 1.9) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                         color = "gray35", linetype = "dashed", linewidth = 0.7) +
    # Clipped to the 1st-99th percentile of release height. One submariner at
    # 1.26 ft stretched the axis over four feet of empty space and squeezed the
    # 6.0-to-6.5 band, where most of the league and nearly every interesting
    # comparison actually lives, into a few pixels. Clipped rather than filtered:
    # the points are still in the data and the subtitle says the axis is cut, so
    # a reader is never shown a league that quietly excludes its extremes.
    ggplot2::coord_cartesian(ylim = ylim, expand = TRUE) +
    ggplot2::labs(x = "Arm Angle (deg)", y = "Release Height (ft)",
                  title = "Where he lets go of the ball, against every pitcher of his hand",
                  subtitle = sub_txt) +
    ggplot2::theme_minimal(base_size = 13) +
    # Pinned, because the pane is far wider than it is tall and an unconstrained
    # panel stretched this to a 2000px ribbon in which a half-foot of release
    # height read as a sliver. The slope of the league trend is the subject of
    # the chart, and a slope is only readable at a fixed aspect.
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   aspect.ratio = 0.34,
                   plot.title = ggplot2::element_text(face = "bold", size = 13),
                   plot.subtitle = ggplot2::element_text(color = "gray35", size = 11))

  if (!is.null(cohort) && nrow(cohort$members)) {
    mem <- d[d$pitcher %in% cohort$members$pitcher, , drop = FALSE]
    if (nrow(mem)) g <- g + ggplot2::geom_point(
      data = mem, color = col, alpha = 0.55, size = 2.8)
  }
  if (!is.null(target_id)) {
    t <- d[d$pitcher == target_id, , drop = FALSE]
    if (nrow(t)) {
      g <- g +
        ggplot2::geom_point(data = t, shape = 21, fill = col, color = "black",
                            stroke = 0.9, size = 5.5) +
        ggplot2::geom_text(data = t, ggplot2::aes(label = player_name),
                           vjust = -1.3, size = 4, fontface = "bold")
    }
  }
  g
}


#' ---- CURRENTLY UNRENDERED ---------------------------------------------------
#'
#' strip_data(), plot_strip() and strip_verdict() built the velocity, break and
#' release panels that sat at the top of the Context tab until 2026-09-18. They
#' were removed because they compared ONE pitch to 25 peers on exactly the
#' traits the league percentile block states in a sentence, and said nothing
#' about the rest of the arsenal.
#'
#' Kept rather than deleted, and kept tested, because the primitive is sound and
#' the reason it went was layout rather than correctness. Nothing in app.R calls
#' them today. If they are still unrendered next time this file is opened, that
#' is the moment to delete them rather than to keep paying for them.
#'
#' The points behind a strip, target last
#'
#' Split out from the plot so that shiny::nearPoints() can be handed the SAME
#' coordinates the chart drew. That is the whole reason the vertical offset
#' below is deterministic rather than jittered: geom_jitter moves points at
#' render time and nearPoints matches against the data, so a jittered strip
#' answers hovers with the wrong peer. It also means no seed, where the old
#' version needed one to stop the dots moving on every reactive tick.
#'
#' The offset is a beeswarm by binning: points are spread within narrow x bins,
#' so overlapping peers separate vertically instead of stacking into one dot,
#' and a dense part of the axis LOOKS dense. Carries no information of its own.
strip_data <- function(cohort, metric) {
  mem <- cohort$members
  tgt <- cohort$target
  v   <- mem[[metric]]
  ok  <- is.finite(v)
  tv  <- tgt[[metric]][1]
  if (!any(ok) || !is.finite(tv)) return(NULL)

  d <- data.frame(player_name = as.character(mem$player_name[ok]),
                  x = v[ok], is_target = FALSE, stringsAsFactors = FALSE)
  d <- rbind(d, data.frame(player_name = as.character(tgt$player_name[1]),
                           x = tv, is_target = TRUE, stringsAsFactors = FALSE))
  rng <- range(d$x)
  span <- if (diff(rng) > 0) diff(rng) else 1
  bin  <- floor((d$x - rng[1]) / span * 24)
  d$y  <- 0
  for (b in unique(bin)) {
    i <- which(bin == b)
    i <- i[order(d$x[i])]
    # 0, +1, -1, +2, -2 ... so a bin of one sits on the axis and a crowded bin
    # opens symmetrically around it.
    k <- seq_along(i) - 1
    d$y[i] <- ifelse(k == 0, 0, ceiling(k / 2) * ifelse(k %% 2 == 1, 1, -1)) * 0.17
  }
  d
}


#' One metric, one cohort, as a strip
plot_strip <- function(cohort, metric, pitch_type, digits = 1,
                       strip_zero = FALSE) {
  d <- strip_data(cohort, metric)
  if (is.null(d)) return(NULL)
  peers <- d[!d$is_target, , drop = FALSE]
  td    <- d[d$is_target, , drop = FALSE]
  if (!nrow(peers)) return(NULL)

  mu  <- mean(peers$x)
  bet <- context_better(metric)

  # Tint by which side of the peer average he sits on, where the metric has a
  # side. NOTE this is a DIFFERENT colour language from the rest of the app,
  # where the percentile ramp makes red "better" and blue "worse" on every
  # table. Here red means worse. The two meet on the Characteristics tab, so
  # this is worth revisiting rather than assuming it reads.
  fill <- if (identical(bet, "none")) {
    if (!is.null(pitch_type) && pitch_type %in% names(pitch_colors))
      pitch_colors[[pitch_type]] else "#7F8C8D"
  } else {
    better <- if (identical(bet, "high")) td$x[1] > mu else td$x[1] < mu
    if (better) "#2E8B57" else "#C0392B"
  }

  fmt <- function(x) {
    t <- sprintf(paste0("%.", digits, "f"), x)
    if (strip_zero) sub("^0", "", t) else t
  }

  rng <- range(c(d$x, mu))
  pad <- max(diff(rng) * 0.12, 1e-6)

  ggplot2::ggplot(peers, ggplot2::aes(x, y)) +
    # The peer average, behind everything, thin and pale. It is the thing every
    # caption compares to, so it should be visible on the axis rather than only
    # in the text under it.
    ggplot2::geom_vline(xintercept = mu, color = "gray72", linewidth = 0.5) +
    ggplot2::annotate("text", x = mu, y = 0.66, label = "peer avg",
                      size = 3.1, color = "gray55", vjust = 0) +
    ggplot2::geom_point(color = "gray55", alpha = 0.45, size = 3.4) +
    ggplot2::geom_point(data = td, shape = 21, fill = fill, color = "black",
                        stroke = 0.9, size = 7) +
    ggplot2::geom_text(data = td, ggplot2::aes(label = fmt(x)),
                       vjust = -1.5, size = 4.6, fontface = "bold") +
    ggplot2::scale_x_continuous(limits = c(rng[1] - pad, rng[2] + pad)) +
    ggplot2::scale_y_continuous(limits = c(-0.72, 0.86)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      legend.position  = "none",
      axis.title       = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(color = "gray30", size = 12),
      axis.line.x      = ggplot2::element_line(color = "gray70"),
      axis.ticks.x     = ggplot2::element_line(color = "gray70"),
      plot.margin      = ggplot2::margin(t = 10, r = 14, b = 2, l = 14))
}


#' Verdict, rank and delta for one metric against its cohort
#'
#' The verdict answers the panel's own question instead of leaving the reader to
#' derive it from three numbers. Derived, never hardcoded.
#'
#' The middle third is deliberately NOT a yes or a no. Forcing a binary on a
#' pitcher sitting at the cohort median makes the header assert a difference the
#' strip plot visibly contradicts, and a reader who catches the page doing that
#' once stops trusting the other panels.
strip_verdict <- function(cohort, metric, unit = "", digits = 1,
                          strip_zero = FALSE,
                          more = "Yes", less = "No") {
  d <- strip_data(cohort, metric)
  if (is.null(d)) return(NULL)
  peers <- d[!d$is_target, , drop = FALSE]
  tv    <- d$x[d$is_target][1]
  if (!nrow(peers)) return(NULL)

  mu  <- mean(peers$x)
  bet <- context_better(metric)
  n   <- nrow(d)

  # Ranked among himself AND his peers, because he is one of the values being
  # ordered. "3rd of 25" against 25 peers would be describing a 26-man list.
  ord  <- if (identical(bet, "low")) order(d$x) else order(-d$x)
  rank <- which(d$is_target[ord])

  f <- function(x, sign = FALSE) {
    t <- sprintf(paste0(if (sign) "%+." else "%.", digits, "f"), x)
    if (strip_zero) sub("^(\\+?-?)0", "\\1", t) else t
  }
  pos  <- (rank - 0.5) / n
  mid  <- pos > (1/3) & pos < (2/3)
  word <- if (mid) "About the same as his peers" else if (tv > mu) more else less

  ordinal <- function(i) paste0(i, switch(as.character(i %% 100),
    "11" = "th", "12" = "th", "13" = "th",
    switch(as.character(i %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")))

  list(verdict = sprintf("%s, %s%s, %s of %d", word, f(tv - mu, TRUE), unit,
                         ordinal(rank), n),
       word = word, rank = rank, n = n, delta = tv - mu, mean = mu, target = tv,
       line = sprintf("%s%s \u00b7 peer avg %s%s \u00b7 %s%s \u00b7 %s of %d",
                      f(tv), unit, f(mu), unit, f(tv - mu, TRUE), unit,
                      ordinal(rank), n))
}
