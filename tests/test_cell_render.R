# The four cell states as rendered by gt (not just resolved), plus table widths, note
# order, plot visibility, the masked-margin guard and NaN handling.
suppressMessages({library(dplyr);library(tidyr);library(purrr);library(forcats)
                  library(ggplot2);library(gt);library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))
ad <- readRDS("data/app_data.rds"); ref <- readRDS("data/league_ref.rds")

fails <- character()
expect <- function(l, got, want) if (!identical(got, want))
  fails <<- c(fails, sprintf("%s: got %s, wanted %s", l, deparse(got), deparse(want)))

# Each table's context and rendered page are built as a matched pair
build <- function(id, hand, dates = NULL) {
  pl <- build_pitch_level(ad, id)
  if (!is.null(dates)) pl <- filter(pl, game_date >= dates[1], game_date <= dates[2])
  tb <- arsenal_table(pl, hand, tibble(pitch_type = character(), stuff_plus = numeric()))
  dn <- arsenal_denoms(pl, hand); pth <- unique(pl$p_throws)[1]
  panel <- function(project, render, glyphs) {
    sub <- project(tb)
    ctx <- resolve_table(sub, dn, ref, pth, hand, "All Counts")
    list(tbl = sub, ctx = ctx, g = render(sub, ctx), glyphs = glyphs)
  }
  # `glyphs`: whether the renderer draws the double dagger (traits_gt() doesn't)
  list(tb      = tb,
       traits  = panel(traits_tbl,  function(s, c) traits_gt(s, hand, ref = c),
                       glyphs = FALSE),
       results = panel(results_tbl, function(s, c) results_gt(s, hand, ref = c),
                       glyphs = TRUE))
}

# Find a cell in a given state in either table. `glyphs = TRUE` limits to renderers
# that draw the double dagger; `cols` narrows to specific columns.
pick <- function(b, state, glyphs = FALSE, cols = NULL) {
  nms <- c("traits", "results")
  if (glyphs) nms <- Filter(function(nm) isTRUE(b[[nm]]$glyphs), nms)
  for (nm in nms) {
    p   <- b[[nm]]
    hit <- p$ctx$cells[p$ctx$cells$state == state, ]
    if (!is.null(cols)) hit <- hit[hit$column %in% cols, ]
    if (nrow(hit)) return(list(panel = p, cell = hit[1, ], which = nm))
  }
  stop("no ", state, " cell in any eligible table, so this probe tested nothing")
}

# One <td> per (column, row) with its inline style
tds <- function(g, col) {
  h <- as.character(as_raw_html(g))
  regmatches(h, gregexpr(paste0('<td headers="', col, '"[^>]*>[^<]*</td>'), h))[[1]]
}
txt <- function(td) gsub("<[^>]*>", "", td)

baz <- build(669358, "R")
cam <- build(702070, "R", c("2026-08-04", "2026-08-17"))

cat("=== fallback renders like an exact cell ===\n")
f    <- pick(baz, "fallback", glyphs = TRUE); fb <- f$cell
cell <- tds(f$panel$g, fb$column)[fb$row]
cat("  [", f$which, "] ", fb$column, " row ", fb$row, ": ", txt(cell), "\n", sep = "")
expect("fallback cell has no marker", grepl("\u2020", txt(cell)), FALSE)
expect("fallback cell is filled, not white", grepl("#FFFFFF", cell, ignore.case = TRUE), FALSE)
# Fallback is not bold in the rendered HTML
expect("fallback cell is not bold", grepl("font-weight: bold", cell), FALSE)

cat("\n=== below floor renders unfilled, grey, italic, with its n ===\n")
# whiff_pct's denominator is swings: it carries an (n)
b2    <- pick(cam, "below_floor", cols = "whiff_pct"); bf <- b2$cell
cell2 <- tds(b2$panel$g, bf$column)[bf$row]
cat("  [", b2$which, "] ", bf$column, " row ", bf$row, ": ", txt(cell2), "\n", sep = "")
# A thin cell is colored; its (n) is what marks it
expect("below floor is not white",
       grepl("background-color: #FFFFFF", cell2, ignore.case = TRUE), FALSE)
expect("below floor is not greyed", grepl("color: #767676", cell2, ignore.case = TRUE), FALSE)
expect("below floor is not italic",  grepl("font-style: italic", cell2), FALSE)
expect("below floor shows its n", grepl("\\([0-9]+\\)$", txt(cell2)), TRUE)

# A thin cell on a COUNT-denominated metric carries no (n)
b3 <- pick(cam, "below_floor", cols = c("velocity", "ivb", "spin", "csw_pct", "zone_pct"))
cell3b <- tds(b3$panel$g, b3$cell$column)[b3$cell$row]
cat("  [", b3$which, "] ", b3$cell$column, " row ", b3$cell$row, ": ", txt(cell3b), "\n", sep = "")
expect("a COUNT-denominated thin cell carries NO parenthetical",
       grepl("\\([0-9]+\\)$", txt(cell3b)), FALSE)
expect("and it is still filled", grepl("background-color: #FFFFFF", cell3b, ignore.case = TRUE), FALSE)

cat("\n=== the percentile fill survives the pitch-colour reduce ===\n")
e2    <- pick(baz, "exact"); ex <- e2$cell
cell3 <- tds(e2$panel$g, ex$column)[ex$row]
pc <- paste0(pitch_colors[[as.character(e2$panel$tbl$pitch_type[ex$row])]], "20")
cat("  [", e2$which, "] cell fill: ", sub('.*background-color: ([^;]*);.*', '\\1', cell3),
    "   resolver said: ", ex$fill, "   pitch colour is: ", pc, "\n", sep = "")
expect("cell carries the percentile fill", grepl(ex$fill, cell3, ignore.case = TRUE), TRUE)
expect("cell does not carry the pitch-color fill", grepl(pc, cell3, ignore.case = TRUE), FALSE)

cat("\n=== the two tables render to the same total width ===\n")
# The two stacked tables render the same total width
total_px <- function(g) {
  h <- as.character(as_raw_html(g))
  w <- as.numeric(gsub("[^0-9]", "", regmatches(h, gregexpr("width:[0-9]+px", h))[[1]]))
  sum(w[w > 40])
}
tw <- total_px(baz$traits$g); rw <- total_px(baz$results$g)
cat(sprintf("  traits %dpx | results %dpx | target %dpx\n", tw, rw, TABLE_WIDTH_PX))
expect("traits table hits TABLE_WIDTH_PX",  tw, as.numeric(TABLE_WIDTH_PX))
expect("results table hits TABLE_WIDTH_PX", rw, as.numeric(TABLE_WIDTH_PX))
expect("the two tables are exactly equal",  tw == rw, TRUE)

cat("\n=== note order is scope, double dagger, small sample ===\n")
# The results table has the most notes to order
np  <- baz$results
nts <- np$ctx$notes
for (n in nts) cat("  ", substr(n, 1, 72), "\n", sep = "")
# Note order: scope, double dagger, small sample
rank_of <- function(n) {
  if (startsWith(n, "Percentiles rank")) 0L
  else if (startsWith(n, "‡")) 2L
  else 3L
}
expect("the note-order check has more than one note to order", length(nts) > 1, TRUE)
expect("notes are in marker-grammar order", !is.unsorted(vapply(nts, rank_of, integer(1))), TRUE)
# The rendered order must match
h <- as.character(as_raw_html(np$g))
pos <- vapply(nts, function(n) regexpr(substr(n, nchar(n) - 30, nchar(n)), h, fixed = TRUE), integer(1))
expect("rendered source notes follow the same order", !is.unsorted(pos), TRUE)

cat("\n=== NULL paths on the plot and usage renderers ===\n")
# Build the movement plot for a short window
pm <- build_pitch_level(ad, 702070) |> filter(game_date >= "2026-08-04", game_date <= "2026-08-17")
pspec <- function(p) tryCatch({
  b <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))
  list(data = b$data, theme = b$plot$theme)
}, error = function(e) paste("BUILD FAILED:", conditionMessage(e)))
expect("plot_movement builds", is.list(pspec(plot_movement(pm))), TRUE)

# League reference marks were removed; they must stay gone
mb   <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(plot_movement(pm))))
labs <- unlist(lapply(mb$data, function(d) if ("label" %in% names(d)) as.character(d$label)))
expect("no n= labels on the movement chart",
       length(grep("^n=[0-9]+$", labs, value = TRUE)), 0L)
shapes <- unlist(lapply(mb$data, function(d) if ("shape" %in% names(d)) unique(d$shape)))
expect("and no cross-shaped marks", any(shapes == 3), FALSE)

# Renders must return a gt_tbl, reported as a named failure if not
renders <- function(expr) tryCatch({ g <- expr; inherits(g, "gt_tbl") }, error = function(e) FALSE)

uw <- count_usage_tbl(pm, "R")
expect("count_usage_gt renders with no ref",     renders(count_usage_gt(uw, "R")), TRUE)
expect("traits_gt renders with no ref",
       renders(traits_gt(traits_tbl(arsenal_table(pm, "R", tibble(pitch_type=character(), stuff_plus=numeric()))), "R")), TRUE)
expect("results_gt renders with no ref",
       renders(results_gt(results_tbl(arsenal_table(pm, "R", tibble(pitch_type=character(), stuff_plus=numeric()))), "R")), TRUE)

cat("\n=== every plot function returns visibly ===\n")
# Plots must return visibly: renderPlot() relies on auto-printing
pd <- shape_arsenal(dplyr::filter(ad, pitcher == 702070))
for (fn in c("plot_movement", "plot_usage")) {
  vis <- withVisible(do.call(fn, list(pd)))$visible
  cat(sprintf("  %-14s visible: %s\n", fn, vis))
  expect(paste(fn, "returns visibly"), vis, TRUE)
}
# plot_heatmap takes a hand argument
expect("plot_heatmap returns visibly",
       withVisible(plot_heatmap(pd, "R"))$visible, TRUE)

cat("\n=== both plots survive a masked margin() ===\n")
# A global margin() masks ggplot2's (as randomForest does); plots must still draw
margin <- function(x, observed, ...) stop("the masked margin() was called")
draws <- function(p) tryCatch({
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 900, height = 600)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  print(p)
  TRUE
}, error = function(e) paste("ERROR:", conditionMessage(e)))
expect("plot_movement draws with margin() masked", draws(plot_movement(pd)), TRUE)
expect("plot_heatmap draws with margin() masked", draws(plot_heatmap(pd, "R")), TRUE)
rm(margin)

cat("\n=== an empty denominator reaches the page as NA, never NaN ===\n")
# Empty denominators must render as blanks, never "NaN" (literal fixture)
nan_frame <- tibble::tibble(
  pitch_type   = factor(c(rep("FF", 6), rep("CU", 6)), levels = c("FF", "CU")),
  stand        = "R", p_throws = "R",
  # CU is always taken: no swings, no PA-ending pitch
  description  = c("swinging_strike", "foul", "hit_into_play", "ball", "ball", "called_strike",
                   rep("called_strike", 3), rep("ball", 3)),
  type         = c("S", "S", "X", "B", "B", "S", rep("S", 3), rep("B", 3)),
  in_zone      = c(1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0),
  release_speed = 93, ivb = 15, hb = 8, release_spin_rate = 2300,
  # Constant release columns
  vaa = -4.8, release_extension = 6.4, release_pos_x = -1.9, release_pos_z = 5.9,
  # bb_type is "" off balls in play; run value held at 0
  bb_type = ifelse(description == "hit_into_play", "ground_ball", ""),
  delta_pitcher_run_exp = 0,
  woba_denom   = c(NA, NA, 1, NA, NA, NA, rep(NA, 6)),
  estimated_woba_using_speedangle = c(NA, NA, 0.3, NA, NA, NA, rep(NA, 6)))
nan_tb <- arsenal_table(nan_frame, "All",
                        tibble(pitch_type = character(), stuff_plus = numeric()))
cu <- nan_tb[as.character(nan_tb$pitch_type) == "CU", ]
cat(sprintf("  CU: %d pitches, whiff %s, chase %s, xwOBA %s\n", cu$count,
            format(cu$whiff_pct), format(cu$chase_pct), format(cu$xwoba)))
expect("no swings gives NA whiff, not NaN", is.na(cu$whiff_pct) && !is.nan(cu$whiff_pct), TRUE)
expect("no PA gives NA xwOBA, not NaN",     is.na(cu$xwoba)     && !is.nan(cu$xwoba),     TRUE)
expect("out-of-zone pitches with no swing still give a chase rate", cu$chase_pct, 0)
# Check both tables
expect("the rendered traits page carries no NaN",
       grepl("NaN", as.character(gt::as_raw_html(traits_gt(traits_tbl(nan_tb), "All")))), FALSE)
expect("the rendered results page carries no NaN",
       grepl("NaN", as.character(gt::as_raw_html(results_gt(results_tbl(nan_tb), "All")))), FALSE)

cat("\n", strrep("-", 58), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("CELL RENDER: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
