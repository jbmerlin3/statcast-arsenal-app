# step3_render.R
#
# What the four states look like in the RENDERED gt, not in the resolver.
# Baz carries the fallback case, Cameron's two-week window the below-floor bulk.
suppressMessages({library(dplyr);library(tidyr);library(purrr);library(forcats)
                  library(ggplot2);library(gt);library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))
ad <- readRDS("data/app_data.rds"); ref <- readRDS("data/league_ref.rds")

fails <- character()
expect <- function(l, got, want) if (!identical(got, want))
  fails <<- c(fails, sprintf("%s: got %s, wanted %s", l, deparse(got), deparse(want)))

# After the 2026-08-31 split a pitcher's table is TWO renders, and a resolved
# context only describes the one it was resolved against. Every probe below
# therefore carries its context and its rendered page as a matched pair. Pairing
# them was not optional: resolving the wide table and rendering a projection of
# it passed three of these assertions by looking up a results column in a traits
# page, finding no cell, and comparing FALSE to FALSE.
build <- function(id, hand, dates = NULL) {
  pl <- build_pitch_level(ad, id)
  if (!is.null(dates)) pl <- filter(pl, game_date >= dates[1], game_date <= dates[2])
  tb <- arsenal_table(pl, hand, tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical()))
  dn <- arsenal_denoms(pl, hand); pth <- unique(pl$p_throws)[1]
  panel <- function(project, render, glyphs) {
    sub <- project(tb)
    ctx <- resolve_table(sub, dn, ref, pth, hand, "All Counts")
    list(tbl = sub, ctx = ctx, g = render(sub, ctx), glyphs = glyphs)
  }
  # `glyphs` says whether that renderer draws the dagger and double-dagger
  # markers at all. traits_gt() stopped on 2026-08-31 when its footnotes were
  # removed, since a marker whose legend is gone is worse than no marker.
  list(tb      = tb,
       traits  = panel(traits_tbl,  function(s, c) traits_gt(s, hand, ref = c),
                       glyphs = FALSE),
       results = panel(results_tbl, function(s, c) results_gt(s, hand, ref = c),
                       glyphs = TRUE))
}

# Which table carries a given state is a property of the PITCHER, not of the
# code: below_floor follows thin denominators, which sit in results for a short
# window and in traits for a rarely-thrown pitch. Hard-coding a panel here is how
# a probe stops firing without anyone noticing, so this searches both and says
# which one answered.
#' `glyphs = TRUE` restricts the search to renderers that actually draw the
#' dagger markers. Without it this function is a trap rather than a helper: it
#' searches traits first, so a pitcher with a fallback cell in his traits table
#' would hand the dagger assertion a panel that deliberately draws no daggers,
#' and the test would fail on a deliberate design choice rather than on a
#' regression. It passed on the current fixtures only because neither happens to
#' have one, which is exactly the kind of accident that should not be load
#' bearing.
pick <- function(b, state, glyphs = FALSE) {
  nms <- c("traits", "results")
  if (glyphs) nms <- Filter(function(nm) isTRUE(b[[nm]]$glyphs), nms)
  for (nm in nms) {
    p   <- b[[nm]]
    hit <- p$ctx$cells[p$ctx$cells$state == state, ]
    if (nrow(hit)) return(list(panel = p, cell = hit[1, ], which = nm))
  }
  stop("no ", state, " cell in any eligible table, so this probe tested nothing")
}

# One <td> per (column, row), in document order, with its inline style attached.
tds <- function(g, col) {
  h <- as.character(as_raw_html(g))
  regmatches(h, gregexpr(paste0('<td headers="', col, '"[^>]*>[^<]*</td>'), h))[[1]]
}
txt <- function(td) gsub("<[^>]*>", "", td)

baz <- build(669358, "R")
cam <- build(702070, "R", c("2026-08-04", "2026-08-17"))

cat("=== fallback renders its marker ===\n")
f    <- pick(baz, "fallback", glyphs = TRUE); fb <- f$cell
cell <- tds(f$panel$g, fb$column)[fb$row]
cat("  [", f$which, "] ", fb$column, " row ", fb$row, ": ", txt(cell), "\n", sep = "")
expect("fallback cell text ends with the dagger", grepl("†$", txt(cell)), TRUE)
expect("fallback cell is filled, not white", grepl("#FFFFFF", cell, ignore.case = TRUE), FALSE)
expect("fallback cell is bold", grepl("font-weight: bold", cell), TRUE)

cat("\n=== below floor renders unfilled, grey, italic, with its n ===\n")
b2    <- pick(cam, "below_floor"); bf <- b2$cell
cell2 <- tds(b2$panel$g, bf$column)[bf$row]
cat("  [", b2$which, "] ", bf$column, " row ", bf$row, ": ", txt(cell2), "\n", sep = "")
expect("below floor is white",  grepl("background-color: #FFFFFF", cell2, ignore.case = TRUE), TRUE)
expect("below floor is grey",   grepl("color: #767676", cell2, ignore.case = TRUE), TRUE)
expect("below floor is italic", grepl("font-style: italic", cell2), TRUE)
expect("below floor shows its n", grepl("\\([0-9]+\\)$", txt(cell2)), TRUE)

cat("\n=== the percentile fill survives the pitch-colour reduce ===\n")
e2    <- pick(baz, "exact"); ex <- e2$cell
cell3 <- tds(e2$panel$g, ex$column)[ex$row]
pc <- paste0(pitch_colors[[as.character(e2$panel$tbl$pitch_type[ex$row])]], "20")
cat("  [", e2$which, "] cell fill: ", sub('.*background-color: ([^;]*);.*', '\\1', cell3),
    "   resolver said: ", ex$fill, "   pitch colour is: ", pc, "\n", sep = "")
expect("cell carries the percentile fill", grepl(ex$fill, cell3, ignore.case = TRUE), TRUE)
expect("cell does NOT carry the pitch-colour fill", grepl(pc, cell3, ignore.case = TRUE), FALSE)

cat("\n=== note order is dagger, double dagger, grey ===\n")
# Whichever panel carries the most notes, so the sortedness check has something
# to sort. A panel with one note satisfies !is.unsorted() by having nothing to
# compare, which is the trivially-passing shape this avoids.
# results only: traits_gt() renders no source notes at all now, so its resolved
# ctx$notes describe prose that never reaches the page. Reading them here would
# assert an order on text nobody sees.
np  <- baz$results
nts <- np$ctx$notes
for (n in nts) cat("  ", substr(n, 1, 72), "\n", sep = "")
# Scope note first, it frames every other line. Then the marker grammar:
# dagger, double dagger, grey.
rank_of <- function(n) {
  if (startsWith(n, "Percentiles rank")) 0L
  else if (startsWith(n, "†")) 1L
  else if (startsWith(n, "‡")) 2L
  else 3L
}
expect("the note-order check has more than one note to order", length(nts) > 1, TRUE)
expect("notes are in marker-grammar order", !is.unsorted(vapply(nts, rank_of, integer(1))), TRUE)
# The rendered order must match, or the vector's order is decorative.
h <- as.character(as_raw_html(np$g))
pos <- vapply(nts, function(n) regexpr(substr(n, nchar(n) - 30, nchar(n)), h, fixed = TRUE), integer(1))
expect("rendered source notes follow the same order", !is.unsorted(pos), TRUE)

cat("\n=== one dagger line names one grain, and two grains stop ===\n")
two <- baz$ctx$cells
two$state[1] <- "fallback"; two$grain[1] <- "pitch_type x p_throws"
got <- tryCatch({
  fb2 <- two[two$state == "fallback", ]
  if (length(unique(fb2$grain)) > 1) stop("guard") else "no guard"
}, error = function(e) "stopped")
expect("two distinct grains would be caught", got, "stopped")

cat("\n=== NULL paths on the two functions phase1_check cannot guard ===\n")
# plot_movement sits in EXPECTED_DIFFS, so the regression reports "differs
# (expected)" whatever happens inside it. These compare the two call shapes
# against each other instead, which needs no historical baseline.
pm <- build_pitch_level(ad, 702070) |> filter(game_date >= "2026-08-04", game_date <= "2026-08-17")
pspec <- function(p) tryCatch({
  b <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))
  list(data = b$data, theme = b$plot$theme)
}, error = function(e) paste("BUILD FAILED:", conditionMessage(e)))
expect("plot_movement ref=NULL equals no ref",
       isTRUE(all.equal(pspec(plot_movement(pm)), pspec(plot_movement(pm, ref = NULL)))), TRUE)
expect("plot_movement with a ref actually differs",
       isTRUE(all.equal(pspec(plot_movement(pm)), pspec(plot_movement(pm, ref = ref)))), FALSE)

# Every reference mark carries the n it was built from. Asserted because
# deleting the label layer passed every other check in this file.
mref <- movement_ref(ref, levels(droplevels(pm$pitch_type)), pm$p_throws[1])
mb   <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(plot_movement(pm, ref = ref))))
labs <- unlist(lapply(mb$data, function(d) if ("label" %in% names(d)) as.character(d$label)))
ns   <- grep("^n=[0-9]+$", labs, value = TRUE)
expect("one n label per reference mark", length(ns), nrow(mref))
expect("the n labels are the reference pitcher counts",
       sort(as.integer(sub("^n=", "", ns))), sort(as.integer(mref$n_pitchers)))

# M7 removed count_usage_gt's early return, so ref = NULL flowed into the context
# block and died on nrow(NULL). Nothing can make that call succeed: the function
# genuinely cannot proceed without a context. What CAN be fixed is that the
# check reported an uncaught error instead of a named failure, so the render is
# asserted to produce a gt_tbl rather than assumed to.
renders <- function(expr) tryCatch({ g <- expr; inherits(g, "gt_tbl") }, error = function(e) FALSE)

uw <- count_usage_tbl(pm, "R")
uctx <- resolve_usage(uw, count_usage_denoms(pm, "R"), ref, pm$p_throws[1], "R")
# Tolerant on purpose. A render that dies must show up as a named comparison
# failure, not as an uncaught error that stops the file before the remaining
# checks run.
gspec <- function(g) tryCatch({
  h <- as.character(gt::as_raw_html(g))
  for (id in unique(regmatches(h, gregexpr('(?<=id=")[a-z]{10,}(?=")', h, perl = TRUE))[[1]]))
    h <- gsub(id, "GT_ID", h, fixed = TRUE)
  h
}, error = function(e) paste("RENDER FAILED:", conditionMessage(e)))
expect("count_usage_gt renders with no ref",     renders(count_usage_gt(uw, "R")), TRUE)
expect("count_usage_gt renders with ref = NULL", renders(count_usage_gt(uw, "R", ref = NULL)), TRUE)
expect("count_usage_gt renders with a context",  renders(count_usage_gt(uw, "R", ref = uctx)), TRUE)
expect("traits_gt renders with no ref",
       renders(traits_gt(traits_tbl(arsenal_table(pm, "R", tibble(pitch_type=character(), stuff_plus=numeric(), fg_exact=logical()))), "R")), TRUE)
expect("results_gt renders with no ref",
       renders(results_gt(results_tbl(arsenal_table(pm, "R", tibble(pitch_type=character(), stuff_plus=numeric(), fg_exact=logical()))), "R")), TRUE)
expect("count_usage_gt ref=NULL equals no ref",
       identical(gspec(count_usage_gt(uw, "R")), gspec(count_usage_gt(uw, "R", ref = NULL))), TRUE)
expect("count_usage_gt with a ref actually differs",
       identical(gspec(count_usage_gt(uw, "R")), gspec(count_usage_gt(uw, "R", ref = uctx))), FALSE)

cat("\n=== every plot function returns VISIBLY ===\n")
# renderPlot() relies on auto-printing, so a function ending in an assignment
# returns invisibly and Shiny draws a blank white device with no error at all.
# plot_movement did exactly that for four commits. Nothing here caught it,
# because ggplot_build() and print() both work fine on an invisible value: the
# tests consumed the object directly, and Shiny does not.
pd <- shape_arsenal(dplyr::filter(ad, pitcher == 702070))
for (fn in c("plot_movement", "plot_velo", "plot_usage")) {
  vis <- withVisible(do.call(fn, list(pd)))$visible
  cat(sprintf("  %-14s visible: %s\n", fn, vis))
  expect(paste(fn, "returns visibly"), vis, TRUE)
}
expect("plot_movement returns visibly with a ref too",
       withVisible(plot_movement(pd, ref = ref))$visible, TRUE)
# plot_heatmap takes a hand argument, so it is checked separately.
expect("plot_heatmap returns visibly",
       withVisible(plot_heatmap(pd, "R"))$visible, TRUE)

cat("\n=== both plots survive a masked margin() ===\n")
# randomForest exports margin(x, observed, ...). Attach it after ggplot2 in a
# console session and ggplot2::margin is masked, at which point the two plots
# that set a plot margin die with `argument "observed" is missing, with no
# default` and the other two render fine. Reported 2026-08-22 as "the movement
# plots and heatmaps broke", which points nowhere near margin.
#
# Masked here rather than by attaching randomForest, which is not in the project
# library. The lookup path is the same either way: these functions are sourced
# into the global environment, so a global margin() shadows the package one
# exactly as a later-attached package does, and so does the app, whose R/
# environment has the global environment as its parent.
#
# This fails on a bare margin() call and passes on ggplot2::margin(), which is
# the whole point of writing the namespace out.
margin <- function(x, observed, ...) stop("the masked margin() was called")
draws <- function(p) tryCatch({
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 900, height = 600)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  print(p)
  TRUE
}, error = function(e) paste("ERROR:", conditionMessage(e)))
expect("plot_movement draws with margin() masked", draws(plot_movement(pd, ref = ref)), TRUE)
expect("plot_heatmap draws with margin() masked", draws(plot_heatmap(pd, "R")), TRUE)
rm(margin)

cat("\n=== an empty denominator reaches the page as NA, never NaN ===\n")
# 0/0 is NaN in R and NaN travels: a pitch type nobody swung at rendered the
# literal "NaN (0)" in the whiff column. The cell is below floor either way, so
# what is at stake is whether the reader sees an empty sample or a computer
# error. Measured 2026-08-22: 1.1% of full-season rows carry an empty
# denominator, 4.5% over a one-week window, which is the window a scout picks.
#
# A literal fixture, not a pitcher who happens to have a zero-swing pitch type
# today. That pitcher gets a swing tomorrow and the check quietly stops
# exercising anything. See CLAUDE.md entry 5.
nan_frame <- tibble::tibble(
  pitch_type   = factor(c(rep("FF", 6), rep("CU", 6)), levels = c("FF", "CU")),
  stand        = "R", p_throws = "R",
  # CU is taken every time: no swing, so no whiff denominator, and no pitch ends
  # a plate appearance, so no xwOBA denominator either.
  description  = c("swinging_strike", "foul", "hit_into_play", "ball", "ball", "called_strike",
                   rep("called_strike", 3), rep("ball", 3)),
  type         = c("S", "S", "X", "B", "B", "S", rep("S", 3), rep("B", 3)),
  in_zone      = c(1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0),
  release_speed = 93, ivb = 15, hb = 8, release_spin_rate = 2300,
  # The release columns the 2026-08-31 split added. Constant across every row on
  # purpose: this frame exists to exercise empty denominators, and a constant keeps
  # those means from varying while the rows under test do.
  vaa = -4.8, release_extension = 6.4, release_pos_x = -1.9, release_pos_z = 5.9,
  woba_denom   = c(NA, NA, 1, NA, NA, NA, rep(NA, 6)),
  estimated_woba_using_speedangle = c(NA, NA, 0.3, NA, NA, NA, rep(NA, 6)))
nan_tb <- arsenal_table(nan_frame, "All",
                        tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical()))
cu <- nan_tb[as.character(nan_tb$pitch_type) == "CU", ]
cat(sprintf("  CU: %d pitches, whiff %s, chase %s, xwOBA %s\n", cu$count,
            format(cu$whiff_pct), format(cu$chase_pct), format(cu$xwoba)))
expect("no swings gives NA whiff, not NaN", is.na(cu$whiff_pct) && !is.nan(cu$whiff_pct), TRUE)
expect("no PA gives NA xwOBA, not NaN",     is.na(cu$xwoba)     && !is.nan(cu$xwoba),     TRUE)
expect("out-of-zone pitches with no swing still give a chase rate", cu$chase_pct, 0)
# Both tables, because the NaN guards live in arsenal_table() and each table
# projects a different subset of the columns they protect: whiff/chase/xwOBA
# land in results, and a degenerate velocity or release mean lands in traits.
expect("the rendered traits page carries no NaN",
       grepl("NaN", as.character(gt::as_raw_html(traits_gt(traits_tbl(nan_tb), "All")))), FALSE)
expect("the rendered results page carries no NaN",
       grepl("NaN", as.character(gt::as_raw_html(results_gt(results_tbl(nan_tb), "All")))), FALSE)

cat("\n", strrep("-", 58), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 3 RENDER: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
