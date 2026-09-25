# step12_chart_rare.R
#
#   Rscript tests/step12_chart_rare.R
#
# The chart rule for stray pitch labels, added 2026-09-23: a type under 1% of
# the window AND under 10 pitches is left off the Movement and usage charts,
# kept in every table, and named in the note above the tabs.
#
# The ways it could go wrong, quietly:
#  1. It hides a real pitch. Either threshold alone would: share alone hides a
#     15-pitch cutter on a season, a count alone hides Fuentes's 3 sliders of 19.
#  2. It changes a number. Dropping the rows before the shares are computed
#     would take the hidden pitches out of the denominator and inflate every
#     other bar.
#  3. It hides silently. The note is the only place the reader learns it.
# Every count below is a literal, never read from CHART_MIN_SHARE or
# CHART_MIN_N (CLAUDE.md, checks that did not discriminate, entry 5).
suppressMessages({library(dplyr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- character(0)
expect <- function(what, got, want) {
  ok <- isTRUE(all.equal(got, want))
  cat(sprintf("  %-62s %s\n", what, if (ok) "ok" else
    paste0("FAIL got ", paste(format(got), collapse = ","),
           " wanted ", paste(format(want), collapse = ","))))
  if (!ok) fails <<- c(fails, what)
}
try_or <- function(expr, marker = "<error>") tryCatch(expr, error = function(e) {
  cat("    error: ", conditionMessage(e), "\n", sep = ""); marker })

mix <- function(...) {
  n <- c(...)
  data.frame(pitch_type = factor(rep(names(n), n), levels = names(n)),
             stand = rep(c("R", "L"), length.out = sum(n)))
}

cat("=== which types are hidden ===\n")
expect("4 SV in 1,000: hidden",               chart_hidden_types(mix(FF = 996, SV = 4)),  "SV")
expect("9 SV in 1,000 (0.9%): hidden",        chart_hidden_types(mix(FF = 991, SV = 9)),  "SV")
expect("10 SV in 1,000 (1.0%): kept",         chart_hidden_types(mix(FF = 990, SV = 10)), character())
expect("15 FC in 2,000 (0.75%): kept, n >= 10", chart_hidden_types(mix(FF = 1985, FC = 15)), character())
expect("9 SV in 500 (1.8%): kept, share >= 1%", chart_hidden_types(mix(FF = 491, SV = 9)),  character())
expect("Fuentes, 13 FF 3 FS 3 SL: nothing hidden",
       chart_hidden_types(mix(FF = 13, FS = 3, SL = 3)), character())

cat("\n=== hiding changes no number ===\n")
d <- mix(FF = 996, SV = 4)
shown  <- plot_usage(d)$data
hidden <- plot_usage(d, hide = "SV")$data
expect("the SV bar is gone", any(hidden$pitch_type == "SV"), FALSE)
expect("the SV level is gone, so no legend key",
       levels(hidden$pitch_type), "FF")
expect("FF's share is unchanged, SV stayed in the denominator",
       hidden$pct[hidden$pitch_type == "FF"], shown$pct[shown$pitch_type == "FF"])

cat("\n=== Harris, the case that prompted it ===\n")
ad <- readRDS("data/app_data.rds")
h  <- shape_arsenal(filter(ad, pitcher == 663687))
expect("Harris season: only the 4 SV are hidden", chart_hidden_types(h), "SV")
expect("Harris season: the 15 FC stay drawn", "FC" %in% chart_hidden_types(h), FALSE)
fills <- function(p) unique(ggplot2::layer_data(p, which(sapply(p$layers,
  function(l) inherits(l$geom, "GeomPoint"))))$fill)
expect("movement chart draws an SV dot without the rule",
       pitch_colors[["SV"]] %in% fills(plot_movement(h)), TRUE)
expect("and none with it", pitch_colors[["SV"]] %in% fills(plot_movement(h, hide = "SV")), FALSE)
expect("the note names it",
       chart_hidden_note(h, "SV"),
       "4 SV left off the charts, under 1% of pitches; still in the tables")
expect("nothing hidden, no note", chart_hidden_note(h, character()), NULL)

cat("\n=== heat maps ===\n")
# Columns are pitch types; two count panels each (Pre-2K, 2K). Harris's 4 SV
# were a whole column of panels of dots.
cols <- function(p) as.character(unique(ggplot2::ggplot_build(p)$layout$layout$pitch_type))
strip_labels <- function(p) {
  l <- p$layers[[which(sapply(p$layers, function(x) inherits(x$geom, "GeomText")))]]$data
  l <- l[l$pitch_type != "SV", ]
  setNames(l$strip, paste(l$situation, l$pitch_type))[order(paste(l$situation, l$pitch_type))]
}
full <- plot_heatmap(h, "All"); cut <- plot_heatmap(h, "All", hide = "SV")
expect("Harris heat map draws an SV column without the rule", "SV" %in% cols(full), TRUE)
expect("and five columns, no SV, with it", cols(cut), c("FF", "CU", "SL", "CH", "FC"))
expect("10 panels, not 12", nrow(ggplot2::ggplot_build(cut)$layout$layout), 10L)
expect("every remaining panel's usage label is unchanged, SV still divides",
       strip_labels(cut), strip_labels(full))
# Harris alone cannot fail that check: 4 pitches in ~1,200 never move a label
# rounded to a whole percent, and hiding SV before the strips passed it when
# mutated on 2026-09-23. This fixture can: 15 FF and 5 SV at 0-0 reads FF 75%
# with SV in the denominator and 100% without it.
set.seed(5)
hx <- data.frame(pitch_type = factor(c(rep("FF", 15), rep("SV", 5)), levels = c("FF", "SV")),
                 stand = "R", balls = 0L, strikes = 0L, in_zone = 1L,
                 plate_x = runif(20, -0.5, 0.5), plate_z = runif(20, 2, 3))
lab <- function(p) { l <- p$layers[[which(sapply(p$layers, function(x) inherits(x$geom, "GeomText")))]]$data
                     l$strip[l$pitch_type == "FF" & l$situation == "Pre-2K"] }
expect("fixture: FF at 0-0 reads 75% with SV hidden, not 100%",
       grepl("Usage 75%", lab(plot_heatmap(hx, "All", hide = "SV")), fixed = TRUE), TRUE)
# Harris threw no FC with two strikes. That panel used to render as a blank
# frame with no label, which reads as broken; it must say 0%.
hl <- cut$layers[[which(sapply(cut$layers, function(x) inherits(x$geom, "GeomText")))]]$data
expect("every panel carries a usage label", nrow(hl), 10L)
expect("Harris FC, 2K: labelled 0%",
       hl$strip[hl$pitch_type == "FC" & hl$situation == "2K"], "Usage 0%")
# Pre-2K and 2K partition the counts. The old three panels skipped 0-1 and 1-1,
# so a pitcher's 0-1 pitches were on no panel at all. Every located pitch must
# now land on exactly one panel.
pan <- ggplot2::ggplot_build(full)$data
pts <- h[!is.na(h$plate_x) & !is.na(h$plate_z), ]
cnt <- paste(pts$balls, pts$strikes, sep = "-")
expect("0-1 and 1-1 pitches are on the Pre-2K panel",
       all(c("0-1", "1-1") %in% cnt) && sum(cnt %in% c("0-1", "1-1")) > 0, TRUE)
hm_rows <- full$data
expect("every located pitch is on exactly one panel", nrow(hm_rows), nrow(pts))
expect("panels are Pre-2K then 2K", levels(hm_rows$situation), c("Pre-2K", "2K"))
# Dots paint over the zone outline, so a pitch on the edge shows whole. Pinned
# here, not in phase1_check: both heat map artifacts sit in its EXPECTED_DIFFS
# as whole-artifact sanctions, so it passed this reorder and would pass the
# revert. CLAUDE.md, checks that did not discriminate, entry 3.
geoms <- sapply(cut$layers, function(x) class(x$geom)[1])
expect("white dots are drawn after the zone outline",
       max(which(geoms == "GeomPoint")) > max(which(geoms == "GeomRect")), TRUE)
# The rule is measured on the whole window, so it holds whichever side is on.
for (hd in c("L", "R")) {
  expect(sprintf("vs %sHH: no SV column", hd),
         "SV" %in% cols(plot_heatmap(h, hd, hide = chart_hidden_types(h))), FALSE)
}


cat("\n=== the server ===\n")
res <- try_or(shiny::testServer(shiny::shinyAppDir("."), {
  session$setInputs(pitcher = "663687", dates = as.Date(c("2026-03-01", "2026-11-01")),
                    hand = "All")
  note <- paste(unlist(output$pitch_code_note$html), collapse = "")
  expect("Harris: the note above the tabs names the 4 SV",
         grepl("4 SV left off the charts", note, fixed = TRUE), TRUE)
  expect("Harris: the usage table still carries SV",
         grepl(">SV<", output$usage_table$html, fixed = TRUE), TRUE)
  session$setInputs(pitcher = "686930", dates = as.Date(c("2026-08-17", "2026-08-17")))
  expect("Barnett 08-17: nothing hidden on one outing", chart_hide(), character())
}))
expect("testServer ran to the end", !identical(res, "<error>"), TRUE)

cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 12 CHART RARE: ", if (!length(fails)) "PASS" else "FAIL", "\n", sep = "")
