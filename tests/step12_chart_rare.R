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
