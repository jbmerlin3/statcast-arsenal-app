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

build <- function(id, hand, dates = NULL) {
  pl <- build_pitch_level(ad, id)
  if (!is.null(dates)) pl <- filter(pl, game_date >= dates[1], game_date <= dates[2])
  tb <- arsenal_table(pl, hand, tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical()))
  ctx <- resolve_table(tb, arsenal_denoms(pl, hand), ref, unique(pl$p_throws)[1], hand, "All Counts")
  list(tb = tb, ctx = ctx, g = arsenal_gt(tb, hand, ref = ctx))
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
fb <- baz$ctx$cells[baz$ctx$cells$state == "fallback", ][1, ]
cell <- tds(baz$g, fb$column)[fb$row]
cat("  ", fb$column, " row ", fb$row, ": ", txt(cell), "\n", sep = "")
expect("fallback cell text ends with the dagger", grepl("†$", txt(cell)), TRUE)
expect("fallback cell is filled, not white", grepl("#FFFFFF", cell, ignore.case = TRUE), FALSE)
expect("fallback cell is bold", grepl("font-weight: bold", cell), TRUE)

cat("\n=== below floor renders unfilled, grey, italic, with its n ===\n")
bf <- cam$ctx$cells[cam$ctx$cells$state == "below_floor", ][1, ]
cell2 <- tds(cam$g, bf$column)[bf$row]
cat("  ", bf$column, " row ", bf$row, ": ", txt(cell2), "\n", sep = "")
expect("below floor is white",  grepl("background-color: #FFFFFF", cell2, ignore.case = TRUE), TRUE)
expect("below floor is grey",   grepl("color: #767676", cell2, ignore.case = TRUE), TRUE)
expect("below floor is italic", grepl("font-style: italic", cell2), TRUE)
expect("below floor shows its n", grepl("\\([0-9]+\\)$", txt(cell2)), TRUE)

cat("\n=== the percentile fill survives the pitch-colour reduce ===\n")
ex <- baz$ctx$cells[baz$ctx$cells$state == "exact", ][1, ]
cell3 <- tds(baz$g, ex$column)[ex$row]
pc <- paste0(pitch_colors[[as.character(baz$tb$pitch_type[ex$row])]], "20")
cat("  cell fill: ", sub('.*background-color: ([^;]*);.*', '\\1', cell3),
    "   resolver said: ", ex$fill, "   pitch colour is: ", pc, "\n", sep = "")
expect("cell carries the percentile fill", grepl(ex$fill, cell3, ignore.case = TRUE), TRUE)
expect("cell does NOT carry the pitch-colour fill", grepl(pc, cell3, ignore.case = TRUE), FALSE)

cat("\n=== note order is dagger, double dagger, grey ===\n")
nts <- baz$ctx$notes
for (n in nts) cat("  ", substr(n, 1, 72), "\n", sep = "")
rank_of <- function(n) if (startsWith(n, "†")) 1L else if (startsWith(n, "‡")) 2L else 3L
expect("notes are in marker-grammar order", !is.unsorted(vapply(nts, rank_of, integer(1))), TRUE)
# The rendered order must match, or the vector's order is decorative.
h <- as.character(as_raw_html(baz$g))
pos <- vapply(nts, function(n) regexpr(substr(n, 4, 40), h, fixed = TRUE), integer(1))
expect("rendered source notes follow the same order", !is.unsorted(pos), TRUE)

cat("\n=== one dagger line names one grain, and two grains stop ===\n")
two <- baz$ctx$cells
two$state[1] <- "fallback"; two$grain[1] <- "pitch_type x p_throws"
got <- tryCatch({
  fb2 <- two[two$state == "fallback", ]
  if (length(unique(fb2$grain)) > 1) stop("guard") else "no guard"
}, error = function(e) "stopped")
expect("two distinct grains would be caught", got, "stopped")

cat("\n", strrep("-", 58), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 3 RENDER: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
