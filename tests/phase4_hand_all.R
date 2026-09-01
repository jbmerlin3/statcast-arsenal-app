# phase4_hand_all.R
#
#   Rscript tests/phase4_hand_all.R
#
# Property tests for hand = "All" on arsenal_table(), count_usage_tbl() and
# plot_heatmap().
#
# Why these are not part of the Phase 1 regression. That harness compares the
# new code against the original script, and the original has no "All" path to
# compare against. Generating the expected values from the new code would be a
# test that passes by construction and proves nothing. So "All" is checked
# against properties that must hold regardless of implementation, chiefly that
# pooling equals the sum of the parts.
#
# The "L" and "R" paths need nothing here. They are byte-identical after the
# widening, which is exactly what scripts/phase1_check.R already verifies.

suppressMessages({library(dplyr); library(ggplot2)})

REPO <- local({
  a <- grep("^--file=", commandArgs(), value = TRUE)
  root <- if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else normalizePath(".")
  if (!dir.exists(file.path(root, "R"))) root <- normalizePath(".")
  root
})
setwd(REPO)
suppressMessages(invisible(lapply(sort(list.files("R", full.names = TRUE)), source)))

MLB_ID <- 702070
d <- shape_arsenal(filter(load_app_data(), pitcher == MLB_ID))
empty_stuff <- tibble::tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical())

fails <- 0L
ok <- function(label, passed, detail = "") {
  cat(sprintf("  %-52s %s%s\n", label, if (isTRUE(passed)) "ok" else "FAIL",
              if (nzchar(detail)) paste0("   ", detail) else ""))
  if (!isTRUE(passed)) fails <<- fails + 1L
}

cat("== arsenal_table ==\n")
aR <- arsenal_table(d, "R", empty_stuff)
aL <- arsenal_table(d, "L", empty_stuff)
aA <- arsenal_table(d, "All", empty_stuff)

# Pooling must equal the sum of the parts. This is the check that catches a
# filter skipped in the wrong place, or applied twice.
parts <- bind_rows(aR["count"] |> mutate(pitch_type = aR$pitch_type),
                   aL["count"] |> mutate(pitch_type = aL$pitch_type)) |>
  group_by(pitch_type) |> summarise(count = sum(count), .groups = "drop")
joined <- aA |> select(pitch_type, all_count = count) |>
  left_join(parts, by = "pitch_type")
ok("All count == R count + L count, per pitch type",
   all(joined$all_count == joined$count),
   sprintf("%d pitch types", nrow(joined)))
ok("All total == the whole frame", sum(aA$count) == nrow(d),
   sprintf("%d vs %d", sum(aA$count), nrow(d)))
ok("pitch_pct sums to 100", abs(sum(aA$pitch_pct) - 100) < 0.15,
   sprintf("%.1f", sum(aA$pitch_pct)))

cat("\n== count_usage_tbl ==\n")
cR <- count_usage_tbl(d, "R"); cL <- count_usage_tbl(d, "L"); cA <- count_usage_tbl(d, "All")
ok("All Counts column sums to 100", abs(sum(cA[["All Counts"]]) - 100) < 0.15,
   sprintf("%.1f", sum(cA[["All Counts"]])))
ok("All covers every pitch type either side saw",
   all(union(as.character(cR$pitch_type), as.character(cL$pitch_type)) %in%
       as.character(cA$pitch_type)))

cat("\n== plot_heatmap ==\n")
dense_panels <- function(hand) {
  p <- suppressWarnings(ggplot_build(plot_heatmap(d, hand)))
  # Layer 1 is stat_density_2d_filled, fitted only to panels at or above
  # KDE_MIN_N, so counting its distinct panels counts the dense ones.
  length(unique(p$data[[1]]$PANEL))
}
hR <- dense_panels("R"); hL <- dense_panels("L"); hA <- dense_panels("All")
ok("All renders", is.numeric(hA) && hA > 0)
ok("All dense panels >= max(R, L)", hA >= max(hR, hL),
   sprintf("R %d, L %d, All %d", hR, hL, hA))

cat("\n== hand_label ==\n")
ok("R   -> vs RHH",        identical(hand_label("R"),   "vs RHH"))
ok("L   -> vs LHH",        identical(hand_label("L"),   "vs LHH"))
ok("All -> vs All Batters", identical(hand_label("All"), "vs All Batters"))
ok("unknown value stops rather than mislabelling",
   inherits(tryCatch(hand_label("B"), error = function(e) e), "error"))

cat("\n== titles ==\n")
title_of <- function(g) g[["_heading"]]$title
# Titles changed on 2026-08-31 with the table split. What these assertions are
# actually guarding is hand_label() reaching the heading at all, and that the
# "All" case does not silently fall through to "vs LHH", which is the bug they
# were written for. Both survive the rename.
ok("traits_gt R title",
   identical(title_of(traits_gt(traits_tbl(aR), "R")), "PITCH TRAITS (vs RHH)"))
ok("traits_gt L title",
   identical(title_of(traits_gt(traits_tbl(aL), "L")), "PITCH TRAITS (vs LHH)"))
ok("traits_gt All title is not 'vs LHH'",
   identical(title_of(traits_gt(traits_tbl(aA), "All")), "PITCH TRAITS (vs All Batters)"))
ok("results_gt R title",
   identical(title_of(results_gt(results_tbl(aR), "R")), "PITCH RESULTS (vs RHH)"))
ok("results_gt All title is not 'vs LHH'",
   identical(title_of(results_gt(results_tbl(aA), "All")), "PITCH RESULTS (vs All Batters)"))
ok("count_usage_gt All title",
   identical(title_of(count_usage_gt(cA, "All")), "USAGE BY COUNT vs All Batters"))
ok("label argument overrides, for Phase 4 date ranges",
   identical(title_of(traits_gt(traits_tbl(aR), "R", label = "2nd Half, vs RHH")),
             "PITCH TRAITS (2nd Half, vs RHH)"))
ok("label argument overrides on results too",
   identical(title_of(results_gt(results_tbl(aR), "R", label = "2nd Half, vs RHH")),
             "PITCH RESULTS (2nd Half, vs RHH)"))

cat(sprintf("\nPHASE 4 HAND=ALL: %s\n", if (fails == 0L) "PASS" else paste(fails, "FAILED")))
quit(status = if (fails == 0L) 0 else 1)
