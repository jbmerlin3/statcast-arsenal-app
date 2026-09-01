# step3_null_identical.R
#
#   Rscript tests/step3_null_identical.R          compare against the baseline
#   Rscript tests/step3_null_identical.R --write  recapture the baseline
#
# The ref = NULL invariant: the Characteristics tab renderers with no league
# context must render exactly what they rendered before Part B step 3 existed.
#
# This exists because scripts/phase1_check.R CANNOT test that. It compares the
# new code against the original script, and all three arsenal_gt artifacts sit
# in EXPECTED_DIFFS for the source-note change, so they report "differs
# (expected)" whatever happens inside the function. Breaking the xwOBA format
# passed phase1_check at six diffs. See species 3 in CLAUDE.md.
#
# So the comparison here is against a committed snapshot of TODAY, not against
# the original script. Baseline captured at 9694276, the commit before
# resolve_table() and the ref argument landed.
#
# RE-ANCHORED 2026-08-31 for the Characteristics tab split, and that word is not
# "recaptured" on purpose. Every earlier entry below recaptured the SAME artifact
# after a change to its contents. This one is different in kind: arsenal_gt() no
# longer exists, so the four old entries could not be compared to anything and
# were replaced by six, three per table. A baseline whose artifact is gone cannot
# fail, so re-anchoring here is mandatory rather than a judgment call -- but it
# also means this file protected NOTHING across that one commit, which is worth
# knowing when reading the history.
#
# What was checked before re-anchoring, since the byte comparison could not be:
# cell TEXT was extracted per column from the old baseline and from the two new
# renders, and the 14 pre-split columns were compared. All 14 identical across
# all three variants, nothing dropped, four columns added (vaa, ext, rel_ht,
# rel_side). So the split moved no value, which is the property that mattered:
# it was meant to be a re-layout and nothing else.
#
# RECAPTURED 2026-08-24 a second time, for Stuff+ shading. That shading sits
# BEFORE the ref early-return on purpose, so a table rendered with no league
# context still gets it, which means it lands in this baseline too. Diffed by
# column first: the only cells that moved were stuff_plus, and only in the
# fixture variant that supplies real grades. The ref = NULL variant, which has
# no grades at all, was unchanged at 0 cells.
#
# RECAPTURED 2026-08-24 for the row-wash removal. Verified by style rather than
# by text before rewriting it, since this change touches no value: all six
# pitch-colour row fills, rgba(...,0.13) across 84 body cells, disappeared; the
# six coloured pitch codes stayed; and every cell's text was byte-identical.
# A text-level diff sees nothing here, which is exactly why it was not the check.
#
# RECAPTURED 2026-08-23, twice: once for the foul_tip change to whiff_desc and
# again when bunt attempts joined the swing set. The second diff moved 15 cells,
# whiff_pct, csw_pct and chase_pct, one each per pitch type, nothing else.
# Recapturing
# is the move that can hide a regression, so it was done only after checking WHAT
# moved: 10 of 84 body cells on the R side, 5 whiff_pct and 5 csw_pct, one per
# pitch type, and no other column touched. The zone change landed in the same
# pass and does NOT appear here, because the fixture carries a frozen in_zone
# column computed before it. If this test ever fails again, diff the cells by
# column BEFORE reaching for --write.

# RECAPTURED 2026-08-31, same day, for the rel_side sign convention. The
# arm-side mirror was removed: rel_side is now negated only, so a righty reads
# positive and a lefty negative, where before both hands read positive toward
# the arm side. Diffed by column first, as the line above insists.
#
# Exactly one column moved, in all three traits variants, and only by sign:
# 1.56|1.19|1.22|1.07|1.26|1.33 -> -1.56|-1.19|... The magnitudes are untouched
# and both results variants were byte-identical. The fixture pitcher is a LHP,
# so every one of his rel_side values flips, which is the whole change.
#
# The FILLS do not move, and that is a property worth stating rather than a
# coincidence. Removing the mirror reverses the within-hand rank, p -> 100 - p,
# and the extreme direction folds to 2 * |p - 50|, which is invariant under that
# reversal. So the colouring is unchanged by construction and only the printed
# number differs. Checked separately against real league context, since this
# baseline is the ref = NULL render and carries no fills to compare.

# RECAPTURED 2026-08-31, third time that day, for the footnote removal and the
# results-table width. Diffed by column first, as always.
#
#   traits_R / traits_L / traits_window : source notes 2 -> 0, every cell text
#     byte-identical. Prose only.
#   traits_footnote : source notes 2 -> 0, footnotes 4 -> 0, and ONE column of
#     cell text moved, stuff_plus, which was carrying the sweeper footnote's
#     superscript marker. gt puts the marker in the cell, so removing the
#     footnote necessarily changes that cell. Expected and accounted for.
#   results_R / results_L : cell text byte-identical, column width 80px -> 107px.
#
# So no VALUE moved anywhere. What this baseline now protects is thinner than it
# was on the traits side: with every note gone, the fallback and no-reference
# states render exactly like an exact hit apart from font weight, and this file
# can no longer tell them apart. tests/step3_render.R carries that instead, and
# its pick() was made glyph-aware in the same commit so the dagger probe cannot
# silently land on a table that draws no daggers.

suppressMessages({library(dplyr); library(tidyr); library(purrr); library(forcats)
                  library(ggplot2); library(gt); library(readr)})

REPO <- local({
  a <- grep("^--file=", commandArgs(), value = TRUE)
  root <- if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else normalizePath(".")
  if (!dir.exists(file.path(root, "R"))) root <- normalizePath(".")
  root
})
invisible(lapply(sort(list.files(file.path(REPO, "R"), full.names = TRUE)), source))

BASELINE <- file.path(REPO, "tests/fixtures/chars_gt_null_baseline.rds")
FIXTURE  <- file.path(REPO, "tests/fixtures/pl_trim_702070.rds")
plt <- readRDS(FIXTURE)

# Same two synthetic stuff_all inputs phase1_artifacts.R uses, so the fg_exact
# footnote branch is exercised and no FanGraphs file is read.
stuff_empty <- tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical())
stuff_mixed <- tibble(pitch_type = c("FF", "SL"), stuff_plus = c(95.0, 102.7),
                      fg_exact = c(TRUE, FALSE))

# gt stamps one random table id, so strip it. Same treatment as gt_spec() in
# tests/phase1_artifacts.R, and it must stay the rendered page rather than the
# internal spec: the spec omits _formats, and the markers render through fmt().
render <- function(g) {
  h <- as.character(gt::as_raw_html(g))
  ids <- unique(regmatches(h, gregexpr('(?<=id=")[a-z]{10,}(?=")', h, perl = TRUE))[[1]])
  for (id in ids) h <- gsub(id, "GT_ID", h, fixed = TRUE)
  h
}

# Every call omits `ref` entirely rather than passing NULL, so the default is
# what gets tested. A caller that never heard of Part B is the case that must
# not move.
# Six entries: the four traits variants that inherit the old cases, plus the two
# results renders. `window` stays traits-only because fg_window is a Stuff+
# source note and results_gt() has no such argument.
current <- list(
  traits_R        = render(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_empty)), "R")),
  traits_L        = render(traits_gt(traits_tbl(arsenal_table(plt, "L", stuff_empty)), "L")),
  traits_footnote = render(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_mixed)), "R")),
  traits_window   = render(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_empty)), "R",
                                     fg_window = "2026-03-26 to 2026-08-17")),
  results_R       = render(results_gt(results_tbl(arsenal_table(plt, "R", stuff_empty)), "R")),
  results_L       = render(results_gt(results_tbl(arsenal_table(plt, "L", stuff_empty)), "L"))
)

# Explicit NULL as well as the default. They are two call shapes and only one of
# them is the default, so both are checked on BOTH renderers, and each must equal
# its own default render.
null_args <- list(
  traits_R  = render(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_empty)), "R", ref = NULL)),
  results_R = render(results_gt(results_tbl(arsenal_table(plt, "R", stuff_empty)), "R", ref = NULL))
)

if ("--write" %in% commandArgs(TRUE)) {
  saveRDS(current, BASELINE)
  cat("baseline written:", basename(BASELINE), "\n")
  for (nm in names(current)) cat(sprintf("  %-9s %6d chars\n", nm, nchar(current[[nm]])))
  quit(status = 0)
}

stopifnot("no baseline; run with --write at the commit you want to freeze" =
            file.exists(BASELINE))
base <- readRDS(BASELINE)

# A baseline missing a case cannot fail on it. Names must match exactly.
extra   <- setdiff(names(current), names(base))
missing <- setdiff(names(base), names(current))
moved   <- names(base)[vapply(names(base), function(nm)
             !identical(base[[nm]], current[[nm]]), logical(1))]

cat("\n", strrep("-", 58), "\n", sep = "")
for (nm in names(base)) {
  cat(sprintf("  %-16s %s\n", nm,
              if (identical(base[[nm]], current[[nm]])) "identical" else "MOVED"))
}
cat(strrep("-", 58), "\n", sep = "")

for (nm in moved) {
  b <- base[[nm]]; c2 <- current[[nm]]
  cat("\n", nm, ": ", nchar(b), " -> ", nchar(c2), " chars\n", sep = "")
  cb <- strsplit(b, "")[[1]]; cc <- strsplit(c2, "")[[1]]
  n <- min(length(cb), length(cc)); d <- which(cb[1:n] != cc[1:n])
  if (length(d)) {
    at <- d[1]
    cat("  first difference at char ", at, "\n", sep = "")
    cat("    baseline: ...", substr(b, max(1, at - 50), at + 50), "...\n", sep = "")
    cat("    current : ...", substr(c2, max(1, at - 50), at + 50), "...\n", sep = "")
  }
}
if (length(extra))   cat("\nbaseline has no entry for: ", paste(extra, collapse = ", "), "\n", sep = "")
if (length(missing)) cat("\ncurrent no longer renders: ", paste(missing, collapse = ", "), "\n", sep = "")

# Explicit ref = NULL must match the default path exactly. Skipped, loudly,
# only while the argument does not exist.
null_ok <- TRUE
for (nm in names(null_args)) {
  this_ok <- identical(null_args[[nm]], base[[nm]])
  null_ok <- null_ok && this_ok
  cat("  ", nm, " ref=NULL  ", if (this_ok) "identical to its default render" else "MOVED",
      "\n", sep = "")
}

ok <- !length(moved) && !length(extra) && !length(missing) && null_ok
cat("\nREF = NULL BYTE IDENTITY: ", if (ok) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (ok) 0 else 1)
