# step3_null_identical.R
#
#   Rscript tests/step3_null_identical.R          compare against the baseline
#   Rscript tests/step3_null_identical.R --write  recapture the baseline
#
# The ref = NULL invariant: arsenal_gt() with no league context must render
# exactly what it rendered before Part B step 3 existed.
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
# RECAPTURED 2026-08-23, once, for the foul_tip change to whiff_desc. Recapturing
# is the move that can hide a regression, so it was done only after checking WHAT
# moved: 10 of 84 body cells on the R side, 5 whiff_pct and 5 csw_pct, one per
# pitch type, and no other column touched. The zone change landed in the same
# pass and does NOT appear here, because the fixture carries a frozen in_zone
# column computed before it. If this test ever fails again, diff the cells by
# column BEFORE reaching for --write.

suppressMessages({library(dplyr); library(tidyr); library(purrr); library(forcats)
                  library(ggplot2); library(gt); library(readr)})

REPO <- local({
  a <- grep("^--file=", commandArgs(), value = TRUE)
  root <- if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else normalizePath(".")
  if (!dir.exists(file.path(root, "R"))) root <- normalizePath(".")
  root
})
invisible(lapply(sort(list.files(file.path(REPO, "R"), full.names = TRUE)), source))

BASELINE <- file.path(REPO, "tests/fixtures/arsenal_gt_null_baseline.rds")
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
current <- list(
  R        = render(arsenal_gt(arsenal_table(plt, "R", stuff_empty), "R")),
  L        = render(arsenal_gt(arsenal_table(plt, "L", stuff_empty), "L")),
  footnote = render(arsenal_gt(arsenal_table(plt, "R", stuff_mixed), "R")),
  window   = render(arsenal_gt(arsenal_table(plt, "R", stuff_empty), "R",
                               fg_window = "2026-03-26 to 2026-08-17"))
  # No explicit ref = NULL here: the baseline is captured at a commit where the
  # argument does not exist yet. It is checked separately below, against this
  # same R entry, once the argument is there.
)

# Explicit NULL as well as the default. They are two call shapes and only one of
# them is the default, so both are checked, and both must equal today's R render.
has_ref_arg <- "ref" %in% names(formals(arsenal_gt))
null_arg <- if (has_ref_arg)
  render(arsenal_gt(arsenal_table(plt, "R", stuff_empty), "R", ref = NULL)) else NULL

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
  cat(sprintf("  arsenal_gt %-9s %s\n", nm,
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
if (has_ref_arg) {
  null_ok <- identical(null_arg, base$R)
  cat("  arsenal_gt ref=NULL   ", if (null_ok) "identical to today's R render" else "MOVED", "\n", sep = "")
} else {
  cat("  arsenal_gt ref=NULL   not applicable, no `ref` argument yet\n")
}

ok <- !length(moved) && !length(extra) && !length(missing) && null_ok
cat("\nREF = NULL BYTE IDENTITY: ", if (ok) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (ok) 0 else 1)
