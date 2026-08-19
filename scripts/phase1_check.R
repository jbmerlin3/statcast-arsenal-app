# phase1_check.R
#
# Phase 1 acceptance. Proves the five-file split changed no behaviour, by
# running the original definitions and the new R/ files over one frozen input
# and comparing what each produces.
#
#   Rscript scripts/phase1_check.R
#
# This is the only file holding a machine-specific path. Nothing under R/ knows
# where the data lives, which is what keeps the library layer portable when
# Phase 6 adds renv.

# Resolve the repo root from this script's own path, so the check runs from any
# working directory. Falls back to the cwd when sourced interactively rather
# than run through Rscript.
REPO <- local({
  a <- grep("^--file=", commandArgs(), value = TRUE)
  root <- if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else normalizePath(".")
  if (!dir.exists(file.path(root, "R"))) root <- normalizePath(".")
  root
})
stopifnot("cannot locate the repo root, run from the project directory" =
            dir.exists(file.path(REPO, "R")))

STATCAST_RDS <- path.expand("~/Desktop/Baseball Questionnaires/03_ArsenalReports/statcast_clean_2026.rds")
ORIGINAL     <- path.expand("~/Desktop/Baseball Questionnaires/05_PlayerEval/scripts/Learning to pull stats.R")
MLB_ID       <- 702070   # Cameron, the worked example in the original script

FIXTURE  <- file.path(REPO, "tests/fixtures/pl_trim_702070.rds")
WORKER   <- file.path(REPO, "tests/phase1_artifacts.R")

# Coverage limit, verified by mutation rather than assumed. Injecting a wrong
# KDE_BW, a wrong KDE_MIN_N of 40, or reverting the movement seed all make this
# check fail, so it does discriminate. But nudging KDE_MIN_N from 15 to 14
# passes, because no heatmap panel in this one fixture has exactly 14 pitches.
# One pitcher cannot sit on every threshold. A second fixture with different
# panel sizes would close that gap if a threshold bug ever slips through.
#
# Artifacts allowed to differ, and why. The check fails on any unexpected
# difference AND on any expected difference that has gone missing, since a
# vanished divergence means an intended change was reverted.
EXPECTED_DIFFS <- c(
  plot_movement       = "set.seed(3) is a deliberate change from the original's 42, cosmetic subsample only",
  arsenal_gt_R        = "source note now reports the FanGraphs export window",
  arsenal_gt_L        = "source note now reports the FanGraphs export window",
  arsenal_gt_footnote = "source note now reports the FanGraphs export window"
)


# ---- Fixture -----------------------------------------------------------------

# Both sides must see byte-identical input, or a difference in the input masks
# or manufactures a difference in the output.
if (!file.exists(FIXTURE)) {
  message("Building fixture from ", basename(STATCAST_RDS), " ...")
  suppressMessages({library(dplyr); library(tidyr); library(purrr); library(forcats); library(ggplot2)})
  suppressMessages(invisible(lapply(sort(list.files(file.path(REPO, "R"), full.names = TRUE)), source)))
  sc  <- readRDS(STATCAST_RDS)
  new_fix <- trim_pitch_level(build_pitch_level(sc, MLB_ID))

  # The fixture is built by the new code, so features.R needs its own check or a
  # bug there would be invisible to every comparison below.
  src <- readLines(ORIGINAL, warn = FALSE)
  e <- new.env(parent = globalenv())
  suppressMessages(eval(parse(text = src[c(126:127, 139:146, 149:164)]), envir = e))
  old_fix <- eval(parse(text = paste(c("pl |>", src[169:178]), collapse = "\n")),
                  envir = list2env(list(pl = e$build_pitch_level(sc, MLB_ID)), parent = globalenv()))
  stopifnot("fixture disagrees with the original build and trim" =
              isTRUE(all.equal(as.data.frame(new_fix), as.data.frame(old_fix))))
  message("  fixture matches the original build and trim")

  dir.create(dirname(FIXTURE), recursive = TRUE, showWarnings = FALSE)
  saveRDS(new_fix, FIXTURE)
  rm(sc)
}
plt <- readRDS(FIXTURE)
message("Fixture: ", nrow(plt), " pitches, ", ncol(plt), " columns\n")


# ---- Run both sides in clean subprocesses ------------------------------------

# Set these in this process and let the subprocess inherit them. system2's own
# `env` argument builds a shell prefix without quoting, which breaks on the
# original script's path, since "Learning to pull stats.R" contains spaces.
Sys.setenv(PHASE1_ORIGINAL_SCRIPT = ORIGINAL, PHASE1_REPO = REPO)

run_side <- function(mode) {
  out <- tempfile(fileext = ".rds")
  st <- system2("Rscript", c(shQuote(WORKER), mode, shQuote(FIXTURE), shQuote(out)),
                stdout = TRUE, stderr = TRUE)
  if (!file.exists(out)) stop("worker failed for mode '", mode, "':\n", paste(st, collapse = "\n"))
  readRDS(out)
}

message("Running original definitions ..."); old <- run_side("old")
message("Running R/ files ...");             new <- run_side("new")


# ---- Compare -----------------------------------------------------------------

stopifnot(identical(names(old), names(new)))
diffs <- vapply(names(new), function(nm) !isTRUE(all.equal(new[[nm]], old[[nm]])), logical(1))

cat("\n", strrep("-", 66), "\n", sep = "")
for (nm in names(diffs)) {
  cat(sprintf("  %-22s %s\n", nm,
              if (!diffs[[nm]]) "identical"
              else if (nm %in% names(EXPECTED_DIFFS)) "differs (expected)"
              else "DIFFERS (UNEXPECTED)"))
}
cat(strrep("-", 66), "\n\n", sep = "")

unexpected <- setdiff(names(diffs)[diffs], names(EXPECTED_DIFFS))
missing    <- setdiff(names(EXPECTED_DIFFS), names(diffs)[diffs])

if (length(unexpected)) {
  cat("Unexpected differences, the split changed behaviour:\n")
  for (nm in unexpected) {
    cat("  ", nm, "\n")
    cat(paste0("      ", utils::head(all.equal(new[[nm]], old[[nm]]), 5)), sep = "\n")
  }
  cat("\n")
}
if (length(missing)) {
  cat("Expected differences that did not occur, an intended change was reverted:\n")
  for (nm in missing) cat("  ", nm, ": ", EXPECTED_DIFFS[[nm]], "\n", sep = "")
  cat("\n")
}
if (length(unexpected) == 0 && length(missing) == 0) {
  cat("Accounted-for differences:\n")
  for (nm in names(EXPECTED_DIFFS)) cat("  ", nm, ": ", EXPECTED_DIFFS[[nm]], "\n", sep = "")
  cat("\n")
}

cat("PHASE 1 REGRESSION: ",
    if (length(unexpected) == 0 && length(missing) == 0) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (length(unexpected) == 0 && length(missing) == 0) 0 else 1)
