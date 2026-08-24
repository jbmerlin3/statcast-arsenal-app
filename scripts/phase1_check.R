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

STATCAST_RDS <- path.expand("~/baseball-store/statcast_clean_2026.rds")
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
# Sanctioned at COLUMN granularity, which is strictly tighter than a whole
# artifact entry: everything else in the frame is still compared byte for byte,
# and each named column is asserted to ACTUALLY differ, so reverting the change
# fails the check instead of quietly satisfying it. That is the one limitation
# recorded below, closed for these two.
EXPECTED_COL_DIFFS <- list(
  arsenal_table_R = c(
                      whiff_pct = "foul_tip and missed_bunt are whiffs, bunt attempts are swings. Savant, 0.032 MAE over 105 pitchers",
                      csw_pct   = "same numerator: whiff_desc is the whiff half of CSW%",
                      chase_pct = "same swing set: a bunt attempt out of the zone is a chase. Savant, 0.060 MAE",
                      hb        = "arm-side normalised, so positive is arm side for both hands. The fixture pitcher is a LHP, so every hb flips sign against the original"),
  arsenal_table_L = c(
                      whiff_pct = "foul_tip and missed_bunt are whiffs, bunt attempts are swings. Savant, 0.032 MAE over 105 pitchers",
                      csw_pct   = "same numerator: whiff_desc is the whiff half of CSW%",
                      chase_pct = "same swing set: a bunt attempt out of the zone is a chase. Savant, 0.060 MAE",
                      hb        = "arm-side normalised, so positive is arm side for both hands. The fixture pitcher is a LHP, so every hb flips sign against the original")
)

EXPECTED_DIFFS <- c(
  plot_movement       = "set.seed(3) is a deliberate change from the original's 42, cosmetic subsample only",
  arsenal_gt_R        = "source note now reports the FanGraphs export window",
  arsenal_gt_L        = "source note now reports the FanGraphs export window",
  arsenal_gt_footnote = "source note now reports the FanGraphs export window",
  plot_heatmap_R      = "deliberate presentation change: strip annotation 3.2 to 4.2, plus plot.margin and strip.text margin so the column labels stop clipping",
  plot_heatmap_L      = "deliberate presentation change: strip annotation 3.2 to 4.2, plus plot.margin and strip.text margin so the column labels stop clipping"
)

# Plots are compared on built layer data AND on the theme, see plot_spec() in
# tests/phase1_artifacts.R. Layer data alone used to leave every purely thematic
# edit invisible: the heatmap margin fix changed the plot and still reported
# "identical". Adding the theme closed that, verified by mutation rather than
# assumed:
#
#   revert the heatmap annotation size, keep the margins -> still differs
#     (before the theme was compared, this reported identical and passed)
#   flip plot_velo's legend.position, an otherwise untouched plot
#     -> DIFFERS (UNEXPECTED), check FAILS
#
# Adding the theme produced no new differences on plots nobody had edited, so
# nothing was added to EXPECTED_DIFFS to accommodate it.
#
# One limit that remains. EXPECTED_DIFFS is keyed by artifact name, so it
# records THAT an artifact may differ, not WHICH difference is sanctioned. An
# entry stays satisfied if the sanctioned change is reverted while some other
# change takes its place.


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
  # in_zone is a DELIBERATE divergence from the original as of 2026-08-23: the
  # vertical bound now carries the ball radius the horizontal one always had.
  # Everything else must still match, and the divergence is checked for its
  # direction rather than waved through, since widening a zone can only turn
  # balls into strikes and never the reverse.
  #
  # pitch_team is a deliberate ADDITION as of 2026-08-23, for the search tab's
  # team filter. The original script has no such column, so it is excluded by
  # name rather than by taking whatever intersection happens to exist: an
  # intersection would also swallow a column the split dropped by accident,
  # which is the failure this check exists to catch.
  ADDED <- "pitch_team"
  nz <- as.data.frame(new_fix); oz <- as.data.frame(old_fix)
  stopifnot("the fixture grew a column that is not sanctioned here" =
              setequal(setdiff(names(nz), names(oz)), ADDED))
  stopifnot("the fixture LOST a column, which is never sanctioned" =
              length(setdiff(names(oz), names(nz))) == 0)
  stopifnot("every pitch resolves to a team" = !anyNA(nz$pitch_team))
  drop <- c("in_zone", ADDED)
  stopifnot("fixture disagrees with the original build and trim, outside in_zone" =
              isTRUE(all.equal(nz[, setdiff(names(nz), drop)],
                               oz[, setdiff(names(oz), drop)])))
  stopifnot("the new zone must contain the old one" = all(nz$in_zone >= oz$in_zone))
  stopifnot("the new zone must be strictly wider on this fixture" = sum(nz$in_zone) > sum(oz$in_zone))
  message("  fixture matches the original build and trim, in_zone widened by ",
          sum(nz$in_zone) - sum(oz$in_zone), " pitches as intended")

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

# Columns sanctioned above are lifted out before the frames are compared, and
# checked separately: each one MUST differ. A sanction that no longer describes
# a real change is a sanction that has to go, so it fails loudly rather than
# sitting there covering something else.
col_failures <- character()
drop_sanctioned <- function(nm, x) {
  cols <- names(EXPECTED_COL_DIFFS[[nm]])
  if (is.null(cols)) return(x)
  x[, setdiff(names(x), cols), drop = FALSE]
}
for (nm in names(EXPECTED_COL_DIFFS)) {
  for (col in names(EXPECTED_COL_DIFFS[[nm]])) {
    if (isTRUE(all.equal(new[[nm]][[col]], old[[nm]][[col]])))
      col_failures <- c(col_failures, sprintf(
        "%s$%s is sanctioned as changed but matches the original. Reverted?", nm, col))
  }
}

diffs <- vapply(names(new), function(nm)
  !isTRUE(all.equal(drop_sanctioned(nm, new[[nm]]), drop_sanctioned(nm, old[[nm]]))), logical(1))

cat("\n", strrep("-", 66), "\n", sep = "")
for (nm in names(diffs)) {
  # "identical" on an artifact with column sanctions would overclaim: the
  # sanctioned columns were lifted out before the comparison.
  sanctioned <- names(EXPECTED_COL_DIFFS[[nm]])
  cat(sprintf("  %-22s %s\n", nm,
              if (!diffs[[nm]] && !is.null(sanctioned))
                paste0("identical outside ", paste(sanctioned, collapse = ", "))
              else if (!diffs[[nm]]) "identical"
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
    cat(paste0("      ", utils::head(all.equal(drop_sanctioned(nm, new[[nm]]),
                                                drop_sanctioned(nm, old[[nm]])), 5)), sep = "\n")
  }
  cat("\n")
}
if (length(missing)) {
  cat("Expected differences that did not occur, an intended change was reverted:\n")
  for (nm in missing) cat("  ", nm, ": ", EXPECTED_DIFFS[[nm]], "\n", sep = "")
  cat("\n")
}
if (length(col_failures)) {
  cat("Column sanctions that no longer describe a real change:\n")
  for (f in col_failures) cat("  ", f, "\n", sep = "")
  cat("\n")
}
if (length(unexpected) == 0 && length(missing) == 0 && length(col_failures) == 0) {
  cat("Accounted-for differences:\n")
  for (nm in names(EXPECTED_DIFFS)) cat("  ", nm, ": ", EXPECTED_DIFFS[[nm]], "\n", sep = "")
  for (nm in names(EXPECTED_COL_DIFFS))
    for (col in names(EXPECTED_COL_DIFFS[[nm]]))
      cat("  ", nm, "$", col, ": ", EXPECTED_COL_DIFFS[[nm]][[col]], "\n", sep = "")
  cat("\n")
}

ok <- length(unexpected) == 0 && length(missing) == 0 && length(col_failures) == 0
cat("PHASE 1 REGRESSION: ", if (ok) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (ok) 0 else 1)
