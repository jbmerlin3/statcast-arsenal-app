# phase1_artifacts.R
#
# Worker for the Phase 1 regression. Run as a subprocess by
# scripts/phase1_check.R, once under the new R/ files and once under the
# original script's definitions, and writes a comparable artifact list.
#
#   Rscript tests/phase1_artifacts.R <old|new> <fixture.rds> <out.rds>
#
# Why a subprocess rather than a second environment in one session. The original
# definitions have to be evaluated somewhere, and any environment whose parent
# chain reaches the new functions will silently fall through to them if a line
# range is incomplete. Then the old side is partly the new side and every
# comparison passes for the wrong reason. A separate process makes that
# impossible, since the new functions are simply not present.

args     <- commandArgs(trailingOnly = TRUE)
mode     <- args[1]
fixture  <- args[2]
out_path <- args[3]

ORIGINAL <- Sys.getenv("PHASE1_ORIGINAL_SCRIPT")
REPO     <- Sys.getenv("PHASE1_REPO")

# Line ranges of the definitions in the original script. Everything outside
# these is top-level analysis, including the Savant scrape at L86 that must
# never fire from a test.
ORIGINAL_DEFS <- c(
  126:127,   # swing_only, whiff_desc
  139:146,   # pitch_colors, MIN_PITCH_COUNT, KDE_BW, KDE_MIN_N
  396:397,   # pitch_text_colors
  149:164,   # build_pitch_level
  182:208,   # plot_usage
  211:240,   # plot_movement
  244:255,   # plot_velo
  260:294,   # plot_heatmap
  301:334,   # FG_TO_SAVANT, load_fg_stuff
  336:358,   # arsenal_table
  360:394,   # arsenal_gt
  402:419,   # count_usage_tbl
  421:438    # count_usage_gt
)

suppressMessages({
  library(dplyr); library(tidyr); library(purrr); library(forcats)
  library(ggplot2); library(gt); library(readr); library(tibble)
})

if (mode == "new") {
  suppressMessages(invisible(lapply(sort(list.files(file.path(REPO, "R"), full.names = TRUE)), source)))
} else if (mode == "old") {
  src <- readLines(ORIGINAL, warn = FALSE)
  suppressMessages(eval(parse(text = src[ORIGINAL_DEFS]), envir = globalenv()))
} else {
  stop("mode must be 'old' or 'new'")
}

plt <- readRDS(fixture)

# Two stuff_all inputs, both synthetic, so the regression never reads FanGraphs.
# The second carries an fg_exact FALSE row specifically to fire arsenal_gt's
# footnote branch, which the empty frame leaves untested.
stuff_empty <- tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical())
stuff_mixed <- tibble(pitch_type = c("FF", "SL"),
                      stuff_plus = c(95.0, 102.7),
                      fg_exact   = c(TRUE, FALSE))

# gt stamps a random table id into rendered HTML, so two renders of the same
# object never compare equal. Compare the internal spec instead.
GT_PARTS <- c("_data", "_boxhead", "_styles", "_heading", "_source_notes", "_footnotes")
gt_spec  <- function(g) stats::setNames(lapply(GT_PARTS, function(p) g[[p]]), GT_PARTS)

# Built layers rather than the plot object. A plot object holds unevaluated
# quosures and environment references that differ harmlessly between sessions,
# while the built data is what actually reaches the page, so a changed constant
# or a dropped column shows up here.
plot_layers <- function(p) suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)$data))

artifacts <- list(
  arsenal_table_R    = as.data.frame(arsenal_table(plt, "R", stuff_empty)),
  arsenal_table_L    = as.data.frame(arsenal_table(plt, "L", stuff_empty)),
  count_usage_tbl_R  = as.data.frame(count_usage_tbl(plt, "R")),
  count_usage_tbl_L  = as.data.frame(count_usage_tbl(plt, "L")),

  plot_usage         = plot_layers(plot_usage(plt)),
  plot_movement      = plot_layers(plot_movement(plt)),
  plot_velo          = plot_layers(plot_velo(plt)),
  plot_heatmap_R     = plot_layers(plot_heatmap(plt, "R")),
  plot_heatmap_L     = plot_layers(plot_heatmap(plt, "L")),

  arsenal_gt_R       = gt_spec(arsenal_gt(arsenal_table(plt, "R", stuff_empty), "R")),
  arsenal_gt_L       = gt_spec(arsenal_gt(arsenal_table(plt, "L", stuff_empty), "L")),
  arsenal_gt_footnote= gt_spec(arsenal_gt(arsenal_table(plt, "R", stuff_mixed), "R")),
  count_usage_gt_R   = gt_spec(count_usage_gt(count_usage_tbl(plt, "R"), "R")),
  count_usage_gt_L   = gt_spec(count_usage_gt(count_usage_tbl(plt, "L"), "L"))
)

saveRDS(artifacts, out_path)
cat("wrote", length(artifacts), "artifacts for mode:", mode, "\n")
