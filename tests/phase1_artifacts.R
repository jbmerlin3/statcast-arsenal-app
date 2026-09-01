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
  360:394,   # traits_gt + results_gt (one arsenal_gt in the original)
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
# The second carries an fg_exact FALSE row specifically to fire traits_gt's
# footnote branch, which the empty frame leaves untested.
stuff_empty <- tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical())
stuff_mixed <- tibble(pitch_type = c("FF", "SL"),
                      stuff_plus = c(95.0, 102.7),
                      fg_exact   = c(TRUE, FALSE))

# gt stamps a random table id into rendered HTML, so two renders of the same
# object never compare equal. Compare the internal spec instead.
# gt stamps one random table id into rendered HTML, used as id="..." and in
# every CSS selector, so two renders of the same object never compare equal.
# Strip that one token and the rendered page is deterministic, verified by
# rendering the same spec twice.
#
# The rendered page rather than the internal spec, because the spec omits
# _formats: two tables differing only in fmt() compared identical, and
# arsenal_gt() renders its percentile markers through fmt(). Same species as
# the layers-without-theme gap below. See the check list in CLAUDE.md.
#
# _formats cannot simply be added to a parts list: it holds format closures
# whose environments do not survive the comparison ("object pitch_type not
# found"). The rendered page sidesteps that and is a stronger artifact anyway,
# since it is what actually reaches the reader.
gt_spec <- function(g) {
  h <- as.character(gt::as_raw_html(g))
  ids <- unique(regmatches(h, gregexpr('(?<=id=")[a-z]{10,}(?=")', h, perl = TRUE))[[1]])
  for (id in ids) h <- gsub(id, "GT_ID", h, fixed = TRUE)
  h
}

# Built layers plus the theme. The layers are what reaches the page as geometry
# and aesthetics; the theme is everything else about how it looks. Comparing
# only the layers made every purely thematic edit invisible: margins, fonts,
# panel colours, legend position all passed silently, which is how a heatmap
# margin fix could change the plot and still report "identical".
#
# Still not the whole plot object, which carries quosures and environment
# references that differ harmlessly between two sessions.
plot_spec <- function(p) {
  b <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))
  list(data = b$data, theme = b$plot$theme)
}

artifacts <- list(
  arsenal_table_R    = as.data.frame(arsenal_table(plt, "R", stuff_empty)),
  arsenal_table_L    = as.data.frame(arsenal_table(plt, "L", stuff_empty)),
  count_usage_tbl_R  = as.data.frame(count_usage_tbl(plt, "R")),
  count_usage_tbl_L  = as.data.frame(count_usage_tbl(plt, "L")),

  plot_usage         = plot_spec(plot_usage(plt)),
  plot_movement      = plot_spec(plot_movement(plt)),
  plot_velo          = plot_spec(plot_velo(plt)),
  plot_heatmap_R     = plot_spec(plot_heatmap(plt, "R")),
  plot_heatmap_L     = plot_spec(plot_heatmap(plt, "L")),

  count_usage_gt_R   = gt_spec(count_usage_gt(count_usage_tbl(plt, "R"), "R")),
  count_usage_gt_L   = gt_spec(count_usage_gt(count_usage_tbl(plt, "L"), "L"))
)

# The Characteristics tab renders differ IN SHAPE between the two sides as of
# 2026-08-31, so they are emitted under mode-specific names rather than forced
# into one comparison. The original script has a single arsenal_gt(); the new
# code has two renderers and no function of that name at all. Renaming one of
# the new tables to the old key would have made the harness compare a 12-column
# traits table against a 14-column characteristics table and report a difference
# that means nothing, and putting both under one key would have hidden it.
#
# phase1_check.R knows both name sets and asserts each appears on exactly one
# side, so this cannot decay into a silent drop. Byte-level coverage for the two
# new renders lives in tests/step3_null_identical.R instead.
if (mode == "old") {
  artifacts$chars_gt_R        <- gt_spec(arsenal_gt(arsenal_table(plt, "R", stuff_empty), "R"))
  artifacts$chars_gt_L        <- gt_spec(arsenal_gt(arsenal_table(plt, "L", stuff_empty), "L"))
  artifacts$chars_gt_footnote <- gt_spec(arsenal_gt(arsenal_table(plt, "R", stuff_mixed), "R"))
} else {
  artifacts$traits_gt_R        <- gt_spec(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_empty)), "R"))
  artifacts$traits_gt_L        <- gt_spec(traits_gt(traits_tbl(arsenal_table(plt, "L", stuff_empty)), "L"))
  artifacts$traits_gt_footnote <- gt_spec(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_mixed)), "R"))
  artifacts$results_gt_R       <- gt_spec(results_gt(results_tbl(arsenal_table(plt, "R", stuff_empty)), "R"))
  artifacts$results_gt_L       <- gt_spec(results_gt(results_tbl(arsenal_table(plt, "L", stuff_empty)), "L"))
}

saveRDS(artifacts, out_path)
cat("wrote", length(artifacts), "artifacts for mode:", mode, "\n")
