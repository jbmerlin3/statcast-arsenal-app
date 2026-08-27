# reconcile.R
#
#   Rscript scripts/reconcile.R [n_dates]
#
# Hunts the class of bug a human found by hand on 2026-08-27: numbers that are
# internally consistent and externally wrong. Both cases that morning were
# invisible to every test in this repo, because every test compared the app to
# itself.
#
#   Fuentes 2026-08-05: app said 100% four-seam, Savant said 68/16/16. A display
#   floor was deleting the rows of any pitch type under five in the window.
#   Elder  2026-08-05: totals matched exactly, the MIX did not, because Savant
#   had reclassified six pitches after ingest and the dedup kept the older row.
#
# So this compares against UPSTREAM, at the grain a scout actually selects:
#
#   A. per pitcher-game pitch mix, against a fresh Savant pull for that date
#   B. per pitcher-game TBF/SO/BB/H/HR, against the MLB StatsAPI game log
#   C. internal invariants that must hold whatever the sources say
#
# Sampling is by DATE, not by pitcher-game, because Savant is one request per
# date and hundreds of pitcher-games ride along with it.
suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(forcats)
  library(ggplot2); library(gt); library(readr)
})
suppressPackageStartupMessages(library(jsonlite))
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

args    <- commandArgs(trailingOnly = TRUE)
N_DATES <- if (length(args)) as.integer(args[1]) else 6

# WHICH data this checks, and why it matters.
#
# The first run of this harness reported 38 misclassified pitches on 2026-08-20
# that were not actually wrong in the live app. It had read data/app_data.rds on
# the laptop, while the deployed app is built in CI from the release asset. Two
# stores, two writers, and they had diverged: CI applied 81 reclassifications at
# 13:38 that the laptop copy never saw.
#
# That divergence has since been closed by retiring the laptop chain agent, so
# CI is the only writer. This banner stays because a harness that checks the
# wrong artifact is worse than no harness: it manufactures findings and buries
# real ones.
cat("checking LOCAL data/app_data.rds. The deployed app is built in CI from the\n",
    "release asset; run the chain locally first if these have drifted.\n\n", sep = "")

ad <- readRDS("data/app_data.rds")
gl <- tryCatch(readRDS("data/game_logs.rds"), error = function(e) NULL)

set.seed(as.integer(format(Sys.Date(), "%Y%m%d")))
dates <- sort(sample(unique(ad$game_date), min(N_DATES, length(unique(ad$game_date)))))
cat("reconciling", length(dates), "dates:", paste(dates, collapse = ", "), "\n\n")

findings <- list()
note <- function(check, detail) {
  findings[[length(findings) + 1]] <<- data.frame(check = check, detail = detail,
                                                  stringsAsFactors = FALSE)
}

savant_day <- function(d) {
  u <- sprintf(paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true",
                      "&hfSea=%s%%7C&player_type=pitcher&game_date_gt=%s&game_date_lt=%s",
                      "&min_pitches=0&min_results=0&type=details"),
               substr(d, 1, 4), d, d)
  tryCatch(read.csv(u, stringsAsFactors = FALSE), error = function(e) NULL)
}

# ---- A. pitch mix against Savant --------------------------------------------
cat("== A. pitch mix vs Savant ==\n")
for (d in dates) {
  fresh <- savant_day(d)
  if (is.null(fresh) || !nrow(fresh)) { note("savant_unreachable", d); next }
  fresh <- fresh |> filter(game_type == "R", pitch_type != "",
                           !is.na(release_speed), !is.na(pfx_x), !is.na(pfx_z))
  ours <- ad |> filter(game_date == d)

  f <- fresh |> count(pitcher, pitch_type, name = "savant")
  o <- ours  |> count(pitcher, pitch_type, name = "ours")
  j <- full_join(f, o, by = c("pitcher", "pitch_type")) |>
    mutate(savant = coalesce(savant, 0L), ours = coalesce(ours, 0L)) |>
    filter(savant != ours)
  if (nrow(j)) {
    per <- j |> group_by(pitcher) |> summarise(n = sum(abs(savant - ours)), .groups = "drop")
    note("pitch_mix_mismatch",
         sprintf("%s: %d pitchers, %d pitches misclassified or missing",
                 d, nrow(per), sum(per$n)))
  }
  cat(sprintf("  %s  ours %5d  savant %5d  mismatched cells %3d\n",
              d, nrow(ours), nrow(fresh), nrow(j)))
}

# ---- B. counting stats against the MLB game log ------------------------------
cat("\n== B. TBF / SO / BB vs MLB game log ==\n")
if (is.null(gl)) {
  note("game_logs_missing", "data/game_logs.rds absent, section B skipped")
} else {
  for (d in dates) {
    ours <- ad |> filter(game_date == d)
    sc <- ours |> filter(is_bf(events)) |>
      group_by(pitcher) |>
      summarise(tbf = n(),
                so  = sum(events == "strikeout"),
                bb  = sum(events == "walk"), .groups = "drop")
    lg <- gl |> filter(game_date == d) |> select(pitcher, l_tbf = tbf, l_so = so, l_bb = bb,
                                                 l_ibb = any_of("ibb"))
    if (!"l_ibb" %in% names(lg)) lg$l_ibb <- 0L
    j <- inner_join(sc, lg, by = "pitcher") |>
      mutate(d_tbf = l_tbf - tbf - coalesce(l_ibb, 0L),
             d_so  = l_so - so,
             d_bb  = l_bb - bb - coalesce(l_ibb, 0L))
    # IBB is subtracted because an automatic intentional walk throws no pitch and
    # so cannot appear in Statcast. That is known and documented, not a finding.
    bad <- j |> filter(d_tbf != 0 | d_so != 0 | d_bb != 0)
    if (nrow(bad)) {
      note("counting_stat_mismatch",
           sprintf("%s: %d pitchers differ after allowing for IBB (worst TBF gap %d)",
                   d, nrow(bad), max(abs(bad$d_tbf))))
    }
    cat(sprintf("  %s  pitchers %3d  disagreeing %3d\n", d, nrow(j), nrow(bad)))
  }
}

# ---- C. internal invariants --------------------------------------------------
cat("\n== C. invariants ==\n")
set.seed(7)
sample_pg <- ad |> distinct(pitcher, game_date) |> slice_sample(n = 300)
v_split <- 0; v_usage <- 0; v_nan <- 0; v_lost <- 0
for (i in seq_len(nrow(sample_pg))) {
  raw <- ad |> filter(pitcher == sample_pg$pitcher[i], game_date == sample_pg$game_date[i])
  a <- results_statcast(raw, "All"); l <- results_statcast(raw, "L"); r <- results_statcast(raw, "R")
  if (a$tbf != l$tbf + r$tbf) v_split <- v_split + 1
  if (any(is.nan(c(a$k_bb, a$hh, a$xwoba)))) v_nan <- v_nan + 1
  d <- tryCatch(shape_arsenal(raw), error = function(e) NULL)
  if (!is.null(d) && nrow(d)) {
    ch <- reconcile_pitch_codes(raw |> filter(!is.na(pitch_type), pitch_type != ""))
    if (nrow(d) != nrow(ch)) v_lost <- v_lost + 1
    u <- prop.table(table(as.character(d$pitch_type))) * 100
    if (abs(sum(u) - 100) > 1e-6) v_usage <- v_usage + 1
  }
}
cat("  windows checked:", nrow(sample_pg), "\n")
cat("  All != L + R          :", v_split, "\n")
cat("  usage not summing 100 :", v_usage, "\n")
cat("  NaN reaching a metric :", v_nan, "\n")
cat("  chartable pitch lost  :", v_lost, "\n")
if (v_split) note("split_invariant", sprintf("%d windows where All != L + R", v_split))
if (v_usage) note("usage_sum",      sprintf("%d windows where usage did not sum to 100", v_usage))
if (v_nan)   note("nan_leak",       sprintf("%d windows leaking NaN into a displayed metric", v_nan))
if (v_lost)  note("pitch_lost",     sprintf("%d windows losing a chartable pitch", v_lost))

# ---- report ------------------------------------------------------------------
cat("\n", strrep("-", 62), "\n", sep = "")
if (!length(findings)) {
  cat("RECONCILE: CLEAN\n")
} else {
  cat("RECONCILE: ", length(findings), " FINDING(S)\n\n", sep = "")
  print(do.call(rbind, findings), row.names = FALSE)
}
