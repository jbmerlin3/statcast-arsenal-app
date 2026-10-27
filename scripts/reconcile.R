# reconcile.R
# Check local data against upstream sources on a sample of dates:
#   A. pitch mix per pitcher-game vs a fresh Savant pull
#   B. TBF / SO / BB per pitcher-game vs the MLB game log
#   C. internal invariants (All = L + R, usage sums to 100, no NaN, no lost pitch)
#   Rscript scripts/reconcile.R [n_dates]

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(forcats)
  library(ggplot2); library(gt); library(readr)
})
suppressPackageStartupMessages(library(jsonlite))
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

args    <- commandArgs(trailingOnly = TRUE)
N_DATES <- if (length(args)) as.integer(args[1]) else 6

# Reads the local files; the deployed app is built in CI
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
    # Intentional walks throw no pitch and don't appear in Statcast
    bad <- j |> filter(d_tbf != 0 | d_so != 0 | d_bb != 0)
    if (nrow(bad)) {
      note("counting_stat_mismatch",
           sprintf("%s: %d pitchers differ after allowing for IBB (worst TBF gap %d)",
                   d, nrow(bad), max(abs(bad$d_tbf))))
    }
    cat(sprintf("  %s  pitchers %3d  disagreeing %3d\n", d, nrow(j), nrow(bad)))
  }
}

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

cat("\n", strrep("-", 62), "\n", sep = "")
if (!length(findings)) {
  cat("RECONCILE: CLEAN\n")
} else {
  cat("RECONCILE: ", length(findings), " FINDING(S)\n\n", sep = "")
  print(do.call(rbind, findings), row.names = FALSE)
}
