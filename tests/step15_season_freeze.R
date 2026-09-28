# step15_season_freeze.R
#
#   Rscript tests/step15_season_freeze.R
#
# The chain stops asking Savant for data once the regular season is over
# (SEASON_LAST_DAY in scripts/update_data.R, set 2026-09-28). Savant and the
# schedule API are replaced by recorders here, so every check is about which
# request refresh_store() makes, and none of them touches the network.
#
#  1. After the season, with the store holding its last day: no request.
#  2. After the season, with the store short of its last day: one request,
#     ending ON the last day, never after it.
#  3. During the season: the request runs to the date asked, as before.
suppressMessages({library(dplyr); library(purrr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))
suppressMessages(source("scripts/update_data.R"))

fails <- character(0)
expect <- function(what, got, want) {
  ok <- isTRUE(all.equal(got, want))
  cat(sprintf("  %-62s %s\n", what, if (ok) "ok" else
    paste0("FAIL got ", paste(format(got), collapse = ","),
           " wanted ", paste(format(want), collapse = ","))))
  if (!ok) fails <<- c(fails, what)
}

calls <- list()
pull_season_statcast <- function(start_date, end_date, ...) {
  calls[[length(calls) + 1]] <<- c(start = as.character(start_date), end = as.character(end_date))
  NULL
}
schedule_final_games <- function(...) 0L

store_ending <- function(last) {
  path <- tempfile(fileext = ".rds")
  saveRDS(data.frame(game_date = as.character(as.Date(last) - 0:2), pitch_type = "FF",
                     plate_x = 0, plate_z = 2.5, sz_bot = 1.5, sz_top = 3.5,
                     stringsAsFactors = FALSE), path)
  path
}
run <- function(last, through) {
  calls <<- list()
  out <- suppressMessages(refresh_store(store_ending(last), through = as.Date(through), repull_days = 7))
  list(out = out, calls = calls)
}

cat("\n=== the season freeze ===\n")
expect("last day is 2026-09-27", SEASON_LAST_DAY, as.Date("2026-09-27"))

r <- run("2026-09-27", "2026-09-28")
expect("day after, store complete: no Savant request", length(r$calls), 0L)
expect("day after, store complete: store returned unchanged", nrow(r$out), 3L)
r <- run("2026-09-27", "2026-11-15")
expect("mid-November, store complete: no Savant request", length(r$calls), 0L)

r <- run("2026-09-25", "2026-10-03")
expect("store short of the last day: one request", length(r$calls), 1L)
expect("that request ends on the last day, not in October",
       unname(r$calls[[1]]["end"]), "2026-09-27")

r <- run("2026-09-14", "2026-09-15")
expect("in season: request runs to the date asked", unname(r$calls[[1]]["end"]), "2026-09-15")

cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 15 SEASON FREEZE: ", if (!length(fails)) "PASS" else "FAIL", "\n", sep = "")
