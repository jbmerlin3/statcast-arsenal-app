# update_data.R
#
# The daily chain. Nothing else touches the data. Per CLAUDE.md it runs three
# steps in order:
#
#   1. Load the season store, pull from max(game_date) + 1 to today, filter,
#      bind, dedup on game_pk / at_bat_number / pitch_number, save.
#   2. Rebuild data/league_ref.rds.
#   3. Write data/app_data.rds, the trimmed column set.
#
# Phase 1 moves the two scrape functions here so they stop living in the old
# script. The chain itself is not wired yet. Step 3 lands in Phase 2, which is
# the first thing that needs app_data.rds, and step 2 in Phase 3.
#
# The app never calls anything in this file. It reads two rds files and renders.

library(purrr)
library(dplyr)


#' Pull one Statcast chunk from Baseball Savant
#'
#' Retries on failure and returns NULL rather than erroring, so one bad chunk
#' cannot abort a long run.
#'
#' Date bounds are INCLUSIVE, both of them, established by probing the endpoint
#' on 2026-08-19 rather than from the docs:
#'
#'   gt=2026-05-05 lt=2026-05-07 -> 2026-05-05, 2026-05-06, 2026-05-07 all present
#'   gt=2026-05-06 lt=2026-05-06 -> 4324 rows, a strict bound would return zero
#'
#' So `game_date_gt` means >= despite the name. CLAUDE.md previously called both
#' bounds strict, which is wrong, though the instruction it drew from that
#' (start the daily pull at `last_date + 1`) is right for a different reason:
#' `last_date` is already in the store and an inclusive lower bound would just
#' re-fetch it.
#'
#' Two things follow. The chunking below, `ends = starts + chunk_days - 1`, is
#' correct and non-overlapping. And no day is being silently dropped, which was
#' the open worry.
get_statcast_chunk <- function(start_date, end_date, retries = 3) {

  url <- paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?",
    "all=true",
    "&hfSea=", format(as.Date(start_date), "%Y"), "%7C",
    "&player_type=pitcher",
    "&game_date_gt=", start_date,
    "&game_date_lt=", end_date,
    "&min_pitches=0",
    "&min_results=0",
    "&type=details"
  )

  for (i in 1:retries) {

    message("Attempt ", i, ":", start_date, " to ", end_date)

    result <- tryCatch(
      read.csv(url, stringsAsFactors = FALSE),
      error = function(e) NULL
    )

    if (!is.null(result)) {
      return(result)
    }

    Sys.sleep(5)
  }

  message("FAILED: ", start_date, " to ", end_date)
  return(NULL)
}


#' Pull a full date range in chunks
#'
#' Failed chunks are dropped and the run continues, so the result can be short
#' without saying so. Check the row count against expectation before saving over
#' a good store.
pull_season_statcast <- function(start_date, end_date, chunk_days = 4) {

  starts <- seq(
    as.Date(start_date),
    as.Date(end_date),
    by = chunk_days
  )

  ends <- pmin(
    starts + (chunk_days - 1),
    as.Date(end_date)
  )

  results <- map2(
    starts,
    ends,
    function(s, e) {

      message(
        "Pulling ",
        s,
        " - ",
        e
      )

      get_statcast_chunk(
        as.character(s),
        as.character(e)
      )
    }
  )

  # Remove failed downloads
  results <- results[!map_lgl(results, is.null)]

  # Combine all successful pulls
  bind_rows(results)
}


# ---- Step 3: write app_data.rds ----------------------------------------------

#' Build the app's pitch store from the season store
#'
#' Requires R/theme.R and R/features.R to be sourced, for MIN_PITCH_COUNT,
#' add_pitch_features(), and APP_DATA_COLS.
#'
#' Only row-wise work happens here. No per-pitcher counting, no MIN_PITCH_COUNT
#' floor, no usage ordering, because all three depend on the date window the
#' user has not chosen yet. shape_arsenal() does that at query time.
#'
#' The pitch_type filter is defensive. statcast_clean already applies it, but
#' this file is what the app reads and a stray empty code would reach a plot.
build_app_data <- function(sc) {
  sc |>
    filter(!is.na(pitch_type), pitch_type != "") |>
    add_pitch_features() |>
    select(all_of(APP_DATA_COLS))
}
