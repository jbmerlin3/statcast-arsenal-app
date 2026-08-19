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
#' Boundary caveat, unresolved. CLAUDE.md states that `game_date_gt` and
#' `game_date_lt` are strictly greater and less than, and separately that the
#' daily pull should start at `last_date + 1`. Those two cannot both be right:
#' if the bound is strict, starting at `last_date + 1` fetches from
#' `last_date + 2` and silently drops a day. Moved verbatim here rather than
#' guessing which half is wrong. Settle it against the live endpoint before
#' wiring step 1, since either reading loses a day of pitches without erroring.
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
