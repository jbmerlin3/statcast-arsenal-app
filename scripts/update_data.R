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
# Run the whole chain with:  Rscript scripts/update_data.R
#
# The app never calls anything in this file. It reads two rds files and renders.

library(purrr)
library(dplyr)


# ---- Paths -------------------------------------------------------------------
#
# The season store lives in the monorepo and is shared with 03_ArsenalReports.
# Overridable so a dry run can point somewhere harmless.

STORE_PATH <- Sys.getenv(
  "STATCAST_STORE",
  path.expand("~/Desktop/Baseball Questionnaires/03_ArsenalReports/statcast_clean_2026.rds")
)
APP_DATA_PATH   <- "data/app_data.rds"
LEAGUE_REF_PATH <- "data/league_ref.rds"


#' Write via a temp file and rename
#'
#' The store is 89 MB and shared with another project. A saveRDS interrupted
#' partway leaves a truncated file where a good one used to be, and the pull
#' that produced it takes minutes to repeat. Rename is atomic on the same
#' filesystem, so the old file survives until the new one is complete.
save_rds_atomic <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  saveRDS(x, tmp)
  if (!file.rename(tmp, path)) stop("could not move ", tmp, " into place")
  invisible(path)
}


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


# ---- Step 1: refresh the season store ----------------------------------------

#' The cleaning filter the store was built with
#'
#' Regular season only, and a pitch needs a type plus the three fields every
#' downstream metric depends on. Kept as its own function so the incremental
#' pull is filtered identically to the original full pull.
clean_statcast <- function(df) {
  df |>
    filter(game_type == "R", pitch_type != "",
           !is.na(release_speed), !is.na(pfx_x), !is.na(pfx_z))
}


#' Pull new days and fold them into the store
#'
#' Starts at `last_date + 1`. Savant's bounds are inclusive, so that is the
#' first day not already held, see the note on get_statcast_chunk().
#'
#' Dedup on game_pk / at_bat_number / pitch_number rather than trusting the
#' date arithmetic. Savant revises pitch classifications for several days after
#' a game, so re-pulling a day already held is a normal thing to want to do and
#' must not double the rows.
refresh_store <- function(store_path = STORE_PATH, through = Sys.Date()) {
  sc <- readRDS(store_path)
  last  <- max(sc$game_date)
  start <- as.Date(last) + 1

  message("Store holds ", format(nrow(sc), big.mark = ","), " rows through ", last)
  if (start > through) {
    message("Nothing to pull, already current through ", through)
    return(sc)
  }
  message("Pulling ", start, " to ", through)

  new_raw <- pull_season_statcast(as.character(start), as.character(through))
  if (is.null(new_raw) || nrow(new_raw) == 0) {
    message("Pull returned no rows, store unchanged")
    return(sc)
  }
  new_clean <- clean_statcast(new_raw)

  # Savant has changed its column set before. Say so rather than letting
  # bind_rows quietly fill a column with NA across half the season.
  #
  # in_zone is excluded because the store derives it and Savant never returns
  # it, so it would warn on every single run. A check that cries wolf daily is
  # one nobody reads on the day the column set actually changes.
  DERIVED <- "in_zone"
  only_new <- setdiff(names(new_clean), names(sc))
  only_old <- setdiff(setdiff(names(sc), names(new_clean)), DERIVED)
  if (length(only_new)) warning("Savant returned new columns: ", paste(only_new, collapse = ", "), call. = FALSE)
  if (length(only_old)) warning("Pull is missing stored columns: ", paste(only_old, collapse = ", "), call. = FALSE)

  out <- bind_rows(sc, new_clean) |>
    distinct(game_pk, at_bat_number, pitch_number, .keep_all = TRUE)

  # The store carries in_zone, and freshly scraped rows arrive without it.
  # Recomputed across the whole frame rather than only the new rows: the formula
  # is identical for existing rows, and this way the column can never be
  # half-populated.
  out <- out |> mutate(in_zone = in_zone_flag(plate_x, plate_z, sz_bot, sz_top))

  message("Added ", format(nrow(out) - nrow(sc), big.mark = ","),
          " rows, now ", format(nrow(out), big.mark = ","), " through ", max(out$game_date))
  out
}


# ---- The chain ---------------------------------------------------------------
#
# Runs when this file is executed with Rscript, not when it is sourced.
# sys.nframe() is 0 only at the top level of an Rscript invocation.
#
# The app reads app_data.rds at startup, so its date range follows from step 3
# with no separate action.

if (sys.nframe() == 0L) {
  if (!dir.exists("R")) stop("run this from the repo root: Rscript scripts/update_data.R", call. = FALSE)
  invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

  message("== Step 1: season store ==")
  sc <- refresh_store()
  save_rds_atomic(sc, STORE_PATH)

  # Built once and used by both remaining steps. The league reference is
  # derived from the same trimmed frame the app reads, so the context and the
  # values it contextualises can never come from different column sets.
  ad <- build_app_data(sc)

  message("\n== Step 2: league_ref.rds ==")
  source("scripts/build_league_ref.R")
  ref <- build_league_ref(ad)
  save_rds_atomic(ref, LEAGUE_REF_PATH)
  message("Wrote ", LEAGUE_REF_PATH, ", ", format(nrow(ref), big.mark = ","), " reference cells, ",
          sum(ref$n_pitchers >= MIN_REF_PITCHERS), " above the pitcher floor")

  message("\n== Step 3: app_data.rds ==")
  save_rds_atomic(ad, APP_DATA_PATH)
  message("Wrote ", APP_DATA_PATH, ", ", format(nrow(ad), big.mark = ","), " rows x ", ncol(ad),
          " cols, ", round(file.size(APP_DATA_PATH) / 1024^2, 1), " MB")
  message("App date range is now ", min(ad$game_date), " to ", max(ad$game_date))
}
