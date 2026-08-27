# update_data.R
#
# The daily chain. Nothing else touches the data. It runs in this order:
#
#   1. Load the season store, pull from max(game_date) + 1 to today, filter,
#      bind, dedup on game_pk / at_bat_number / pitch_number, save.
#   2. Rebuild data/league_ref.rds.
#   4. Rebuild data/game_logs.rds, the only step allowed to fail soft.
#   3. Write data/app_data.rds, the trimmed column set.
#   5. Deploy, because the data rides inside the bundle.
#
# Steps 3 and 4 are numbered in the order they were written, not the order they
# run, which is why 4 appears above 3. Left alone because the numbers appear in
# the log and in CLAUDE.md.
#
# Run the whole chain with:  Rscript scripts/update_data.R
#
# The app never calls anything in this file. It reads three rds files and
# renders.

library(purrr)
library(dplyr)


# ---- Paths -------------------------------------------------------------------
#
# The season store lives in the monorepo and is shared with 03_ArsenalReports.
# Overridable so a dry run can point somewhere harmless.

STORE_PATH <- Sys.getenv(
  "STATCAST_STORE",
  path.expand("~/baseball-store/statcast_clean_2026.rds")
)
APP_DATA_PATH   <- "data/app_data.rds"
LEAGUE_REF_PATH <- "data/league_ref.rds"
GAME_LOGS_PATH  <- "data/game_logs.rds"


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

  # Drop failed downloads AND empty ones.
  #
  # An empty result is not the same shape as a full one. read.csv on a CSV with
  # only a header gives zero rows and types every column `logical`, so
  # bind_rows() meets logical against character and aborts with a vctrs type
  # error. Found 2026-08-27 by a 200-day sweep that reached back to February,
  # before the season, where every chunk is empty. A 7-day window never hits it,
  # which is exactly why it sat here unnoticed.
  results <- results[!map_lgl(results, function(x) is.null(x) || !NROW(x))]
  if (!length(results)) return(NULL)

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
#'
#' KNOWN COST, measured 2026-08-26, recorded so it is not re-derived. The
#' `pitch_type != ""` clause drops pitches Savant failed to classify, and it
#' takes their PA-ending rows with them, so those batters faced disappear from
#' the results panel. Sampling five dates across the season: 0.57% of pitches
#' are untyped, about 874 batters faced over a season. Usually nothing, but it
#' clusters. Savant left 28 of Gerrit Cole's 90 pitches untyped in game 823535 on
#' 2026-06-16, which cost him 8 of his 23 batters faced.
#'
#' Not fixed here on purpose. Keeping untyped pitches would mean excluding them
#' again in every movement, usage and characteristics path, and this store is
#' shared with 03_ArsenalReports, whose contract is that every row has a pitch
#' type. Fixing it belongs in a results-specific store, not in this filter.
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
#' How many regular-season games finished in a date window
#'
#' The chain needs to tell "Savant returned nothing because nothing was played"
#' apart from "Savant returned nothing because the pull broke". Only those two
#' produce the same empty frame, and only one of them is fine.
#'
#' Counts Final games only, and only on dates BEFORE today. Both exclusions
#' matter and they are different.
#'
#' Preview and In Progress games have no complete Statcast rows to pull, so
#' counting them would make every evening run look like a failure.
#'
#' Today's FINAL games are excluded because Savant lags the schedule by hours.
#' A game can be final at 22:00 and not appear on Savant until the small hours,
#' and erroring on that gap would fire nightly. Verified 2026-08-20: six games
#' were final and Savant returned nothing for the date, which is normal and not
#' a failure. Yesterday's games have had a full day, so if those are missing
#' something is genuinely wrong, and the 06:15 run is the one that catches it.
#'
#' Returns NA when the schedule itself cannot be reached, which the caller
#' treats as "cannot judge" rather than as zero.
#' Today, in baseball's timezone
#'
#' NOT Sys.Date(). The chain runs on a GitHub runner with TZ=UTC, where the date
#' rolls over at 20:00 Eastern, in the middle of a night game. A run at 22:00
#' Eastern therefore saw UTC's tomorrow, counted the evening's finished games as
#' having finished YESTERDAY, demanded Savant already have them, and stopped the
#' chain. Savant posts hours later. Observed 2026-08-27T02:00Z: "8 final games in
#' that window before today", 0 rows pulled, run failed.
#'
#' The 06:15 Eastern schedule never hit this, because 10:15 UTC and 06:15 Eastern
#' fall on the same date. It fired on a hand-triggered evening run and would fire
#' on any run between roughly 20:00 and 02:00 Eastern.
#'
#' MLB schedules by Eastern date, so that is the clock the "has Savant had time
#' to post this" question is asked on, wherever the chain happens to run.
baseball_today <- function() {
  as.Date(format(Sys.time(), tz = "America/New_York", format = "%Y-%m-%d"))
}


schedule_final_games <- function(from, to, today = baseball_today()) {
  u <- sprintf(paste0("https://statsapi.mlb.com/api/v1/schedule",
                      "?sportId=1&startDate=%s&endDate=%s&gameType=R"), from, to)
  j <- tryCatch(jsonlite::fromJSON(u, flatten = TRUE), error = function(e) NULL)
  if (is.null(j) || is.null(j$dates) || !NROW(j$dates)) return(0L)
  gs <- tryCatch(j$dates$games, error = function(e) NULL)
  if (is.null(gs)) return(NA_integer_)
  keep <- as.Date(j$dates$date) < as.Date(today)
  if (!any(keep)) return(0L)
  sum(unlist(lapply(gs[keep], function(g)
    if (is.null(g) || !NROW(g)) 0L else sum(g$status.abstractGameState == "Final"))))
}


# How many days back to re-pull on every run.
#
# Savant revises pitch classifications for days after a game. Measured
# 2026-08-27 on Bryce Elder's 2026-08-05 start: six pitches moved FC to FF after
# the fact. The store held the original call and the app therefore showed a
# 24/10 cutter-fastball split against Savant's 18/16, on a total that matched
# exactly, which is the kind of wrong that looks right.
#
# Seven days is comfortably past when revisions stop landing, and costs two
# extra chunk requests a run.
# Overridable so a deep sweep can be run on demand without editing code. The
# daily value is 7; a one-off backfill of every revision Savant has made this
# season is REPULL_DAYS=200 on a workflow_dispatch.
REPULL_DAYS <- as.integer(Sys.getenv("REPULL_DAYS", "7"))

refresh_store <- function(store_path = STORE_PATH, through = baseball_today(),
                          repull_days = REPULL_DAYS) {
  sc <- readRDS(store_path)
  last  <- max(sc$game_date)
  # Two different starts. `new_start` is the first day not held at all, and is
  # what the empty-pull guard reasons about. `start` reaches further back so
  # revisions to days already held actually arrive.
  new_start <- as.Date(last) + 1
  # Never reach back before the store's own first day. A wide sweep would
  # otherwise spend requests on the offseason and, worse, pull empty frames that
  # have nothing to contribute but can still break the bind.
  start     <- max(as.Date(last) - repull_days + 1, as.Date(min(sc$game_date)))

  # in_zone is DERIVED, so it is recomputed over the whole store on every path
  # out of this function, including the two that pull nothing. It used to be
  # recomputed only where new rows were bound in, which meant a change to
  # in_zone_flag() reached the store on the next day a game was played and not
  # before, and on a quiet day the chain could finish green with the old
  # geometry still in the file. Found 2026-08-23 when the vertical ball radius
  # was added and a rerun changed nothing.
  refresh_derived <- function(df) {
    df |> mutate(in_zone = in_zone_flag(plate_x, plate_z, sz_bot, sz_top))
  }

  message("Store holds ", format(nrow(sc), big.mark = ","), " rows through ", last)
  if (start > through) {
    message("Nothing to pull, already current through ", through)
    return(refresh_derived(sc))
  }
  message("Pulling ", start, " to ", through,
          " (", repull_days, "-day re-pull window; new days start ", new_start, ")")

  # Asked before the pull, so a broken pull cannot also break the sanity check.
  # Counts only the genuinely-new days: the re-pulled ones are already held, so
  # games in that stretch say nothing about whether this pull worked.
  n_games <- if (new_start > through) 0L else
    schedule_final_games(as.character(new_start), as.character(through))
  message(if (is.na(n_games)) "Schedule unreachable, cannot judge an empty pull"
          else paste0(n_games, " final games in that window before today"))

  new_raw <- pull_season_statcast(as.character(start), as.character(through))
  if (is.null(new_raw) || nrow(new_raw) == 0) {
    # Zero rows on a day with completed games is a failure, not a no-op. The
    # old behaviour returned the store unchanged and let the chain finish
    # green, which is how a stale store survives a run that looked fine.
    if (isTRUE(n_games > 0)) {
      stop("Pull returned 0 rows for ", start, " to ", through,
           ", but ", n_games, " regular-season games finished in that window. ",
           "Savant is failing or the request is wrong. Store left at ", last,
           ".", call. = FALSE)
    }
    message("Pull returned no rows, and nothing finished before today, ",
            "store unchanged. Savant lags the schedule by hours.")
    return(refresh_derived(sc))
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

  # NEW ROWS FIRST. distinct() keeps the first occurrence of each key, so this
  # ordering is the whole mechanism by which a revision lands: the freshly
  # pulled row wins and the stored one is dropped.
  #
  # It used to be bind_rows(sc, new_clean), which kept the OLD row and made
  # re-pulling a revised day a no-op. The comment above this function has always
  # said re-pulling a revised day is a normal thing to want to do; the code
  # quietly refused to. Found 2026-08-27 via Bryce Elder's 2026-08-05 start,
  # where six pitches Savant had since moved from FC to FF were still stored as
  # FC, giving a 24/10 split against Savant's 18/16 on an identical total.
  revised <- 0L
  if (nrow(sc)) {
    key <- function(d) paste(d$game_pk, d$at_bat_number, d$pitch_number, sep = "-")
    old_pt <- setNames(as.character(sc$pitch_type), key(sc))
    k_new  <- key(new_clean)
    seen   <- k_new %in% names(old_pt)
    revised <- sum(seen & old_pt[k_new] != as.character(new_clean$pitch_type), na.rm = TRUE)
  }

  out <- bind_rows(new_clean, sc) |>
    distinct(game_pk, at_bat_number, pitch_number, .keep_all = TRUE)

  if (revised > 0) {
    message("Applied ", revised, " reclassified pitch",
            if (revised == 1) "" else "es", " from the re-pull window")
  }

  # The store carries in_zone, and freshly scraped rows arrive without it.
  # Recomputed across the whole frame rather than only the new rows: the formula
  # is identical for existing rows, and this way the column can never be
  # half-populated.
  out <- refresh_derived(out)

  added <- nrow(out) - nrow(sc)
  # Asked about DATES, not row count. With a re-pull window a healthy run can
  # legitimately add zero rows and still have done its job, by replacing
  # existing ones with revised copies. The old row-count test would have called
  # that a failure. What must not happen is the store failing to reach a day on
  # which games were played.
  if (isTRUE(n_games > 0) && max(out$game_date) < as.character(new_start)) {
    stop("Pull returned ", format(nrow(new_clean), big.mark = ","),
         " rows but the store still ends ", max(out$game_date),
         ", with ", n_games, " final games from ", new_start, " to ", through,
         ". It did not advance past ", last, ".", call. = FALSE)
  }

  message("Added ", format(added, big.mark = ","),
          " rows, now ", format(nrow(out), big.mark = ","), " through ", max(out$game_date))

  # Savant lags the schedule by hours, so this is reported and not an error:
  # the evening run legitimately sees today's finals before Savant posts them.
  if (max(out$game_date) < as.character(through)) {
    message("Note: store ends ", max(out$game_date), ", request ran to ", through,
            ". Savant has not posted the rest yet.")
  }
  out
}


# ---- The chain ---------------------------------------------------------------

#' Run all four steps
#'
#' A named function rather than a bare block, because the bare block was gated
#' on `sys.nframe() == 0L` and that is TRUE only under Rscript. Sourcing this
#' file from an R session gives `sys.nframe() == 4`, so the whole chain was
#' skipped, printed nothing, and returned cleanly. A stale store survived a run
#' that looked like it had worked. The gate now selects between running and
#' SAYING it is not running, never between running and silence.
#'
#' The app reads app_data.rds at startup, so its date range follows from step 3
#' with no separate action.
run_chain <- function() {
  if (!dir.exists("R")) stop("run this from the repo root", call. = FALSE)
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

  message("\n== Step 4: game_logs.rds ==")
  # Fails soft on purpose. Steps 1 to 3 are pure transforms of one store; this
  # one depends on somebody else's uptime, so a bad night costs a stale results
  # panel rather than the whole chain. The previous file is left in place and
  # the panel prints the log's own max game date, so staleness shows on the page.
  tryCatch({
    source("scripts/build_game_logs.R")
    gl <- build_game_logs(ad)
    if (!nrow(gl)) stop("game-log pull returned no rows")
    save_rds_atomic(gl, GAME_LOGS_PATH)
    message("Wrote ", GAME_LOGS_PATH, ", ", format(nrow(gl), big.mark = ","),
            " game lines for ", length(unique(gl$pitcher)), " pitchers, through ",
            max(gl$game_date))
  }, error = function(e) {
    warning("Step 4 failed, keeping the existing ", GAME_LOGS_PATH, ": ",
            conditionMessage(e), call. = FALSE)
    message("Step 4 FAILED: ", conditionMessage(e))
    message("  previous game_logs.rds left in place")
  })

  message("\n== Step 3: app_data.rds ==")
  save_rds_atomic(ad, APP_DATA_PATH)
  message("Wrote ", APP_DATA_PATH, ", ", format(nrow(ad), big.mark = ","), " rows x ", ncol(ad),
          " cols, ", round(file.size(APP_DATA_PATH) / 1024^2, 1), " MB")
  message("App date range is now ", min(ad$game_date), " to ", max(ad$game_date))

  # The data files ship inside the bundle, so steps 1 to 4 do nothing for the
  # deployed link until this runs. It used to be a manual step, and the result
  # was a chain that reported RUN OK every morning while the live page sat at
  # whatever the last hand deploy left there. Found 2026-08-26 with the chain
  # holding 08-25 and the live page reading "Data through 2026-08-24".
  #
  # Not wrapped in tryCatch, unlike step 4. A failed game-log pull is visible on
  # the page, because the panel prints the log's own max date. A failed deploy
  # is the opposite: the live page keeps rendering older data with no sign
  # anything is wrong, which is the whole failure being fixed here. So it fails
  # the run, chain_status.sh says FAILED, and tomorrow's run tries again.
  # Steps 1 to 4 have already saved by this point, so a failed deploy costs the
  # deploy and nothing else.
  message("\n== Step 5: deploy ==")
  source("scripts/deploy.R")
  deploy_app()

  invisible(ad)
}


if (sys.nframe() == 0L) {
  run_chain()
} else {
  # Sourced, not run. Say so, loudly. This is the branch that used to be empty.
  message("update_data.R sourced: functions loaded, THE CHAIN DID NOT RUN.")
  message("  Rscript scripts/update_data.R   to run it, or call run_chain()")
}
