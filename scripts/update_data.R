# update_data.R
# The daily data chain, the only thing that writes data/. Steps:
#   1. Pull new and recently revised days from Savant into the season store
#   2. Rebuild data/league_ref.rds
#   3. Rebuild data/game_logs.rds from the MLB StatsAPI (fails soft)
#   4. Write data/app_data.rds
#   5. Rebuild data/league_pool.rds (fails soft; the app computes live without it)
#   6. Deploy to shinyapps.io (the data ships inside the bundle)
#
# Usage: Rscript scripts/update_data.R

library(purrr)
library(dplyr)


# ---- Paths -------------------------------------------------------------------

# Season store, outside the repo (override with STATCAST_STORE)
STORE_PATH <- Sys.getenv(
  "STATCAST_STORE",
  path.expand("~/baseball-store/statcast_clean_2026.rds")
)
APP_DATA_PATH   <- "data/app_data.rds"
LEAGUE_REF_PATH <- "data/league_ref.rds"
GAME_LOGS_PATH  <- "data/game_logs.rds"


#' Write to a temp file, then rename (an interrupted save can't truncate the store)
save_rds_atomic <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  saveRDS(x, tmp)
  if (!file.rename(tmp, path)) stop("could not move ", tmp, " into place")
  invisible(path)
}


#' Pull one Statcast chunk from Savant, with retries; NULL on failure.
#' Both date bounds are inclusive (game_date_gt means >=).
get_statcast_chunk <- function(start_date, end_date, retries = 3) {
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true",
                "&hfSea=", format(as.Date(start_date), "%Y"), "%7C&player_type=pitcher",
                "&game_date_gt=", start_date, "&game_date_lt=", end_date,
                "&min_pitches=0&min_results=0&type=details")
  for (i in 1:retries) {
    message("Attempt ", i, ": ", start_date, " to ", end_date)
    result <- tryCatch(read.csv(url, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(result)) return(result)
    Sys.sleep(5)
  }
  message("Failed: ", start_date, " to ", end_date)
  NULL
}


#' Pull a date range in chunks. Failed chunks are dropped.
pull_season_statcast <- function(start_date, end_date, chunk_days = 4) {
  starts <- seq(as.Date(start_date), as.Date(end_date), by = chunk_days)
  ends   <- pmin(starts + chunk_days - 1, as.Date(end_date))
  results <- map2(starts, ends, function(s, e) {
    message("Pulling ", s, " - ", e)
    get_statcast_chunk(as.character(s), as.character(e))
  })
  # Drop failed and empty chunks (an empty CSV reads as all-logical columns)
  results <- results[!map_lgl(results, function(x) is.null(x) || !NROW(x))]
  if (!length(results)) return(NULL)

  bind_rows(results)
}


# ---- Step 4: write app_data.rds ----------------------------------------------

#' Build app_data from the season store: row-wise features, arm angle cleanup
#' (against each pitcher's season median) and the shipped column set.
build_app_data <- function(sc) {
  sc |>
    filter(!is.na(pitch_type), pitch_type != "") |>
    add_pitch_features() |>
    clean_arm_angle() |>
    select(all_of(APP_DATA_COLS))
}


# ---- Step 1: refresh the season store ----------------------------------------

#' The store's cleaning filter: regular season, typed pitches with velocity and
#' movement. Untyped pitches (about 0.6%) and their batters faced are dropped.
clean_statcast <- function(df) {
  df |>
    filter(game_type == "R", pitch_type != "",
           !is.na(release_speed), !is.na(pfx_x), !is.na(pfx_z))
}


#' Today's date in Eastern time (MLB schedules by it; the runner is on UTC)
baseball_today <- function() {
  as.Date(format(Sys.time(), tz = "America/New_York", format = "%Y-%m-%d"))
}


#' Final regular-season games in a window, before today; NA if the schedule is unreachable.
#' Tells "nothing was played" apart from "the pull broke".
schedule_final_games <- function(from, to, today = baseball_today()) {
  u <- sprintf(paste0("https://statsapi.mlb.com/api/v1/schedule",
                      "?sportId=1&startDate=%s&endDate=%s&gameType=R"), from, to)
  j <- tryCatch(jsonlite::fromJSON(u, flatten = TRUE), error = function(e) NULL)
  if (is.null(j)) return(NA_integer_)
  if (is.null(j$dates) || !NROW(j$dates)) return(0L)
  gs <- tryCatch(j$dates$games, error = function(e) NULL)
  if (is.null(gs)) return(NA_integer_)
  keep <- as.Date(j$dates$date) < as.Date(today)
  if (!any(keep)) return(0L)
  sum(unlist(lapply(gs[keep], function(g)
    if (is.null(g) || !NROW(g)) 0L else sum(g$status.abstractGameState == "Final"))))
}


# Days re-pulled each run for Savant's revisions. 7 covers pitch type changes;
# the workflow uses 21 because arm angle fills in later.
REPULL_DAYS <- as.integer(Sys.getenv("REPULL_DAYS", "7"))

# Last regular-season day; no Savant requests after it
SEASON_LAST_DAY <- as.Date("2026-09-27")

#' Pull new days plus the re-pull window and fold them into the store, deduped on
#' game_pk / at_bat_number / pitch_number. Fails if games were played but the store
#' did not advance.
refresh_store <- function(store_path = STORE_PATH, through = baseball_today(),
                          repull_days = REPULL_DAYS) {
  sc <- readRDS(store_path)
  last  <- max(sc$game_date)
  # new_start: first day not held. start: reaches back for revisions.
  new_start <- as.Date(last) + 1
  start     <- max(as.Date(last) - repull_days + 1, as.Date(min(sc$game_date)))

  # Recompute the derived in_zone on every path out
  refresh_derived <- function(df) {
    df |> mutate(in_zone = in_zone_flag(plate_x, plate_z, sz_bot, sz_top))
  }

  message("Store holds ", format(nrow(sc), big.mark = ","), " rows through ", last)
  if (as.Date(through) > SEASON_LAST_DAY && as.Date(last) >= SEASON_LAST_DAY) {
    message("Regular season ended ", SEASON_LAST_DAY, " and the store holds it through ",
            last, ". No pull until next season.")
    return(refresh_derived(sc))
  }
  through <- min(as.Date(through), SEASON_LAST_DAY)
  if (start > through) {
    message("Nothing to pull, already current through ", through)
    return(refresh_derived(sc))
  }
  message("Pulling ", start, " to ", through,
          " (", repull_days, "-day re-pull window; new days start ", new_start, ")")

  # Games on genuinely new days, asked before the pull
  n_games <- if (new_start > through) 0L else
    schedule_final_games(as.character(new_start), as.character(through))
  message(if (is.na(n_games)) "Schedule unreachable, cannot judge an empty pull"
          else paste0(n_games, " final games in that window before today"))

  new_raw <- pull_season_statcast(as.character(start), as.character(through))
  if (is.null(new_raw) || nrow(new_raw) == 0) {
    # Zero rows on a day with finished games is a failure
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

  # Warn if Savant's column set changes
  DERIVED <- "in_zone"
  only_new <- setdiff(names(new_clean), names(sc))
  only_old <- setdiff(setdiff(names(sc), names(new_clean)), DERIVED)
  if (length(only_new)) warning("Savant returned new columns: ", paste(only_new, collapse = ", "), call. = FALSE)
  if (length(only_old)) warning("Pull is missing stored columns: ", paste(only_old, collapse = ", "), call. = FALSE)

  # Count reclassified pitches for the log
  revised <- 0L
  if (nrow(sc)) {
    key <- function(d) paste(d$game_pk, d$at_bat_number, d$pitch_number, sep = "-")
    old_pt <- setNames(as.character(sc$pitch_type), key(sc))
    k_new  <- key(new_clean)
    seen   <- k_new %in% names(old_pt)
    revised <- sum(seen & old_pt[k_new] != as.character(new_clean$pitch_type), na.rm = TRUE)
  }

  # New rows first: distinct() keeps the first copy and revisions win
  out <- bind_rows(new_clean, sc) |>
    distinct(game_pk, at_bat_number, pitch_number, .keep_all = TRUE)

  if (revised > 0) {
    message("Applied ", revised, " reclassified pitch",
            if (revised == 1) "" else "es", " from the re-pull window")
  }

  out <- refresh_derived(out)

  added <- nrow(out) - nrow(sc)
  # Judge by dates: a re-pull can replace rows without adding any
  if (isTRUE(n_games > 0) && max(out$game_date) < as.character(new_start)) {
    stop("Pull returned ", format(nrow(new_clean), big.mark = ","),
         " rows but the store still ends ", max(out$game_date),
         ", with ", n_games, " final games from ", new_start, " to ", through,
         ". It did not advance past ", last, ".", call. = FALSE)
  }

  message("Added ", format(added, big.mark = ","),
          " rows, now ", format(nrow(out), big.mark = ","), " through ", max(out$game_date))

  # Savant lags the schedule by hours; a short end is only a note
  if (max(out$game_date) < as.character(through)) {
    message("Note: store ends ", max(out$game_date), ", request ran to ", through,
            ". Savant has not posted the rest yet.")
  }
  out
}


# ---- The chain ---------------------------------------------------------------

#' Run every step. Sourcing the file loads this without running it.
run_chain <- function() {
  if (!dir.exists("R")) stop("run this from the repo root", call. = FALSE)
  invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

  message("== Step 1: season store ==")
  sc <- refresh_store()
  save_rds_atomic(sc, STORE_PATH)

  # One app_data feeds every later step
  ad <- build_app_data(sc)

  message("\n== Step 2: league_ref.rds ==")
  source("scripts/build_league_ref.R")
  ref <- build_league_ref(ad)
  save_rds_atomic(ref, LEAGUE_REF_PATH)
  message("Wrote ", LEAGUE_REF_PATH, ", ", format(nrow(ref), big.mark = ","), " reference cells, ",
          sum(ref$n_pitchers >= MIN_REF_PITCHERS), " above the pitcher floor")

  message("\n== Step 3: game_logs.rds ==")
  # Game logs depend on StatsAPI uptime; keep the old file on failure
  tryCatch({
    source("scripts/build_game_logs.R")
    gl <- build_game_logs(ad)
    if (!nrow(gl)) stop("game-log pull returned no rows")
    save_rds_atomic(gl, GAME_LOGS_PATH)
    message("Wrote ", GAME_LOGS_PATH, ", ", format(nrow(gl), big.mark = ","),
            " game lines for ", length(unique(gl$pitcher)), " pitchers, through ",
            max(gl$game_date))
  }, error = function(e) {
    warning("Step 3 failed, keeping the existing ", GAME_LOGS_PATH, ": ",
            conditionMessage(e), call. = FALSE)
    message("Step 3 failed: ", conditionMessage(e))
    message("  previous game_logs.rds left in place")
  })

  message("\n== Step 4: app_data.rds ==")
  save_rds_atomic(ad, APP_DATA_PATH)
  message("Wrote ", APP_DATA_PATH, ", ", format(nrow(ad), big.mark = ","), " rows x ", ncol(ad),
          " cols, ", round(file.size(APP_DATA_PATH) / 1024^2, 1), " MB")
  message("App date range is now ", min(ad$game_date), " to ", max(ad$game_date))

  message("\n== Step 5: league_pool.rds ==")
  # On failure, remove the pool rather than ship a stale one
  tryCatch({
    pool <- build_league_pool(ad)
    save_rds_atomic(pool, LEAGUE_POOL_PATH)
    message("Wrote ", LEAGUE_POOL_PATH, ", ", length(pool$search), " search tables and ",
            length(pool$release), " release tables, ",
            round(file.size(LEAGUE_POOL_PATH) / 1024^2, 1), " MB")
  }, error = function(e) {
    if (file.exists(LEAGUE_POOL_PATH)) file.remove(LEAGUE_POOL_PATH)
    warning("Step 5 failed, removed ", LEAGUE_POOL_PATH, ": ", conditionMessage(e), call. = FALSE)
    message("Step 5 failed: ", conditionMessage(e))
    message("  the app will compute Search and Context live")
  })

  # A failed deploy fails the run
  if (nzchar(Sys.getenv("SKIP_DEPLOY"))) {
    message("\nSKIP_DEPLOY is set, not deploying")
    return(invisible(ad))
  }
  message("\n== Step 6: deploy ==")
  source("scripts/deploy.R")
  deploy_app()

  invisible(ad)
}


if (sys.nframe() == 0L) {
  run_chain()
} else {
  message("update_data.R sourced: functions loaded, chain not run.")
  message("  Rscript scripts/update_data.R   to run it, or call run_chain()")
}
