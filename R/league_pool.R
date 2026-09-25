# league_pool.R
#
# The league-wide tables that Search and Context need, computed once by the
# daily chain instead of on every click.
#
# WHY. The live app runs on a 1 GB shinyapps.io instance, one R process shared by
# every visitor, and idles at roughly 430 to 460 MB of R heap. Aggregating all
# ~636,000 pitches of the season on a click pushed it over: the log shows
# "Container event: oom (out of memory)" on the Search tab (2026-09-24, 21:08
# UTC), and again after the leaner search_aggregate() of PR #12 when Context
# followed three searches (00:04 UTC on 2026-09-25). Precomputing moves that
# work to the chain's runner, where memory is not the constraint.
#
# WHAT. For each date window a preset button can set (All, 1H, 2H, from
# season_halves()) and each batter side (All, L, R), the exact object
# search_aggregate() returns for "All teams". For each window, the release
# profile pitcher_release_profile() returns. Anything else a visitor picks, a
# typed date range or one club, still computes live.
#
# ONE DEFINITION. Every table in the file is produced by the same function the
# app would call, so a pool hit and a live computation cannot disagree. The file
# carries a fingerprint of the code and lookups that built it, and the app
# refuses a file built by different code or for different data (see
# load_league_pool()), falling back to live computation rather than showing
# numbers the page's other tabs would not match.

LEAGUE_POOL_PATH <- "data/league_pool.rds"
POOL_BATTER_SIDES <- c("All", "L", "R")

# The columns pitcher_release_profile() reads. Handing it these alone, instead
# of the whole 34-column window, is what keeps the live fallback small.
RELEASE_PROFILE_COLS <- c("pitcher", "player_name", "p_throws", "release_pos_x",
                          "release_pos_z", "release_extension", "arm_angle")


#' md5 of every file whose contents shape the pooled numbers
#'
#' All of R/ rather than a hand-picked list: a hand-picked list is exactly the
#' kind of thing that goes stale the day someone moves a constant between files.
#' A change anywhere in R/ invalidates the pool until the next chain run
#' rebuilds it, and the app computes live in between. Paths are relative, so the
#' chain (run from the repo root) and the deployed app (run from the bundle
#' root) fingerprint the same files.
pool_fingerprint <- function() {
  f <- sort(c(list.files("R", pattern = "\\.R$", full.names = TRUE),
              list.files("lookups", pattern = "\\.csv$", full.names = TRUE)))
  stats::setNames(unname(tools::md5sum(f)), f)
}


#' The rows of one date window, restricted to some columns, in ONE copy
window_rows <- function(df, from, to, cols) {
  df[which(df$game_date >= from & df$game_date <= to),
     intersect(cols, names(df)), drop = FALSE]
}


pool_key <- function(from, to, side = NULL) {
  paste(c(as.character(from), as.character(to), side), collapse = "|")
}


#' Build the pool from app_data. Called by the daily chain, never by the app.
build_league_pool <- function(app_data, heights = load_pitcher_heights()) {
  h <- season_halves(app_data)
  wins <- Filter(Negate(is.null), list(full = h$full, first = h$first, second = h$second))

  search <- list(); release <- list()
  for (w in wins) {
    from <- as.character(w[1]); to <- as.character(w[2])
    for (side in POOL_BATTER_SIDES) {
      search[[pool_key(from, to, side)]] <-
        search_aggregate(app_data, side, "All", from = from, to = to)
    }
    # min_pitches = 1 so the app applies its own floor afterwards. The function
    # filters n >= min_pitches as its last step, so filtering later is the same.
    release[[pool_key(from, to)]] <-
      pitcher_release_profile(window_rows(app_data, from, to, RELEASE_PROFILE_COLS),
                              heights, min_pitches = 1)
  }

  list(search  = search,
       release = release,
       meta    = list(through     = max(app_data$game_date),
                      n_rows      = nrow(app_data),
                      fingerprint = pool_fingerprint(),
                      built       = format(Sys.time(), tz = "UTC", usetz = TRUE)))
}


#' Read the pool, or NULL if it is missing or does not belong to this app_data
#'
#' NULL is safe: every caller falls back to computing live. A mismatched pool is
#' NOT safe, since it would show league numbers from a different day or a
#' different definition than the rest of the page, so it is rejected loudly.
load_league_pool <- function(app_data, path = LEAGUE_POOL_PATH) {
  if (!file.exists(path)) {
    warning("No ", path, ", Search and Context will compute live.", call. = FALSE)
    return(NULL)
  }
  p <- tryCatch(readRDS(path), error = function(e) NULL)
  why <- if (is.null(p) || is.null(p$meta)) "unreadable"
    else if (!identical(p$meta$through, max(app_data$game_date))) "built for other data dates"
    else if (!identical(p$meta$n_rows, nrow(app_data))) "built for other data rows"
    else if (!identical(p$meta$fingerprint, pool_fingerprint())) "built by different code"
    else NULL
  if (!is.null(why)) {
    warning(path, " rejected (", why, "), Search and Context will compute live.",
            call. = FALSE)
    return(NULL)
  }
  message("league_pool: ", length(p$search), " search tables, ", length(p$release),
          " release tables, built ", p$meta$built)
  p
}


#' The pooled search_aggregate() result for one window and side, or NULL
pool_search <- function(pool, from, to, side) {
  if (is.null(pool)) return(NULL)
  pool$search[[pool_key(from, to, side)]]
}


#' The pooled release profile for one window, or NULL
pool_release <- function(pool, from, to) {
  if (is.null(pool)) return(NULL)
  pool$release[[pool_key(from, to)]]
}
