# league_pool.R
# Search and Context league tables for the preset date windows, precomputed by
# the daily chain to keep whole-season aggregation off the 1 GB app instance.
# Built with the app's own functions; rejected if built from other code or data.

LEAGUE_POOL_PATH <- "data/league_pool.rds"
POOL_BATTER_SIDES <- c("All", "L", "R")

# Columns pitcher_release_profile() reads
RELEASE_PROFILE_COLS <- c("pitcher", "player_name", "p_throws", "release_pos_x",
                          "release_pos_z", "release_extension", "arm_angle")


#' md5 of every file in R/ and lookups/; any change invalidates the pool
pool_fingerprint <- function() {
  f <- sort(c(list.files("R", pattern = "\\.R$", full.names = TRUE),
              list.files("lookups", pattern = "\\.csv$", full.names = TRUE)))
  stats::setNames(unname(tools::md5sum(f)), f)
}


#' One date window's rows, restricted to some columns
window_rows <- function(df, from, to, cols) {
  df[which(df$game_date >= from & df$game_date <= to),
     intersect(cols, names(df)), drop = FALSE]
}


pool_key <- function(from, to, side = NULL) {
  paste(c(as.character(from), as.character(to), side), collapse = "|")
}


#' Build the pool from app_data (called by the daily chain)
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
    # The app applies its own pitch floor
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


#' Read the pool, or NULL if missing or built for different data or code
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


#' Pooled search_aggregate() result for one window and side, or NULL
pool_search <- function(pool, from, to, side) {
  if (is.null(pool)) return(NULL)
  pool$search[[pool_key(from, to, side)]]
}


#' Pooled release profile for one window, or NULL
pool_release <- function(pool, from, to) {
  if (is.null(pool)) return(NULL)
  pool$release[[pool_key(from, to)]]
}
