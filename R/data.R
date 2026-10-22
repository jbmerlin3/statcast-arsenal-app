# data.R
# Loaders for the files the app reads at startup, and the pitcher index

library(dplyr)


#' Load the pitch data (built by scripts/update_data.R)
load_app_data <- function(path = "data/app_data.rds") {
  if (!file.exists(path)) {
    stop("Missing ", path, ".\nBuild it with: Rscript scripts/update_data.R (see README)",
         call. = FALSE)
  }
  readRDS(path)
}


#' Load the precomputed league reference
load_league_ref <- function(path = "data/league_ref.rds") {
  if (!file.exists(path)) {
    stop("Missing ", path, ".\nBuild it with: Rscript scripts/update_data.R", call. = FALSE)
  }
  readRDS(path)
}


#' Load game logs, or NULL if missing
load_game_logs <- function(path = "data/game_logs.rds") {
  if (!file.exists(path)) {
    warning("No ", path, ". IP, ERA, WHIP and FIP will be unavailable.", call. = FALSE)
    return(NULL)
  }
  readRDS(path)
}


#' Savant's "Last, First" to "First Last"
format_player_name <- function(x) {
  sub("^\\s*([^,]+),\\s*(.*)$", "\\2 \\1", x)
}


#' Pitcher index built from app_data, keyed by id. `exclude` hides position
#' players from the dropdown only.
build_player_index <- function(app_data, exclude = integer()) {
  app_data |>
    distinct(pitcher, player_name) |>
    filter(!pitcher %in% exclude) |>
    mutate(display = format_player_name(player_name)) |>
    arrange(display)
}


#' Ids of position players who pitched (empty if the lookup is missing)
load_position_players <- function(path = NULL, year = 2026) {
  if (is.null(path)) path <- file.path("lookups", sprintf("position_players_%d.csv", year))
  if (!file.exists(path)) {
    warning("No position-player lookup at ", path,
            ". The pitcher dropdown will list position players. ",
            "Build it with: Rscript scripts/build_position_players.R", call. = FALSE)
    return(integer())
  }
  as.integer(utils::read.csv(path, stringsAsFactors = FALSE)$pitcher)
}


#' Named choices for selectizeInput (the value comes back as character)
player_choices <- function(player_index) {
  stats::setNames(player_index$pitcher, player_index$display)
}


#' Full season and first/second half date ranges. The break is the longest gap
#' between game dates; halves are NULL until a gap of `min_gap` days exists.
season_halves <- function(app_data, min_gap = 3) {
  d <- sort(unique(as.Date(app_data$game_date)))
  full <- c(d[1], d[length(d)])
  gaps <- as.numeric(diff(d))
  if (!length(gaps) || max(gaps) < min_gap) {
    return(list(full = full, first = NULL, second = NULL, boundary = NULL))
  }
  i <- which.max(gaps)
  list(full     = full,
       first    = c(d[1], d[i]),
       second   = c(d[i + 1], d[length(d)]),
       boundary = d[i + 1])
}
