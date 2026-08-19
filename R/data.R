# data.R
#
# The app's data layer. Loads what the app reads at startup and builds the
# search index.
#
# The app never pulls from Savant and never rebuilds the league reference. It
# reads rds files written by scripts/update_data.R and renders. Anything that
# reaches out to a network belongs in that script, not here.
#
# league_ref.rds joins this file in Phase 3. Nothing in the movement chart needs
# it, so Phase 2 loads app_data alone.

library(dplyr)


#' Load the app's pitch store
#'
#' Fails with the rebuild instruction rather than a bare file-not-found. This is
#' the first thing that breaks on a fresh clone, since app_data.rds is
#' gitignored and has to be generated.
load_app_data <- function(path = "data/app_data.rds") {
  if (!file.exists(path)) {
    stop("Missing ", path, ".\n",
         "Build it with step 3 of the daily chain:\n",
         "  source R/, then scripts/update_data.R, then\n",
         "  saveRDS(build_app_data(readRDS(<statcast_clean_2026.rds>)), \"", path, "\")",
         call. = FALSE)
  }
  readRDS(path)
}


#' Load the precomputed league reference
#'
#' Built by scripts/build_league_ref.R, step 2 of the daily chain. The app never
#' rebuilds it: the per-pitcher league query it comes from would hang a reactive.
load_league_ref <- function(path = "data/league_ref.rds") {
  if (!file.exists(path)) {
    stop("Missing ", path, ".\nBuild it with: Rscript scripts/update_data.R", call. = FALSE)
  }
  readRDS(path)
}


#' Savant's "Last, First" to display order
#'
#' Leaves anything without a comma untouched, so a single-token name passes
#' through rather than being mangled.
format_player_name <- function(x) {
  sub("^\\s*([^,]+),\\s*(.*)$", "\\2 \\1", x)
}


#' Searchable pitcher index
#'
#' Built from app_data itself rather than mlb_rosters(), which guarantees every
#' name the search box offers has pitches behind it. A roster-derived index can
#' offer a pitcher who has not thrown a tracked pitch, and selecting him yields
#' an empty chart with nothing to explain it.
#'
#' The choice value is the pitcher id, not the name. Display names happen to be
#' unique in 2026, but two pitchers sharing a name would otherwise collide
#' silently, and ids never do.
build_player_index <- function(app_data) {
  app_data |>
    distinct(pitcher, player_name) |>
    mutate(display = format_player_name(player_name)) |>
    arrange(display)
}


#' Named vector of choices for selectizeInput
#'
#' setNames(values, labels) is the shape selectize wants: names are shown, values
#' are returned. The value arrives back from the browser as a character string,
#' so callers must coerce before comparing against the integer pitcher column.
player_choices <- function(player_index) {
  stats::setNames(player_index$pitcher, player_index$display)
}


#' First and second half date ranges, derived from the schedule
#'
#' The half boundary is the All-Star break, found as the longest gap between
#' consecutive game dates rather than hardcoded. In 2026 that is 2026-07-12 to
#' 2026-07-16, the only gap over one day all season.
#'
#' The original script hardcoded 2026-07-17, which put 2026-07-16 in the first
#' half. That day holds one game and 258 pitches, so it changed nothing
#' materially, but deriving the boundary is right and survives a season whose
#' break falls elsewhere.
#'
#' Returns `first` and `second` as NULL when no gap of at least `min_gap` days
#' exists, which is the case early in a season. Callers should omit the presets
#' rather than invent a boundary.
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
