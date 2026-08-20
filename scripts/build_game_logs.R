# build_game_logs.R
#
# Step 4 of the daily chain. Game-level pitching lines for every pitcher in
# app_data, which is the only source for IP and therefore for ERA, WHIP and FIP.
#
# `events` in app_data is one row per plate appearance, so a double play reads as
# one out and IP derived from it is wrong. See CLAUDE.md hard rules.
#
# baseballr 2.0.0 has no per-player game-log function. mlb_pitcher_game_logs()
# does not exist, mlb_player_game_stats() is one game at a time, and mlb_stats()
# takes no player id. This calls the StatsAPI gameLog endpoint directly.
#
# Measured 2026-08-20: 0.246 s per pitcher, so 802 pitchers is about 3.3 minutes.
# That is cheap enough to pull everyone and skip a workload threshold entirely.

library(dplyr)
library(jsonlite)

GAME_LOG_URL <- "https://statsapi.mlb.com/api/v1/people/%s/stats?stats=gameLog&group=pitching&season=%s"

# ip_to_outs() and outs_to_ip() live in R/results.R: the app formats IP too,
# and one parser is the point. The chain sources R/ before calling this.

#' One pitcher's season of game lines, or NULL
fetch_game_log <- function(id, season) {
  j <- tryCatch(fromJSON(sprintf(GAME_LOG_URL, id, season), flatten = TRUE),
                error = function(e) NULL)
  sp <- tryCatch(j$stats$splits[[1]], error = function(e) NULL)
  if (is.null(sp) || !NROW(sp)) return(NULL)

  need <- c("date", "stat.inningsPitched", "stat.earnedRuns", "stat.hits",
            "stat.baseOnBalls", "stat.strikeOuts", "stat.battersFaced",
            "stat.homeRuns", "stat.hitByPitch")
  if (!all(need %in% names(sp))) return(NULL)

  data.frame(
    pitcher   = as.integer(id),
    # Character, matching game_date everywhere else in this repo. See CLAUDE.md.
    game_date = as.character(sp$date),
    ip_outs   = ip_to_outs(sp$stat.inningsPitched),
    er        = as.integer(sp$stat.earnedRuns),
    h         = as.integer(sp$stat.hits),
    bb        = as.integer(sp$stat.baseOnBalls),
    so        = as.integer(sp$stat.strikeOuts),
    tbf       = as.integer(sp$stat.battersFaced),
    hr        = as.integer(sp$stat.homeRuns),
    hbp       = as.integer(sp$stat.hitByPitch),
    stringsAsFactors = FALSE
  )
}

#' Game logs for every pitcher in app_data
build_game_logs <- function(ad, season = 2026, ids = NULL) {
  if (is.null(ids)) ids <- sort(unique(ad$pitcher))
  out <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    out[[i]] <- fetch_game_log(ids[[i]], season)
    if (i %% 100 == 0) message("  ", i, " of ", length(ids), " pitchers")
  }
  logs <- bind_rows(out)
  # Keyed by pitcher and game date, per the contract. A doubleheader gives one
  # pitcher two rows on a date only if he threw in both, which is real.
  logs |> arrange(pitcher, game_date)
}
