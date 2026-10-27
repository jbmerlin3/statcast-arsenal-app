# build_game_logs.R
# Step 3 of the chain: per-game pitching lines from the MLB StatsAPI gameLog endpoint.
# The only source for IP, ERA, WHIP and FIP: pitch-level data can't count outs.

library(dplyr)
suppressPackageStartupMessages(library(jsonlite))

GAME_LOG_URL <- "https://statsapi.mlb.com/api/v1/people/%s/stats?stats=gameLog&group=pitching&season=%s"

#' One pitcher's season of game lines, or NULL
fetch_game_log <- function(id, season) {
  j <- tryCatch(fromJSON(sprintf(GAME_LOG_URL, id, season), flatten = TRUE),
                error = function(e) NULL)
  sp <- tryCatch(j$stats$splits[[1]], error = function(e) NULL)
  if (is.null(sp) || !NROW(sp)) return(NULL)

  need <- c("date", "stat.inningsPitched", "stat.earnedRuns", "stat.hits",
            "stat.baseOnBalls", "stat.strikeOuts", "stat.battersFaced",
            "stat.homeRuns", "stat.hitByPitch", "stat.intentionalWalks")
  if (!all(need %in% names(sp))) return(NULL)

  data.frame(
    pitcher   = as.integer(id),
    game_date = as.character(sp$date),
    ip_outs   = ip_to_outs(sp$stat.inningsPitched),
    er        = as.integer(sp$stat.earnedRuns),
    h         = as.integer(sp$stat.hits),
    bb        = as.integer(sp$stat.baseOnBalls),
    so        = as.integer(sp$stat.strikeOuts),
    tbf       = as.integer(sp$stat.battersFaced),
    hr        = as.integer(sp$stat.homeRuns),
    hbp       = as.integer(sp$stat.hitByPitch),
    # Intentional walks throw no pitch; this is the only record of them
    ibb       = as.integer(sp$stat.intentionalWalks),
    stringsAsFactors = FALSE
  )
}

#' Game logs for every pitcher in app_data, one row per pitcher and game date
build_game_logs <- function(ad, season = 2026, ids = NULL) {
  if (is.null(ids)) ids <- sort(unique(ad$pitcher))
  out <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    out[[i]] <- fetch_game_log(ids[[i]], season)
    if (i %% 100 == 0) message("  ", i, " of ", length(ids), " pitchers")
  }
  logs <- bind_rows(out)
  logs |> arrange(pitcher, game_date)
}
