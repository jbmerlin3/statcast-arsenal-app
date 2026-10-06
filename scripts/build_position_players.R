# build_position_players.R
#
#   Rscript scripts/build_position_players.R      refresh lookups/position_players_<year>.csv
#
# MLBAM ids of position players who threw a pitch this season, so the pitcher
# dropdown can leave them out. Tracked CSV, refreshed by hand, for the same
# reason as build_pitcher_heights.R: a StatsAPI outage should never break the
# nightly deploy.
#
# ---- Why primary position and not a pitch or velocity floor -------------------
#
# Measured 2026-10-06 on 868 players who pitched. A 50-pitch floor drops 46 real
# pitchers on a one-game call-up and still keeps Tolbert (106 pitches) and
# Trevino (94). A max-velocity floor fails in the 85 to 88 mph band, where Cortes,
# Robles and Sanchez sit next to Tyler Rogers (85.7) and Alek Jacob (86.7).
# StatsAPI's primary position separates them exactly: 811 P, 1 TWP (Ohtani,
# kept), 56 position players.
#
# The list is an exclusion list on purpose. An id missing from it, a position
# player who pitched after the last refresh, stays in the dropdown, which is the
# old behaviour. Failing toward showing a name beats hiding a real pitcher.

YEAR <- as.integer(Sys.getenv("POSITION_YEAR", "2026"))
OUT  <- file.path("lookups", sprintf("position_players_%d.csv", YEAR))

url <- sprintf(paste0("https://statsapi.mlb.com/api/v1/stats?stats=season&group=pitching",
                      "&season=%d&sportId=1&playerPool=ALL&limit=3000"), YEAR)
message("Fetching ", url)
splits <- jsonlite::fromJSON(url)$stats$splits[[1]]

all_pitched <- unique(data.frame(pitcher = splits$player$id,
                                 name    = splits$player$fullName,
                                 pos     = splits$position$abbreviation,
                                 stringsAsFactors = FALSE))
pp <- all_pitched[!all_pitched$pos %in% c("P", "TWP"), ]
pp <- pp[order(pp$name), ]

utils::write.csv(pp, OUT, row.names = FALSE)
message(nrow(all_pitched), " players pitched, ", nrow(pp), " position players written to ", OUT)
