# build_position_players.R
# Refresh lookups/position_players_<year>.csv: position players who pitched, hidden
# from the pitcher dropdown. Uses StatsAPI primary position (P and TWP are kept).
#   Rscript scripts/build_position_players.R

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
