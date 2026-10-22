# build_pitcher_heights.R
# Refresh lookups/pitcher_heights_<year>.csv (listed height per MLBAM id) from the StatsAPI.
#   Rscript scripts/build_pitcher_heights.R
# Tracked and refreshed by hand, outside the daily chain.

YEAR <- as.integer(Sys.getenv("HEIGHT_YEAR", "2026"))
OUT  <- file.path("lookups", sprintf("pitcher_heights_%d.csv", YEAR))

url <- sprintf("https://statsapi.mlb.com/api/v1/sports/1/players?season=%d", YEAR)
message("Fetching ", url)
raw <- jsonlite::fromJSON(url, simplifyVector = FALSE)

people <- raw$people
if (!length(people)) stop("StatsAPI returned no people for ", YEAR, call. = FALSE)

# `6' 5"` to inches; NA if it doesn't parse
parse_ht <- function(s) {
  if (is.null(s) || !nzchar(s)) return(NA_integer_)
  m <- regmatches(s, regexec("^([0-9]+)'\\s*([0-9]+)\"$", s))[[1]]
  if (length(m) != 3) return(NA_integer_)
  as.integer(m[2]) * 12L + as.integer(m[3])
}

out <- do.call(rbind, lapply(people, function(p) {
  data.frame(mlbam_id   = p$id,
             full_name  = p$fullName %||% NA_character_,
             height_str = p$height %||% NA_character_,
             height_in  = parse_ht(p$height),
             pos_code   = (p$primaryPosition$code %||% NA_character_),
             stringsAsFactors = FALSE)
}))

# Mostly NA means the field changed format
ok <- mean(!is.na(out$height_in))
if (ok < 0.95) {
  stop(sprintf("only %.1f%% of %d players parsed a height. The StatsAPI `height` field may have changed format.",
               100 * ok, nrow(out)), call. = FALSE)
}

dir.create("lookups", showWarnings = FALSE)
write.csv(out, OUT, row.names = FALSE)
message(sprintf("Wrote %s: %d players, %.1f%% with a parsed height, %d distinct heights",
                OUT, nrow(out), 100 * ok, length(unique(na.omit(out$height_in)))))
