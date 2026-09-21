# build_pitcher_heights.R
#
#   Rscript scripts/build_pitcher_heights.R      refresh lookups/pitcher_heights_<year>.csv
#
# Listed height per MLBAM id, for the release-context model in R/release_context.R.
#
# ---- Why this is a TRACKED CSV and not a data/*.rds ---------------------------
#
# data/*.rds is gitignored and rebuilt by the chain on every run. Height is not
# that kind of data. It changes when a roster changes, which is a handful of
# times a season, and putting it in the chain would add a network dependency to
# a job whose whole purpose is to not fail. A StatsAPI outage would then break
# the nightly deploy over a column that had not changed since March.
#
# So it follows the fg_stuff/*.csv pattern instead: tracked, refreshed by hand,
# shipped in the bundle through appFiles in deploy.R. Run this in spring and
# after a big call-up wave, not nightly.
#
# ---- Why listed height at all ------------------------------------------------
#
# Arm angle and release height are 0.82 correlated, because a lower slot means a
# lower release for most pitchers. Listed height is what breaks that link: a 6-6
# pitcher at a 40 degree slot releases the ball well above the 5-11 pitchers who
# share his slot. Without height in the model the residual just re-measures arm
# angle and finds nothing.
#
# ---- What it is NOT ----------------------------------------------------------
#
# It is self-reported and rounded to the inch: 18 distinct values across 1,455
# players. Measured 2026-09-12, the release-height residual moves 0.78 in per
# inch of height error, which is 24 to 26 percent of the residual SD. Two inches
# is about half an SD. That is why expected_release_height() returns a band and
# not just a point estimate. Do not read a single pitcher's residual as exact.

YEAR <- as.integer(Sys.getenv("HEIGHT_YEAR", "2026"))
OUT  <- file.path("lookups", sprintf("pitcher_heights_%d.csv", YEAR))

url <- sprintf("https://statsapi.mlb.com/api/v1/sports/1/players?season=%d", YEAR)
message("Fetching ", url)
raw <- jsonlite::fromJSON(url, simplifyVector = FALSE)

people <- raw$people
if (!length(people)) stop("StatsAPI returned no people for ", YEAR, call. = FALSE)

# Height arrives as the string `6' 5"`. Parsed to inches rather than kept as
# text, because the model needs a number and a silent parse failure must not
# become a plausible-looking height.
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

# An all-NA or mostly-NA height column means the field was renamed, and it would
# otherwise ship as a lookup that joins cleanly and explains nothing. Same
# species as the Savant all-NA leaderboard trap in CLAUDE.md.
ok <- mean(!is.na(out$height_in))
if (ok < 0.95) {
  stop(sprintf("only %.1f%% of %d players parsed a height. The StatsAPI `height` field may have changed format.",
               100 * ok, nrow(out)), call. = FALSE)
}

dir.create("lookups", showWarnings = FALSE)
write.csv(out, OUT, row.names = FALSE)
message(sprintf("Wrote %s: %d players, %.1f%% with a parsed height, %d distinct heights",
                OUT, nrow(out), 100 * ok, length(unique(na.omit(out$height_in)))))
