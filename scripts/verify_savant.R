# verify_savant.R
# Compare per-pitcher arm angle and rate stats (whiff%, chase%, zone%, hard-hit%) with
# Savant's custom leaderboard, as mean absolute error.
#   Rscript scripts/verify_savant.R
# The leaderboard's min=100 returns pitchers with about 415+ PA. Extension, release
# height and side, and VAA have no external source. The leaderboard returns an
# all-NA column for unknown field names.

suppressPackageStartupMessages({library(dplyr); library(readr)})
source("R/theme.R")
YEAR <- 2026
ad <- readRDS("data/app_data.rds")

u <- paste0("https://baseballsavant.mlb.com/leaderboard/custom?year=", YEAR,
            "&type=pitcher&filter=&min=100&selections=player_id,pa,arm_angle,",
            "whiff_percent,oz_swing_percent,in_zone_percent,hard_hit_percent&csv=true")
sv <- suppressMessages(suppressWarnings(read_csv(u, show_col_types = FALSE))) |>
  transmute(pitcher = `player_id...2`, pa, sv_arm = arm_angle, sv_whiff = whiff_percent,
            sv_chase = oz_swing_percent, sv_zone = in_zone_percent, sv_hh = hard_hit_percent)

# Fail on an all-NA column instead of a perfect MAE over nothing
for (col in c("sv_arm", "sv_whiff", "sv_chase", "sv_zone", "sv_hh")) {
  if (all(is.na(sv[[col]]))) stop("Savant returned an all-NA ", col, "; a field name may have changed")
}

# Pitcher level, all pitches, as Savant aggregates
ours <- ad |> group_by(pitcher) |>
  summarise(n       = n(),
            arm     = mean(arm_angle, na.rm = TRUE),
            whiff   = 100 * sum(description %in% whiff_desc) / sum(description %in% swing_only),
            chase   = 100 * sum(in_zone == 0 & description %in% swing_only) / sum(in_zone == 0),
            zone    = 100 * mean(in_zone == 1),
            hh      = 100 * sum(type == "X" & launch_speed >= 95, na.rm = TRUE) / sum(type == "X"),
            .groups = "drop")

j <- inner_join(sv, ours, by = "pitcher")
cat("matched pitchers:", nrow(j), "\n\n")

check <- function(a, b, lab, bar) {
  d <- abs(a - b); d <- d[is.finite(d)]
  stopifnot("nothing comparable survived the join" = length(d) > 0)
  ok <- mean(d) < bar
  cat(sprintf("  %-12s MAE %.3f  median %.3f  max %.3f  n %d   %s\n",
              lab, mean(d), median(d), max(d), length(d), if (ok) "PASS" else "FAIL"))
  ok
}
cat("Ours vs Savant, per pitcher (rates in percentage points; above 0.3 means a definition difference)\n")
ok <- c(check(j$arm,   j$sv_arm,   "arm angle", 1.0),
        check(j$whiff, j$sv_whiff, "whiff%",    0.3),
        check(j$chase, j$sv_chase, "chase%",    0.3),
        check(j$zone,  j$sv_zone,  "zone%",     0.3),
        check(j$hh,    j$sv_hh,    "hard hit%", 0.3))

cat("\nLargest arm angle disagreements:\n")
print(j |> mutate(d = abs(arm - sv_arm)) |> arrange(desc(d)) |>
  transmute(pitcher, savant_pa = pa, our_pitches = n,
            ours = round(arm, 2), savant = round(sv_arm, 2), d = round(d, 3)) |>
  head(5) |> as.data.frame(), row.names = FALSE)

cat("\nNot checked: extension, release height, release side and VAA have no external source.\n")
quit(status = if (all(ok)) 0 else 1)
