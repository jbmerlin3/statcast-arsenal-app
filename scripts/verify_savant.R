# verify_savant.R
#
#   Rscript scripts/verify_savant.R
#
# The EXTERNAL half of the traits check. scripts/verify_traits.R proves the
# numbers are computed correctly; nothing in it can prove they are DEFINED
# correctly, because both sides of every comparison there use our definition.
# This diffs against Savant, who publish their own.
#
# Read the MEAN ABSOLUTE ERROR over many pitchers, never one match. CLAUDE.md
# fixes the reading: above about 0.3 on a percentage is a definition difference
# rather than noise.
#
# COVERAGE IS THIN, and the point of this file is to say exactly how thin.
#
#   arm_angle   the ONLY release-tracking column the custom leaderboard serves.
#               Checked below. Note carefully what that check does and does not
#               prove: we ship Savant's own arm_angle column and take its mean,
#               so agreement tests our POPULATION, WINDOW AND AGGREGATION rather
#               than any definition of ours. That is still worth having, because
#               a window or dedup that disagreed with Savant would make every
#               release-trait mean wrong while each per-pitch value stayed right.
#               It is not evidence that release height or VAA is defined well.
#
#   extension   NOT AVAILABLE. Tried release_extension, avg_release_extension,
#               extension, release_extension_avg, pitcher_release_extension:
#               all return an all-NA column. The custom leaderboard echoes any
#               selection name back as a header and fills it with NA when the
#               field is not real, so a careless read of this endpoint LOOKS
#               like a successful fetch. Check for all-NA before trusting it.
#
#   rel ht/side NOT on the custom leaderboard at any usable grain.
#
#   VAA         Savant has no such column anywhere. There is NO external check
#               that exists. verify_traits.R tier C is the whole of it.

suppressPackageStartupMessages({library(dplyr); library(readr)})
YEAR <- 2026
ad <- readRDS("data/app_data.rds")

u <- paste0("https://baseballsavant.mlb.com/leaderboard/custom?year=", YEAR,
            "&type=pitcher&filter=&min=100&selections=player_id,pa,",
            "arm_angle&csv=true")
sv <- suppressWarnings(read_csv(u, show_col_types = FALSE)) |>
  transmute(pitcher = `player_id...2`, pa, sv_arm = arm_angle)

# Guard the all-NA trap described above, so a silently empty column fails loudly
# instead of reporting a perfect MAE over zero comparisons.
stopifnot("Savant returned an all-NA arm_angle; the selection name may have changed" =
            !all(is.na(sv$sv_arm)))

# Pitcher level, all pitches, matching how Savant aggregates its leaderboard.
ours <- ad |> group_by(pitcher) |>
  summarise(n = n(), our_arm = mean(arm_angle, na.rm = TRUE), .groups = "drop")

j <- inner_join(sv, ours, by = "pitcher")
cat("matched pitchers:", nrow(j), "\n\n")

rep <- function(a, b, lab, bar) {
  d <- abs(a - b); d <- d[is.finite(d)]
  stopifnot("nothing comparable survived the join" = length(d) > 0)
  ok <- mean(d) < bar
  cat(sprintf("  %-12s MAE %.4f  median %.4f  max %.3f  n %d   %s\n",
              lab, mean(d), median(d), max(d), length(d), if (ok) "PASS" else "FAIL"))
  ok
}
cat("Ours vs Savant, per pitcher\n")
ok1 <- rep(j$our_arm, j$sv_arm, "arm angle", 1.0)

cat("\nWorst disagreements:\n")
print(j |> mutate(d = abs(our_arm - sv_arm)) |> arrange(desc(d)) |>
  transmute(pitcher, savant_pa = pa, our_pitches = n,
            ours = round(our_arm,2), savant = round(sv_arm,2),
            d = round(d,3)) |> head(5) |> as.data.frame(), row.names = FALSE)

cat("\nNOT CHECKED: extension, release height, release side, VAA.\n",
    "Extension is not served by this endpoint under any name tried. The other\n",
    "three are not published at a usable grain, and VAA is not published at\n",
    "all, so it has NO external ground truth in any source.\n", sep = "")
quit(status = if (ok1) 0 else 1)
