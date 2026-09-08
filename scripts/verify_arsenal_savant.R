# verify_arsenal_savant.R
#
#   Rscript scripts/verify_arsenal_savant.R
#
# The strongest external check this repo has. Savant's pitch-arsenal leaderboard
# publishes run value, whiff% and pitch counts PER PITCHER PER PITCH TYPE, which
# is exactly the grain the characteristics tables render at. Every other check
# here either compares the app to itself or compares a pitcher-level aggregate.
#
# WINDOW MISMATCH IS EXPECTED AND IS NOT A FINDING. The leaderboard is
# season-to-date and the local store ends whenever it last refreshed, so counts
# differ by roughly the games in between. Read correlation, and read the mean
# gap against that lag. Exact equality would actually be suspicious.
#
# The one thing this settles that nothing else can: the SIGN of run value.
# delta_pitcher_run_exp positive means good for the pitcher, which is asserted
# below against Savant's own wOBA rather than assumed from the column name.
# Getting it backwards paints the league's best pitches deep blue.

suppressPackageStartupMessages({library(dplyr); library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))
YEAR <- 2026
STORE <- Sys.getenv("STATCAST_STORE",
                    unset = path.expand("~/baseball-store/statcast_clean_2026.rds"))
sc <- readRDS(STORE)
cat("local store ends ", max(sc$game_date), "; leaderboard is season-to-date.\n\n", sep = "")

u <- paste0("https://baseballsavant.mlb.com/leaderboard/pitch-arsenal-stats?type=pitcher",
            "&pitchType=&year=", YEAR, "&team=&min=50&csv=true")
sv <- suppressWarnings(read_csv(u, show_col_types = FALSE)) |>
  transmute(pitcher = player_id, pitch_type, sv_pitches = pitches,
            sv_rv = run_value, sv_rv100 = run_value_per_100,
            sv_whiff = whiff_percent, sv_woba = woba)
stopifnot("Savant returned no rows; the endpoint or its parameters may have changed" =
            nrow(sv) > 100)

ours <- sc |> filter(!is.na(pitch_type), pitch_type != "") |>
  reconcile_pitch_codes() |> mutate(pitch_type = as.character(pitch_type)) |>
  group_by(pitcher, pitch_type) |>
  summarise(our_pitches = n(),
            our_rv    = sum(delta_pitcher_run_exp, na.rm = TRUE),
            our_rv100 = 100 * sum(delta_pitcher_run_exp, na.rm = TRUE) / n(),
            our_whiff = 100 * sum(description %in% whiff_desc) /
                              sum(description %in% swing_only),
            .groups = "drop")

j <- inner_join(sv, ours, by = c("pitcher", "pitch_type")) |> filter(our_pitches >= 100)
cat("matched pitcher-pitchtype cells: ", nrow(j), "\n\n", sep = "")

fails <- character()
chk <- function(lab, a, b, bar) {
  r <- cor(a, b, use = "complete.obs"); ok <- r >= bar
  cat(sprintf("  %-11s r = %.4f  MAE %7.3f   ours %8.2f  savant %8.2f   %s\n",
              lab, r, mean(abs(a - b), na.rm = TRUE), mean(a, na.rm = TRUE),
              mean(b, na.rm = TRUE), if (ok) "PASS" else "FAIL"))
  if (!ok) fails <<- c(fails, lab)
}
cat("OURS vs SAVANT, per pitcher-pitchtype\n")
chk("pitches",  j$our_pitches, j$sv_pitches, 0.98)
chk("whiff%",   j$our_whiff,   j$sv_whiff,   0.98)
chk("run value",j$our_rv,      j$sv_rv,      0.90)
chk("RV/100",   j$our_rv100,   j$sv_rv100,   0.90)

# The sign, asserted rather than assumed. A better pitch allows less wOBA, so a
# correct run value must run NEGATIVE against wOBA. If delta_pitcher_run_exp
# were batter-perspective this flips positive and everything above still passes,
# because correlation is sign-blind about a shared convention.
cat("\nSIGN OF RUN VALUE\n")
r_woba  <- cor(j$our_rv100, j$sv_woba,  use = "complete.obs")
r_whiff <- cor(j$our_rv100, j$sv_whiff, use = "complete.obs")
cat(sprintf("  RV/100 vs wOBA allowed : %+.3f   (must be negative)  %s\n",
            r_woba,  if (r_woba  < -0.4) "PASS" else "FAIL"))
cat(sprintf("  RV/100 vs whiff%%       : %+.3f   (must be positive)  %s\n",
            r_whiff, if (r_whiff >  0.05) "PASS" else "FAIL"))
if (r_woba  >= -0.4) fails <- c(fails, "RV sign vs wOBA")
if (r_whiff <=  0.05) fails <- c(fails, "RV sign vs whiff")

cat("\nNOT CHECKED HERE: GB% is not on this leaderboard, nor are the release\n",
    "traits or VAA. GB% is covered by its ground_ball/BBE definition matching\n",
    "the league split it reproduces -- sinkers 54-56%, four-seams 31-33%.\n", sep = "")
cat("\nARSENAL vs SAVANT: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
if (length(fails)) for (f in fails) cat("  failed: ", f, "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
