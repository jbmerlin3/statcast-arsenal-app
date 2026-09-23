# step10_vaa_adj.R
#
#   Rscript tests/step10_vaa_adj.R
#
# VAA ships location-adjusted as of 2026-09-22. This file guards the three ways
# that change can go wrong, in order of how quietly each would fail:
#
#  1. The adjustment does not adjust. A no-op leaves the page claiming a
#     property of the pitch while printing a property of his location mix, and
#     every number still renders.
#  2. It over-corrects into a different trait. If the adjusted angle stops
#     tracking the raw one, the app has replaced VAA rather than cleaned it, and
#     no reader would know.
#  3. It fires on a frame too thin to fit. A quadratic on a handful of rows is a
#     number with a confidence interval wider than the league, and it would
#     reach a fixture or a one-day window silently.
suppressMessages({library(dplyr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- character(0)
expect <- function(what, got, want) {
  ok <- isTRUE(all.equal(got, want))
  cat(sprintf("  %-58s %s\n", what, if (ok) "ok" else
    paste0("FAIL got ", paste(format(got), collapse = ","),
           " wanted ", paste(format(want), collapse = ","))))
  if (!ok) fails <<- c(fails, what)
}

ad <- readRDS("data/app_data.rds")

cat("=== the shipped column is an angle, not a residual ===\n")
# Adding the fit back at the pitch type's mean height is what keeps this a
# trait. A residual would centre on zero and read as a grade.
expect("every VAA is negative", all(ad$vaa[is.finite(ad$vaa)] < 0), TRUE)
ff <- ad |> filter(pitch_type == "FF") |> group_by(pitcher) |>
  summarise(v = mean(vaa, na.rm = TRUE), z = mean(plate_z, na.rm = TRUE),
            n = n(), .groups = "drop") |> filter(n >= 200)
cat(sprintf("  FF pitcher means span %.2f to %.2f degrees over %d pitchers\n",
            min(ff$v), max(ff$v), nrow(ff)))
expect("FF pitcher means sit in a scouting range", all(ff$v > -8 & ff$v < -2), TRUE)

cat("\n=== the location component is gone ===\n")
# THE CHECK THIS FILE EXISTS FOR. A pitcher who lives at the top of the zone
# used to rank flatter for it. Measured 2026-09-22 on 328 pitchers with 200+
# four-seams: raw VAA correlated +0.430 with his own average plate height,
# adjusted +0.021.
r_adj <- cor(ff$v, ff$z)
cat(sprintf("  corr(adjusted FF VAA, his mean plate height) = %+.3f\n", r_adj))
expect("adjusted VAA is not a restatement of where he works",
       abs(r_adj) < 0.15, TRUE)

cat("\n=== a thin frame is left raw, never fitted ===\n")
# Under VAA_ADJ_MIN_N the raw angle passes through. A fixture, a one-day window
# or a rare pitch code must not get a quadratic fitted on a handful of rows.
set.seed(11)
thin <- data.frame(pitch_type = "FF",
                   vaa = rnorm(50, -5, 0.6),
                   plate_z = runif(50, 1.5, 3.5))
expect("under the minimum, the input is returned untouched",
       adjust_vaa_for_location(thin), thin$vaa)
# And at scale it must actually change the number, or check 1 is vacuous.
big <- data.frame(pitch_type = "FF",
                  plate_z = runif(VAA_ADJ_MIN_N + 500, 1.2, 3.8))
big$vaa <- -7 + 0.9 * big$plate_z + rnorm(nrow(big), 0, 0.3)
adj <- adjust_vaa_for_location(big)
expect("at scale, the adjustment moves the number", isTRUE(all.equal(adj, big$vaa)), FALSE)
expect("and removes the planted location slope",
       abs(cor(adj, big$plate_z)) < 0.05, TRUE)
# NOT exactly equal, and the gap is the curvature rather than a bug. The
# reference is the fit AT the mean height, while the residuals are taken against
# the fit at each pitch's own height, and for a bending curve the fit at the
# mean is not the mean of the fits. Measured here at 0.001 degrees. Bounded
# rather than pinned, so a genuine recentring (a wrong reference, a dropped
# intercept) still fails this.
expect("the mean barely moves, only by the curvature",
       abs(mean(adj) - mean(big$vaa)) < 0.05, TRUE)
expect("a row with no plate height keeps its raw angle",
       { na_row <- big; na_row$plate_z[1] <- NA_real_
         adjust_vaa_for_location(na_row)[1] }, big$vaa[1])
expect("deterministic across calls",
       identical(adjust_vaa_for_location(big), adj), TRUE)

cat("\n=== still the same trait, against the raw angle ===\n")
# Needs the season store, which carries the trajectory primitives app_data does
# not. Skipped rather than failed when it is absent, and the skip is printed.
store <- Sys.getenv("STATCAST_STORE",
                    unset = path.expand("~/baseball-store/statcast_clean_2026.rds"))
if (!file.exists(store)) {
  cat("  SKIPPED: no season store at", store, "\n")
} else {
  sc <- readRDS(store)
  yf <- 17 / 12
  tf <- (-sc$vy0 - sqrt(sc$vy0^2 - 2 * sc$ay * (50 - yf))) / sc$ay
  sc$raw <- atan((sc$vz0 + sc$az * tf) / abs(sc$vy0 + sc$ay * tf)) * 180 / pi
  rw <- sc |> filter(pitch_type == "FF") |> group_by(pitcher) |>
    summarise(raw = mean(raw, na.rm = TRUE), z = mean(plate_z, na.rm = TRUE),
              n = n(), .groups = "drop") |> filter(n >= 200)
  j <- inner_join(ff |> select(pitcher, adj = v), rw, by = "pitcher")
  cat(sprintf("  corr(adjusted, raw) = %.3f, and raw vs plate height = %+.3f\n",
              cor(j$adj, j$raw), cor(j$raw, j$z)))
  expect("adjusted still tracks raw, so it is the same trait",
         cor(j$adj, j$raw) > 0.85, TRUE)
  expect("and raw really was contaminated, or there was nothing to fix",
         cor(j$raw, j$z) > 0.25, TRUE)
}

cat("\n=== a knuckle curve is fit as the curve it is ranked as ===\n")
# KC folds into CU before ranking, so it must fold in before fitting too, or a
# knuckle curve is centred at its own mean height and ranked among curves
# centred at another. Two probe pitches, one KC and one CU, identical in angle
# and height, must come out identical. Literal fixture: 2,500 CU and 500 KC,
# so KC alone is under the 2,000-row minimum and fitting it separately leaves
# it raw, which is exactly what makes the two probes differ if this regresses.
set.seed(11)
fx <- data.frame(pitch_type = c(rep("CU", 2500), rep("KC", 500), "CU", "KC"),
                 plate_z = c(runif(2500, 0.8, 2.8), runif(500, 0.6, 2.4), 2.5, 2.5))
fx$vaa <- -9.5 + 1.0 * (fx$plate_z - 1.8) + rnorm(nrow(fx), 0, 0.3)
fx$vaa[3001:3002] <- -8.8
a <- adjust_vaa_for_location(fx)
expect("KC and CU probes with the same angle and height match", a[3001], a[3002])
expect("and both were actually adjusted, not left raw", a[3002] != -8.8, TRUE)

cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 10 VAA ADJ: ", if (!length(fails)) "PASS" else "FAIL", "\n", sep = "")
