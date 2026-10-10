# step17_arm_angle.R
#
#   Rscript tests/step17_arm_angle.R
#
# clean_arm_angle(), the guard against Savant arm angles that disagree with an
# unchanged release point. The fixture is a clean starter, so the bad pitches
# are planted: the Hudson pattern from game 822873 (about -50 degrees, release
# point untouched) must be blanked, and a position-player-style swing (angle
# AND release point both move) must survive.

suppressMessages({library(dplyr); library(tidyr); library(purrr); library(forcats)
                  library(ggplot2); library(gt); library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- 0
expect <- function(label, got, want) {
  ok <- isTRUE(all.equal(got, want, tolerance = 1e-9))
  cat(if (ok) "  ok   " else "  FAIL ", label, "\n", sep = "")
  if (!ok) { fails <<- fails + 1; cat("       got: ", format(got), "\n       want: ", format(want), "\n") }
}

d <- readRDS("tests/fixtures/pl_trim_702070.rds")
d <- d[!is.na(d$arm_angle), ]
med <- median(d$arm_angle)

expect("a clean pitcher is untouched", identical(clean_arm_angle(d)$arm_angle, d$arm_angle), TRUE)

# The Hudson pattern: angle 60 degrees off, release point where it always is.
# The six pitches nearest his median release point, as the real ones were.
shift <- sqrt((d$release_pos_x - median(d$release_pos_x))^2 + (d$release_pos_z - median(d$release_pos_z))^2)
i <- order(shift)[1:6]
bad <- d
bad$arm_angle[i] <- med - 60
out <- clean_arm_angle(bad)
expect("planted -60 degree readings are blanked", all(is.na(out$arm_angle[i])), TRUE)
expect("and nothing else is", identical(out$arm_angle[-i], bad$arm_angle[-i]), TRUE)
expect("other columns are untouched", identical(out[names(out) != "arm_angle"], bad[names(bad) != "arm_angle"]), TRUE)
expect("the class comes back unchanged", class(out), class(bad))
expect("rows are kept, only the angle goes", nrow(out), nrow(bad))

# A real slot change moves the hand: angle off by 40 AND release 1.2 ft lower.
real <- d; j <- order(shift)[7:12]
real$arm_angle[j] <- med - 40
real$release_pos_z[j] <- real$release_pos_z[j] - 1.2
expect("an angle change with a release change is kept",
       identical(clean_arm_angle(real)$arm_angle[j], real$arm_angle[j]), TRUE)

# The 30 degree line is a boundary, not a band: 25 off with no release change
# is inside a pitcher's tail and stays.
near <- d; near$arm_angle[i] <- med + 25
expect("25 degrees off is kept", identical(clean_arm_angle(near)$arm_angle[i], near$arm_angle[i]), TRUE)

expect("idempotent", identical(clean_arm_angle(out), out), TRUE)

# Medians are per pitcher: a second pitcher's angles must not pull this one's.
other <- d; other$pitcher <- other$pitcher + 1L; other$arm_angle <- other$arm_angle - 45
two <- bind_rows(bad, other)
expect("per pitcher, not pooled", sum(is.na(clean_arm_angle(two)$arm_angle)), length(i))

cat(if (fails == 0) "\nSTEP 17 ARM ANGLE: PASS\n" else sprintf("\nSTEP 17 ARM ANGLE: FAIL (%d)\n", fails))
if (fails > 0) quit(status = 1)
