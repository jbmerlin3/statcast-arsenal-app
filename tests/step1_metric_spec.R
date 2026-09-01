# Step 1 acceptance: the map resolves both ways, direction is total.
suppressMessages({library(dplyr);library(tidyr);library(purrr);library(forcats)
                  library(ggplot2);library(gt);library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))
plt   <- readRDS("tests/fixtures/pl_trim_702070.rds")
stuff <- suppressMessages(load_fg_stuff(702070, "fg_stuff/fg_stuff_2026-03-26_2026-08-17.csv"))
tb    <- arsenal_table(plt, "R", stuff)

bad_keys <- setdiff(names(ARSENAL_METRIC_COLS), names(tb))
bad_vals <- setdiff(unname(ARSENAL_METRIC_COLS), METRIC_SPEC$metric)
# "extreme" joined the set on 2026-08-31 for the release traits. This list is
# the total-ness check for pctile_fill(), so it must be widened deliberately and
# in step with that switch, never to make a failure go away: a direction that
# reaches pctile_fill() without a branch there stops the render.
lvls     <- setdiff(unique(METRIC_SPEC$direction),
                    c("high", "low", "neutral", "extreme"))

cat("map keys absent from arsenal_table :", if (length(bad_keys)) bad_keys else "none", "\n")
cat("map values absent from METRIC_SPEC :", if (length(bad_vals)) bad_vals else "none", "\n")
cat("direction levels outside the three :", if (length(lvls)) lvls else "none", "\n")
cat("direction NA rows                  :", sum(is.na(METRIC_SPEC$direction)), "\n")
ok <- !length(bad_keys) && !length(bad_vals) && !length(lvls) && !anyNA(METRIC_SPEC$direction)
cat("STEP 1: ", if (ok) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (ok) 0 else 1)
