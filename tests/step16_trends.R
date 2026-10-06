# step16_trends.R
#
#   Rscript tests/step16_trends.R
#
# The Trends tab. Checks the numbers trend_series() hands the plot, and the
# titles built from them, against values computed by hand from the fixture.

suppressMessages({library(dplyr); library(tidyr); library(purrr); library(forcats)
                  library(ggplot2); library(gt); library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- 0
expect <- function(label, got, want) {
  ok <- isTRUE(all.equal(got, want, tolerance = 1e-9))
  cat(if (ok) "  ok   " else "  FAIL ", label, "\n", sep = "")
  if (!ok) { fails <<- fails + 1; cat("       got: ", format(got), "\n       want: ", format(want), "\n") }
}

d <- shape_arsenal(readRDS("tests/fixtures/pl_trim_702070.rds"))
ch <- trend_pitch_choices(d)
expect("selector is most-used first", unname(ch), sort(unname(ch), decreasing = TRUE))

s  <- trend_series(d, "FF", "R")
pv <- s$points |> filter(metric == "velo") |> arrange(game_date)
ff <- d[d$pitch_type == "FF", ]
expect("first game velo is that game's FF mean", pv$v[1],
       mean(ff$release_speed[ff$game_date == pv$game_date[1]], na.rm = TRUE))
expect("window spin is the traits table's mean over pitches",
       s$summ$season[s$summ$metric == "spin"], mean(ff$release_spin_rate, na.rm = TRUE))
expect("six panels for a starter, in order", s$summ$metric,
       c("velo", "spin", "ivb", "hb", "usage", "arm_angle"))
expect("window velo is the traits table's mean over pitches",
       s$summ$season[s$summ$metric == "velo"], mean(ff$release_speed, na.rm = TRUE))

last_days <- tail(pv$game_date, TREND_LAST_N)
expect("last-5 velo is the mean over pitches in the last 5 plotted games",
       s$summ$last[s$summ$metric == "velo"],
       mean(ff$release_speed[ff$game_date %in% last_days], na.rm = TRUE))

# Thin games: under TREND_MIN_N of the pitch, no point. Discriminates only if
# the fixture has some, so the count is printed.
thin <- d |> count(pitch_type, game_date) |> filter(n < TREND_MIN_N)
cat("       (", nrow(thin), " thin pitch-type games in the fixture)\n", sep = "")
for (pt in unique(thin$pitch_type)) {
  pts <- trend_series(d, pt, "All")$points |> filter(metric == "velo")
  expect(paste("no", pt, "point under TREND_MIN_N"),
         any(pts$game_date %in% thin$game_date[thin$pitch_type == pt]), FALSE)
}

# HB keeps the raw sign. The fixture is a left-hander, so his FF is negative.
expect("LHP four-seam HB stays negative",
       all(s$points$v[s$points$metric == "hb"] < 0), TRUE)

# Usage follows the batter side; outings are counted on both sides.
u <- s$points |> filter(metric == "usage") |> arrange(game_date)
g1 <- d[d$game_date == u$game_date[1] & d$stand == "R", ]
expect("first-game usage vs RHH", u$v[1], mean(g1$pitch_type == "FF") * 100)
expect("first rolling share equals the first game's", u$line[1], u$v[1])
games <- trend_usage_games(d)
expect("usage outings counted on both sides", all(games$game_n >= TREND_USAGE_MIN_GAME), TRUE)

# Arm angle ignores the pitch selection.
expect("arm angle is the same whatever pitch is selected",
       trend_series(d, "SL", "R")$summ$season[6], s$summ$season[6])

# A reliever-only window has no usage panel.
short <- d[!d$game_date %in% games$game_date, ]
if (nrow(short) > 0) expect("no usage panel without 2 starter-length outings",
                            "usage" %in% trend_series(short, "FF", "All")$summ$metric, FALSE)

tt <- trend_titles(s$summ, "R")
expect("usage title names the side", grepl("^Usage vs RHH", tt[5]), TRUE)
expect("velo title reads avg then last 5 games", grepl("avg .* mph .*last 5 games .* mph$", tt[1]), TRUE)
expect("shading starts at the first of the last 5 velo games",
       s$summ$from[s$summ$metric == "velo"], min(last_days))

# Hover tooltip: the game date, the sample, and the value against the average.
tp  <- trend_panels(d, "FF", "R")
row <- tp$pts |> filter(metric == "hb") |> slice(1)
tip <- trend_tip_text(row, tp)
expect("tip opens with the game date", tip[1], format(row$game_date, "%b %d, %Y"))
expect("tip gives the pitch count", tip[2], paste0(row$n, " FFs"))
expect("tip compares to the window average", grepl("^HB -.*\\(avg -", tip[3]), TRUE)
urow <- tp$pts |> filter(metric == "usage") |> slice(1)
expect("usage tip names the side and its denominator",
       trend_tip_text(urow, tp)[2], paste0(urow$n, " of ", urow$tot, " pitches vs RHH"))
expect("plot builds from the panels", inherits(plot_trends(tp), "ggplot"), TRUE)

# Band: his game-to-game SD, over the plotted game values.
expect("band sd is the SD of the game values", s$summ$sd[s$summ$metric == "velo"], sd(pv$v))

# What changed. Discriminates both ways: a synthetic +3 mph on the last 5 FF
# games must be flagged; the same pitch with its game order intact must not
# carry a flag it did not earn from the data.
ffg  <- sort(unique(d$game_date[d$pitch_type == "FF"]))
bump <- d; hit <- bump$pitch_type == "FF" & bump$game_date %in% tail(ffg, TREND_LAST_N)
bump$release_speed[hit] <- bump$release_speed[hit] + 3
chb <- trend_changes(bump, "All")
expect("a +3 mph last-5 jump is flagged", any(chb$pitch == "FF" & chb$metric == "velo" & chb$diff > 0), TRUE)
expect("and reads as velocity up on FF", any(grepl("^Velocity up: .*FF \\+", trend_change_text(chb, "All"))), TRUE)
flat <- d; flat$release_speed[flat$pitch_type == "FF"] <- 93
expect("a constant velo is never flagged",
       any(trend_changes(flat, "All")$metric == "velo" & trend_changes(flat, "All")$pitch == "FF"), FALSE)
set.seed(1)
small <- d; hit2 <- small$pitch_type == "FF" & small$game_date %in% tail(ffg, TREND_LAST_N)
small$release_speed[small$pitch_type == "FF"] <- 93 + rnorm(sum(small$pitch_type == "FF"), 0, 0.05)
small$release_speed[hit2] <- small$release_speed[hit2] + 0.3
expect("a significant but sub-0.5 mph change is not flagged",
       any(trend_changes(small, "All")$metric == "velo"), FALSE)
hbtxt <- trend_change_text(data.frame(pitch = "FF", metric = "hb", season = -12, last = -14,
                                      diff = -2, sd = 1, z = -4), "All")
expect("HB away from zero reads as more break", hbtxt, "HB more break: FF -2.0\"")

expect("roll_sum trails by three", roll_sum(c(1, 2, 3, 4)), c(1, 3, 6, 9))

cat(if (fails == 0) "\nTRENDS: PASS\n" else sprintf("\nTRENDS: FAIL (%d)\n", fails))
if (fails > 0) quit(status = 1)
