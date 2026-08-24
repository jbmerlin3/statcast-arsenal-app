# phase6_results.R
#
# The results panel. Two sources, two rows, and the one thing that must never
# happen: the game-log row narrowing with the batter side.
suppressMessages({library(dplyr);library(tidyr);library(purrr);library(forcats)
                  library(ggplot2);library(gt);library(readr);library(shiny)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))
ad <- readRDS("data/app_data.rds"); gl <- readRDS("data/game_logs.rds")

fails <- character()
expect <- function(l, got, want) if (!identical(got, want))
  fails <<- c(fails, sprintf("%s: got %s, wanted %s", l, deparse(got), deparse(want)))
html <- function(...) as.character(results_panel(...))

win <- c("2026-08-04", "2026-08-17"); cf <- fip_constant(gl)
d   <- build_pitch_level(ad, 702070) |> filter(game_date >= win[1], game_date <= win[2])

cat("=== the Statcast row narrows with the side, the game-log row does not ===\n")
sc <- lapply(c("All","R","L"), function(h) results_statcast(d, h))
names(sc) <- c("All","R","L")
for (h in names(sc)) cat(sprintf("  %-3s TBF %3d  K-BB%% %5.1f  HH%% %5.1f  xwOBA %.3f\n",
  h, sc[[h]]$tbf, sc[[h]]$k_bb, sc[[h]]$hh, sc[[h]]$xwoba))
expect("the sides partition TBF", sc$R$tbf + sc$L$tbf, sc$All$tbf)
expect("vs RHH is a strict subset", sc$R$tbf < sc$All$tbf, TRUE)

g <- results_gamelog(gl, 702070, win, cf)
cat(sprintf("  game log: %d G  IP %s  ERA %.2f  WHIP %.2f  FIP %.2f\n", g$games, g$ip, g$era, g$whip, g$fip))

# results_gamelog takes no hand argument at all, which is the structural
# guarantee. If one is ever added, this stops compiling rather than silently
# narrowing.
expect("results_gamelog cannot see the batter side",
       "hand" %in% names(formals(results_gamelog)), FALSE)
expect("results_gamelog cannot see stand either",
       any(grepl("stand", deparse(body(results_gamelog)))), FALSE)

cat("\n=== the game-log header tracks dates and ignores the side ===\n")
heads <- function(h) {
  x <- html(results_statcast(d, h), g, win, h, "2026-08-20")
  regmatches(x, gregexpr('rp-head">[^<]*<', x))[[1]]
}
hR <- heads("R"); hA <- heads("All")
cat("  vs RHH:", paste(gsub('rp-head">|<', "", hR), collapse = "  |  "), "\n")
cat("  All   :", paste(gsub('rp-head">|<', "", hA), collapse = "  |  "), "\n")
expect("the Statcast header moves with the side", hR[1] == hA[1], FALSE)
expect("the game-log header does NOT move with the side", hR[2], hA[2])

win2 <- c("2026-07-01", "2026-07-14")
g2 <- results_gamelog(gl, 702070, win2, cf)
h2 <- regmatches(html(results_statcast(d, "R"), g2, win2, "R", "2026-08-20"),
                 gregexpr('rp-head">[^<]*<', html(results_statcast(d, "R"), g2, win2, "R", "2026-08-20")))[[1]]
cat("  other window, game-log header:", gsub('rp-head">|<', "", h2[2]), "\n")
expect("the game-log header DOES move with the dates", h2[2] == hR[2], FALSE)

cat("\n=== IP comes from the game log, never from events ===\n")
# events is one row per PA, so a double play reads as one out. The two numbers
# must NOT agree, and this is what catches IP being derived the wrong way.
pa_outs <- sum(build_pitch_level(ad, 702070) |>
  filter(game_date >= win[1], game_date <= win[2]) |>
  pull(events) %in% c("field_out","strikeout","force_out","grounded_into_double_play",
                      "sac_fly","sac_bunt","double_play","fielders_choice_out"))
cat("  game-log outs:", g$ip_outs, "   outs if counted off events:", pa_outs, "\n")
expect("game-log IP is not the events count", g$ip_outs == pa_outs, FALSE)
expect("game-log outs exceed the events count", g$ip_outs > pa_outs, TRUE)
# 5.2 is five and two thirds, not five point two.
expect("IP parses as thirds, not decimals", ip_to_outs(c("5.0","5.1","5.2","6.0")), c(15L,16L,17L,18L))

cat("\n=== absence is reported, not zeroed ===\n")
stale <- gl[gl$game_date <= "2026-07-01", ]
gs <- results_gamelog(stale, 702070, win, fip_constant(stale))
hs <- html(sc$R, gs, win, "R", max(stale$game_date))
expect("absence says so", grepl("No game log in this window", hs, fixed = TRUE), TRUE)
expect("absence renders no zero", grepl("0.00", hs, fixed = TRUE), FALSE)
expect("absence renders no empty value", grepl('rp-val"></div>', hs, fixed = TRUE), FALSE)
expect("the Statcast row survives it", grepl(">59<", hs, fixed = TRUE), TRUE)
expect("the stale log date is on the page", grepl("Log file through 2026-07-01", hs, fixed = TRUE), TRUE)
cat("  stale-log panel reports absence and keeps the Statcast row\n")

cat("\n=== the FIP constant is derived, not hardcoded ===\n")
cat("  cFIP:", round(cf, 3), "\n")
expect("cFIP is in the plausible band", cf > 2.5 && cf < 3.7, TRUE)

cat("\n=== HH% counts batted balls, not every tracked exit velocity ===\n")
# Statcast measures fouls, and fouls are weak, so a denominator of "rows with an
# exit velocity" halves the rate. Kirby 2026 vs LHH read 28.3 against
# FanGraphs' 45.4 until the denominator became type == "X".
#
# A literal fixture rather than a slice of app_data. The season store grows
# every night, so a rate pinned off live data would have to be re-pinned daily,
# and a fixture that moves with the data cannot pin anything. See CLAUDE.md
# entry 5.
#
# The numbers discriminate three ways. Two of the four batted balls are hard
# hit, so the answer is 50. Counting every tracked EV gives 4 of 5, 80. Dropping
# the untracked batted ball gives 2 of 3, 66.7.
hh_frame <- tibble::tibble(
  stand        = "R",
  type         = c("X", "X", "X", "X", "S", "S", "S"),
  description  = c(rep("hit_into_play", 4), "foul", "foul", "swinging_strike"),
  launch_speed = c(100, 96, 94, NA, 105, 99, NA),
  events       = c("single", "field_out", "field_out", "field_out", "", "", ""),
  woba_denom   = c(1, 1, 1, 1, 0, 0, 0),
  estimated_woba_using_speedangle = c(0.9, 0.3, 0.2, 0.1, NA, NA, NA))
hh <- results_statcast(hh_frame, "All")
cat(sprintf("  batted balls %d, hard hit 2, HH%% %.1f\n", hh$n_bip, hh$hh))
expect("HH% denominator is batted balls, not tracked EVs", hh$n_bip, 4L)
expect("a 105 mph foul does not count as hard hit", round(hh$hh, 1), 50)

cat("\n=== league context: direction, floors, and the window ===\n")
# A literal league. Ten pitchers spread evenly, so a percentile is a count and
# every expectation below is arithmetic rather than a quantile of live data.
# The plate appearances PARTITION: k strikeouts, bb walks, and the rest are
# batted balls. Deriving bbe rather than passing it is the fix for the first
# version, where a high strikeout count made the remainder negative.
lg_arm <- function(id, tbf, k, bb, hard, date = "2026-08-01") {
  bbe <- tbf - k - bb
  stopifnot("the fixture must partition its plate appearances" = bbe >= 0,
            "cannot hard-hit more balls than were put in play" = hard <= bbe)
  tibble::tibble(
    pitcher = id, player_name = paste0("A", id), game_date = date,
    pitch_type = "FF", stand = "R", p_throws = "R", balls = 0, strikes = 0,
    release_speed = 95, release_extension = 6.5, release_spin_rate = 2300,
    pfx_x = 0, pfx_z = 0, arm_angle = 45, plate_x = 0, plate_z = 2.5,
    sz_bot = 1.5, sz_top = 3.5, hb = 0, ivb = 0, in_zone = 1,
    pitch_team = "AAA",
    type         = c(rep("S", k), rep("B", bb), rep("X", bbe)),
    description  = c(rep("swinging_strike", k), rep("ball", bb),
                     rep("hit_into_play", bbe)),
    events       = c(rep("strikeout", k), rep("walk", bb), rep("field_out", bbe)),
    launch_speed = c(rep(NA_real_, k + bb), rep(100, hard), rep(80, bbe - hard)),
    estimated_woba_using_speedangle = 0.300, woba_denom = 1)
}
# Ten arms at 40 TBF, strikeouts running 2 to 20, so K-BB% is 5% to 50% in even
# steps and the two extremes are unambiguous.
lg_ad <- dplyr::bind_rows(lapply(1:10, function(i)
  lg_arm(100L + i, tbf = 40, k = i * 2, bb = 0, hard = 10)))
lg_gl <- tibble::tibble(
  pitcher = 101:110, game_date = "2026-08-01", ip_outs = 60L,
  er = 1:10, h = 10L, bb = 3L, so = 20L, tbf = 40L, hr = 1L, hbp = 0L)
dts <- c("2026-08-01", "2026-08-01")
L <- results_league(lg_ad, lg_gl, dts, "All", 3.1)

cat(sprintf("  K-BB%% floor %.0f over %d qualified, ERA floor %.1f over %d\n",
            L$k_bb$floor, L$k_bb$n, L$era$floor, L$era$n))
expect("every arm clears the TBF floor at this size", L$k_bb$n, 10L)

ctx_for <- function(id) {
  one <- dplyr::filter(lg_ad, pitcher == id)
  sc  <- results_statcast(one, "All")
  g   <- results_gamelog(lg_gl, id, dts, 3.1)
  results_context(sc, g, L)
}
fill_of <- function(ctx, m) ctx$fill[ctx$metric == m]
pct_of  <- function(ctx, m) ctx$pctile[ctx$metric == m]

best <- ctx_for(110L)   # most strikeouts, fewest earned runs
worst <- ctx_for(101L)  # fewest strikeouts, but ALSO the lowest ERA
cat(sprintf("  best K-BB%% pctile %3.0f, worst %3.0f | ERA pctile best %3.0f worst %3.0f\n",
            pct_of(best, "k_bb"), pct_of(worst, "k_bb"),
            pct_of(best, "era"), pct_of(worst, "era")))

# Direction, the least visible bug this feature can have. K-BB% is high-is-good
# and ERA is low-is-good, so the arm at the TOP of the K-BB% league and the arm
# at the BOTTOM of the ERA league must both come out red.
red  <- function(hex) { r <- grDevices::col2rgb(hex); r[1] > r[3] }
expect("the best K-BB% fills red",  red(fill_of(best,  "k_bb")), TRUE)
expect("the worst K-BB% fills blue", red(fill_of(worst, "k_bb")), FALSE)
expect("the LOWEST ERA fills red, because low is good",
       red(fill_of(ctx_for(101L), "era")), TRUE)
expect("the highest ERA fills blue", red(fill_of(ctx_for(110L), "era")), FALSE)

cat("\n=== the floor scales with the window, and greys below it ===\n")
# Same arms, spread across 20 days instead of one. Each pitcher now has 40 TBF
# either way, but the league median denominator is what moves the floor, so a
# thin arm added below it must be excluded from the reference AND left unfilled.
thin <- lg_arm(199L, tbf = 8, k = 4, bb = 0, hard = 2)
L2 <- results_league(dplyr::bind_rows(lg_ad, thin), lg_gl, dts, "All", 3.1)
cat(sprintf("  floor %.0f, qualified %d of 11 arms\n", L2$k_bb$floor, L2$k_bb$n))
expect("the 8-TBF arm is outside the reference", L2$k_bb$n, 10L)
sc_thin <- results_statcast(thin, "All")
ctx_thin <- results_context(sc_thin, list(have = FALSE, games = 0L), L2)
expect("and its own K-BB% is left unfilled",
       fill_of(ctx_thin, "k_bb"), PCTILE_UNFILLED)
expect("with no percentile at all", is.na(pct_of(ctx_thin, "k_bb")), TRUE)

# The floor is half the median denominator, so halving everyone's TBF must halve
# it. A fixed floor passes every other check in this file.
half <- dplyr::bind_rows(lapply(1:10, function(i)
  lg_arm(200L + i, tbf = 20, k = i, bb = 0, hard = 5)))
L3 <- results_league(half, lg_gl, dts, "All", 3.1)
cat(sprintf("  40-TBF league floor %.0f, 20-TBF league floor %.0f\n",
            L$k_bb$floor, L3$k_bb$floor))
expect("the floor tracks the population it is drawn from",
       L3$k_bb$floor < L$k_bb$floor, TRUE)
expect("and the absolute minimum takes over on a tiny one",
       results_league(lg_arm(300L, tbf = 4, k = 2, bb = 0, hard = 1),
                      lg_gl, dts, "All", 3.1)$k_bb$floor,
       RESULTS_METRIC_SPEC$min_denom[RESULTS_METRIC_SPEC$metric == "k_bb"])

cat("\n=== the panel renders with and without context ===\n")
plain  <- html(results_statcast(dplyr::filter(lg_ad, pitcher == 110L), "All"),
               results_gamelog(lg_gl, 110L, dts, 3.1), dts, "All", "2026-08-01")
shaded <- as.character(results_panel(
  results_statcast(dplyr::filter(lg_ad, pitcher == 110L), "All"),
  results_gamelog(lg_gl, 110L, dts, 3.1), dts, "All", "2026-08-01", ctx = best))
expect("ctx = NULL still renders, so every old caller is unaffected",
       grepl("rp-cells", plain), TRUE)
expect("and paints nothing", grepl("rp-filled", plain), FALSE)
expect("with context, cells carry a background", grepl("rp-filled", shaded), TRUE)
expect("TBF is never filled: it is the sample, not a result",
       grepl('rp-cell rp-filled[^>]*>[^<]*<div class="rp-val">40<', shaded), FALSE)

cat("\n", strrep("-", 58), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("PHASE 6 RESULTS: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
