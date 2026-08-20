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

cat("\n", strrep("-", 58), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("PHASE 6 RESULTS: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
