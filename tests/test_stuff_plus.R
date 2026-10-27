# Stuff+ v2 grades: parity with the model's leaderboard, window and batter side honored,
# per-group scale, and the two-column contract.
suppressMessages({ library(dplyr); library(readr); library(tibble) })
source("R/stuff.R")
fails <- 0
expect <- function(label, got, want) {
  ok <- isTRUE(all.equal(got, want))
  cat(if (ok) "  ok  " else "  FAIL", label, "\n")
  if (!ok) { cat("       got:", format(got), " want:", format(want), "\n"); fails <<- fails + 1 }
}
sv <- load_stuff_v2()
season <- c("2026-03-25", "2026-09-27")

# Parity with the stuff-plus 2026 leaderboard on 25 sampled pitcher-pitch types
fx <- read_csv("tests/fixtures/stuff_v2_parity.csv", show_col_types = FALSE)
app_vals <- vapply(seq_len(nrow(fx)), function(i) {
  g <- stuff_for(sv, fx$pitcher[i], season, "All")
  g$stuff_plus[g$pitch_type == fx$pitch_type[i]]
}, numeric(1))
expect("app grade equals the model's leaderboard on 25 sampled pitcher-pitch types", app_vals, fx$stuff_plus)

# A one-month window differs from the season
mis <- 694819   # Misiorowski, a full season of starts
expect("a one-month window changes the grades",
       identical(stuff_for(sv, mis, c("2026-04-01", "2026-04-30"), "All"),
                 stuff_for(sv, mis, season, "All")), FALSE)

# vs RHH and vs LHH differ
expect("vs RHH and vs LHH differ",
       identical(stuff_for(sv, mis, season, "R")$stuff_plus,
                 stuff_for(sv, mis, season, "L")$stuff_plus), FALSE)

# Each group centers near 100 (one league-wide scale would put fastballs near 85)
ids <- unique(sv$daily$pitcher)
season_grades <- bind_rows(lapply(ids, function(id) {
  g <- stuff_for(sv, id, season, "All")
  n <- sv$daily |> filter(pitcher == id) |> summarise(n = sum(n), .by = pitch_type)
  inner_join(g, n, by = "pitch_type")
})) |> filter(n >= 250) |> mutate(group = stuff_group(pitch_type))
meds <- season_grades |> summarise(m = median(stuff_plus), .by = group)
expect("every group's median grade sits within 5 of 100", all(abs(meds$m - 100) <= 5), TRUE)

# Two columns; an unknown pitcher or empty window gives zero rows
expect("contract is pitch_type, stuff_plus", names(stuff_for(sv, mis, season, "All")),
       c("pitch_type", "stuff_plus"))
expect("unknown pitcher gives zero rows", nrow(stuff_for(sv, 1L, season, "All")), 0L)
expect("window with no games gives zero rows",
       nrow(stuff_for(sv, mis, c("2026-12-01", "2026-12-31"), "All")), 0L)
expect("missing grade file blanks the column instead of failing",
       nrow(suppressWarnings(stuff_for(load_stuff_v2("nope.csv"), mis, season, "All"))), 0L)

cat(if (fails == 0) "\nSTUFF+: PASS\n" else sprintf("\nSTUFF+: FAIL (%d)\n", fails))
if (fails > 0) quit(status = 1)
