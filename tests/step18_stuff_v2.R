# Step 18: Stuff+ v2 in the app. Each check can fail the bug it targets.
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

# 1. Parity: the app's full-season overall grade equals the stuff-plus 2026
#    leaderboard for the same pitcher and pitch, on 25 sampled rows. Fails if
#    the scale, the sums or the 50/50 rule drift from the model's own board
fx <- read_csv("tests/fixtures/stuff_v2_parity.csv", show_col_types = FALSE)
app_vals <- vapply(seq_len(nrow(fx)), function(i) {
  g <- stuff_for(sv, fx$pitcher[i], season, "All")
  g$stuff_plus[g$pitch_type == fx$pitch_type[i]]
}, numeric(1))
expect("app grade equals the model's leaderboard on 25 sampled pitches", app_vals, fx$stuff_plus)

# 2. The window is honored: a grade over one month differs from the season
#    for at least some pitches. Fails if the date filter is ignored
mis <- 694819   # Misiorowski, a full season of starts
expect("a one-month window changes the grades",
       identical(stuff_for(sv, mis, c("2026-04-01", "2026-04-30"), "All"),
                 stuff_for(sv, mis, season, "All")), FALSE)

# 3. The batter side is honored: vs RHH and vs LHH are not the same table.
#    Fails if hand is ignored or s_side is wired to s_both
expect("vs RHH and vs LHH differ",
       identical(stuff_for(sv, mis, season, "R")$stuff_plus,
                 stuff_for(sv, mis, season, "L")$stuff_plus), FALSE)

# 4. Contract and empties: two columns; an unknown pitcher and a window with no
#    games give zero rows, not an error
expect("contract is pitch_type, stuff_plus", names(stuff_for(sv, mis, season, "All")),
       c("pitch_type", "stuff_plus"))
expect("unknown pitcher gives zero rows", nrow(stuff_for(sv, 1L, season, "All")), 0L)
expect("window with no games gives zero rows",
       nrow(stuff_for(sv, mis, c("2026-12-01", "2026-12-31"), "All")), 0L)
expect("missing grade file blanks the column instead of failing",
       nrow(suppressWarnings(stuff_for(load_stuff_v2("nope.csv"), mis, season, "All"))), 0L)

cat(if (fails == 0) "\nSTEP 18 STUFF V2: PASS\n" else sprintf("\nSTEP 18 STUFF V2: FAIL (%d)\n", fails))
if (fails > 0) quit(status = 1)
