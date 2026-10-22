# Precomputed league pool: a pool hit equals the live computation, a pool built for
# other data or code is refused, and a non-preset window misses the pool.
suppressMessages({library(dplyr);library(tidyr);library(purrr);library(forcats)
                  library(ggplot2);library(gt);library(readr);library(tibble)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- character(0)
expect <- function(what, got, want) {
  ok <- isTRUE(all.equal(got, want))
  cat(sprintf("  %-62s %s\n", what, if (ok) "ok" else
    paste0("FAIL got ", paste(format(got), collapse = ","),
           " wanted ", paste(format(want), collapse = ","))))
  if (!ok) fails <<- c(fails, what)
}
same_frame <- function(a, b) isTRUE(all.equal(as.data.frame(a), as.data.frame(b),
                                              check.attributes = FALSE))

app_data <- readRDS("data/app_data.rds")
heights  <- suppressWarnings(load_pitcher_heights())
t0 <- Sys.time()
pool <- build_league_pool(app_data, heights)
cat(sprintf("built in %.1f s\n", as.numeric(Sys.time() - t0, units = "secs")))

h <- season_halves(app_data)
wins <- Filter(Negate(is.null), list(full = h$full, first = h$first, second = h$second))
cat("\n1. Every pooled table equals the pre-pool live path\n")
expect("one search table per window x batter side",
       length(pool$search), length(wins) * length(POOL_BATTER_SIDES))
expect("one release table per window", length(pool$release), length(wins))

for (w in names(wins)) {
  from <- as.character(wins[[w]][1]); to <- as.character(wins[[w]][2])
  # The live path, a filtered copy of the window
  win <- filter(app_data, game_date >= from, game_date <= to)
  for (side in POOL_BATTER_SIDES) {
    hit <- pool_search(pool, from, to, side)
    expect(sprintf("%-6s side %-3s Search table", w, side),
           same_frame(hit, search_aggregate(win, side, "All")), TRUE)
    expect(sprintf("%-6s side %-3s Context shape", w, side),
           same_frame(pitch_shape_finish(hit, 1), pitch_shape(win, side, 1)), TRUE)
  }
  prof <- pool_release(pool, from, to)
  expect(sprintf("%-6s release profile at the Context floor of 100", w),
         same_frame(prof[prof$n >= 100, , drop = FALSE],
                    pitcher_release_profile(win, heights, min_pitches = 100)), TRUE)
}
# The three sides differ: a swapped key can't pass
full <- as.character(wins$full)
expect("L and R tables differ (a swapped key would be caught)",
       same_frame(pool_search(pool, full[1], full[2], "L"),
                  pool_search(pool, full[1], full[2], "R")), FALSE)

cat("\n2. A pool for other data or other code is refused\n")
tmp <- tempfile(fileext = ".rds"); saveRDS(pool, tmp)
expect("fresh pool loads", !is.null(suppressMessages(load_league_pool(app_data, tmp))), TRUE)
bad <- pool; bad$meta$through <- "2026-01-01"; saveRDS(bad, tmp)
expect("other data dates -> NULL", is.null(suppressWarnings(load_league_pool(app_data, tmp))), TRUE)
bad <- pool; bad$meta$n_rows <- pool$meta$n_rows + 1L; saveRDS(bad, tmp)
expect("other row count -> NULL", is.null(suppressWarnings(load_league_pool(app_data, tmp))), TRUE)
bad <- pool; bad$meta$fingerprint[1] <- "0"; saveRDS(bad, tmp)
expect("other code -> NULL", is.null(suppressWarnings(load_league_pool(app_data, tmp))), TRUE)
expect("missing file -> NULL",
       is.null(suppressWarnings(load_league_pool(app_data, tempfile()))), TRUE)
unlink(tmp)

cat("\n3. Windows no button sets miss the pool\n")
expect("full season minus one day misses",
       is.null(pool_search(pool, full[1], as.character(as.Date(full[2]) - 1), "All")), TRUE)
expect("NULL pool misses", is.null(pool_search(NULL, full[1], full[2], "All")), TRUE)

cat("\n", strrep("-", 64), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("LEAGUE POOL: ", if (!length(fails)) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (!length(fails)) 0 else 1)
