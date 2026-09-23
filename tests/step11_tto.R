# step11_tto.R
#
#   Rscript tests/step11_tto.R
#
# The Usage tab's times-through toggle, added 2026-09-23. Three parts, and the
# third is the first check in this suite that runs the SERVER, not a function
# the server calls. Nothing did before, and three bugs shipped through that gap
# in September.
#
#  A. The rules, on literal fixtures. When the toggle shows, which views are
#     disabled, and what a pick the window cannot honour falls back to. Every
#     count here is a literal, never read from the code under test (CLAUDE.md,
#     checks that did not discriminate, entry 5).
#  B. The table, on real data. The three views add back up to Overall, every
#     view prints the same pitch rows, and Overall is identical to the function
#     as it was before this change, including on one-day windows where a count
#     bucket is empty and the new code takes a different path.
#  C. The server, through shiny::testServer, on four real outings: a pure
#     reliever, a starter chased early, a long relief day, and a full starter.
suppressMessages({library(dplyr); library(purrr); library(tidyr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- character(0)
expect <- function(what, got, want) {
  ok <- isTRUE(all.equal(got, want))
  cat(sprintf("  %-62s %s\n", what, if (ok) "ok" else
    paste0("FAIL got ", paste(format(got), collapse = ","),
           " wanted ", paste(format(want), collapse = ","))))
  if (!ok) fails <<- c(fails, what)
}
# Owns a crash rather than dying on it, so one dead call fails its own line
# and every check after it still runs. See CLAUDE.md, entry 7.
try_or <- function(expr, marker = "<error>") tryCatch(expr, error = function(e) {
  cat("    error: ", conditionMessage(e), "\n", sep = ""); marker })

fx <- function(tto, stand = rep("R", length(tto))) {
  data.frame(n_thruorder_pitcher = as.integer(tto), stand = stand,
             stringsAsFactors = FALSE)
}
html <- function(tag) if (is.null(tag)) "" else as.character(tag)
n_match <- function(pattern, x) lengths(regmatches(x, gregexpr(pattern, x)))
# The one <input> tag for a value, so a check reads its attributes whatever
# order they are written in. Matching 'value="3" disabled' as one string broke
# the day an aria-label went between them.
pill_tag <- function(h, v) {
  tags <- regmatches(h, gregexpr("<input[^>]*>", h))[[1]]
  hit <- tags[grepl(sprintf('value="%s"', v), tags, fixed = TRUE)]
  if (length(hit) == 1) hit else ""
}


cat("=== A. rules, literal fixtures ===\n")
expect("4th and later fold into 3rd+", tto_bucket(c(1, 2, 3, 4, 5)),
       c("1", "2", "3", "3", "3"))

# A reliever who never saw a hitter twice, and a starter chased in the 1st, look
# the same to the rules, and that is the point: nothing asks about role.
for (case in list(list("reliever, 15 pitches, all 1st", fx(rep(1, 15))),
                  list("starter chased in the 1st, 28 pitches", fx(rep(1, 28))))) {
  k <- tto_counts(case[[2]], "All")
  expect(paste(case[[1]], ": toggle hidden"), tto_visible(k), FALSE)
  expect(paste(case[[1]], ": no markup at all"), tto_toggle_ui(k), NULL)
  expect(paste(case[[1]], ": a stale 2nd falls back"), tto_effective("2", k), "All")
}

bulk <- fx(c(rep(1, 40), rep(2, 9)))
k <- tto_counts(bulk, "All")
expect("long relief: counts", unname(k), c(49, 40, 9, 0))
expect("long relief: toggle shows", tto_visible(k), TRUE)
expect("long relief: 2nd is honoured", tto_effective("2", k), "2")
expect("long relief: 3rd+ has none, falls back", tto_effective("3", k), "All")
h <- html(tto_toggle_ui(k, "2"))
expect("long relief: four radios, one group", n_match('name="tto"', h), 4L)
expect("long relief: exactly one disabled", n_match("disabled", h), 1L)
expect("long relief: the disabled one is 3rd+",
       grepl("disabled", pill_tag(h, "3"), fixed = TRUE), TRUE)
expect("long relief: 2nd is the checked one",
       grepl("checked", pill_tag(h, "2"), fixed = TRUE), TRUE)
expect("the group carries the id Shiny's radio binding reads",
       grepl('id="tto" class="shiny-input-radiogroup', h, fixed = TRUE), TRUE)

full <- fx(c(rep(1, 30), rep(2, 25), rep(3, 18), rep(4, 4)))
k <- tto_counts(full, "All")
expect("full start: 4th time folds into the 3rd+ count", k[["3"]], 22L)
expect("full start: the views add up to Overall", sum(k[c("1", "2", "3")]), k[["All"]])
expect("full start: nothing disabled", n_match("disabled", html(tto_toggle_ui(k))), 0L)

# He saw a righty twice and no lefty twice. The count table follows the batter
# side, so the toggle does too.
side <- fx(c(1, 1, 1, 2, 2, 1, 1), c("R", "R", "L", "R", "R", "L", "L"))
expect("2nd time only vs RHH: hidden for vs LHH",
       tto_visible(tto_counts(side, "L")), FALSE)
expect("2nd time only vs RHH: shown for vs RHH",
       tto_visible(tto_counts(side, "R")), TRUE)

k <- tto_counts(full, "All")
expect("a NULL pick is Overall", tto_effective(NULL, k), "All")
expect("an NA pick is Overall", tto_effective(NA_character_, k), "All")
expect("an unknown pick is Overall", tto_effective("7", k), "All")


# A pitch he throws only the first time through. The real starters below throw
# everything every time through, so on them alone the "same rows" check could
# not fail: cutting before droplevels() or dropping .drop = FALSE both passed it
# when mutated on 2026-09-23. This fixture is what makes those two mutations die.
drop_sl <- data.frame(
  pitch_type = factor(c("FF", "FF", "SL", "FF", "FF", "CH"), levels = c("FF", "SL", "CH")),
  stand = "R", balls = c(0L, 1L, 0L, 0L, 0L, 1L), strikes = c(0L, 1L, 2L, 0L, 2L, 1L),
  n_thruorder_pitcher = c(1L, 1L, 1L, 2L, 2L, 2L))
v2 <- count_usage_tbl(drop_sl, "R", "2")
expect("SL thrown only 1st time: its row survives the 2nd view",
       as.character(v2$pitch_type), c("FF", "SL", "CH"))
expect("SL thrown only 1st time: 0.0 in every 2nd-view bucket",
       unname(unlist(v2[v2$pitch_type == "SL", -1])), rep(0, 6))
expect("2nd view, All Counts: FF 2 of 3, CH 1 of 3",
       v2[["All Counts"]], c(66.7, 0, 33.3))


# One pitch to one side reads 100%, and its label sits outside the bar end. The
# old fixed axis stopped at 105 and clipped it to "100'". Found on the page
# 2026-09-23, Barnett's 3rd+ view: every pitch to righties was a sweeper.
one_side <- data.frame(pitch_type = factor(c("ST", "ST", "FF"), levels = c("FF", "ST")),
                       stand = c("R", "R", "L"))
# ggplot pads 5% of the span past each limit, so the old 105 axis draws to
# 115.5 and a widened one to 137.5; the thresholds sit between the two.
xr <- ggplot2::ggplot_build(plot_usage(one_side))$layout$panel_params[[1]]$x.range
expect("a 100% bar leaves room for its label", xr[2] >= 130, TRUE)
xr <- ggplot2::ggplot_build(plot_usage(data.frame(
  pitch_type = factor(c("FF", "SL", "FF", "SL"), levels = c("FF", "SL")),
  stand = c("R", "R", "L", "L"))))$layout$panel_params[[1]]$x.range
expect("a 50/50 chart keeps the old 105 axis", xr[2] < 120, TRUE)


cat("\n=== B. the table, real outings ===\n")
ad <- readRDS("data/app_data.rds")
win <- function(id, from = "2026-01-01", to = "2026-12-31") {
  shape_arsenal(filter(ad, pitcher == id, game_date >= from, game_date <= to))
}
alc <- win(645261)   # Sandy Alcantara, season

# The weighted sum of the three views must reproduce Overall, cell by cell.
# Each view's share is rounded to 0.1 and so is Overall's, so the most the two
# can honestly disagree by is 0.1. A dropped 4th time through, or a view cut
# before the batter side, lands well outside that.
cnt <- function(d) paste(d$balls, d$strikes, sep = "-")
for (hd in c("All", "R", "L")) {
  side_d <- if (hd == "All") alc else filter(alc, stand == hd)
  views  <- lapply(c("1", "2", "3"), function(t) count_usage_tbl(alc, hd, t))
  over   <- count_usage_tbl(alc, hd)
  worst  <- 0
  for (b in names(COUNT_BUCKETS)) {
    inb <- if (is.null(COUNT_BUCKETS[[b]])) rep(TRUE, nrow(side_d)) else cnt(side_d) %in% COUNT_BUCKETS[[b]]
    n_all <- sum(inb)
    n_t   <- sapply(c("1", "2", "3"), function(t) sum(inb & tto_bucket(side_d$n_thruorder_pitcher) == t))
    rebuilt <- Reduce(`+`, Map(function(v, n) v[[b]] * n / n_all, views, n_t))
    worst <- max(worst, abs(rebuilt - over[[b]]))
  }
  expect(sprintf("Alcantara %s: the three views rebuild Overall (worst %.3f)", hd, worst),
         worst <= 0.1 + 1e-9, TRUE)
  expect(sprintf("Alcantara %s: every view prints Overall's rows", hd),
         all(sapply(views, function(v) identical(v$pitch_type, over$pitch_type))), TRUE)
}

# count_usage_tbl() as it stood before this change, verbatim, so Overall is
# compared against the real previous behaviour rather than against itself.
old_count_usage_tbl <- function(df, hand) {
  if (hand != "All") df <- filter(df, stand == hand)
  df <- df |> mutate(pitch_type = droplevels(pitch_type))
  buckets <- list("All Counts"=NULL, "Early Count"=c("0-0","0-1","1-0"),
                  "Pitcher Ahead"=c("0-1","0-2","1-2","2-2"),
                  "Pitcher Behind"=c("1-0","2-0","3-0","2-1","3-1"),
                  "Pre Two Strikes"=c("0-0","0-1","1-0","1-1","2-1","3-1"),
                  "Two Strikes"=c("0-2","1-2","2-2","3-2"))
  base <- df |> mutate(cnt = paste(balls, strikes, sep = "-"))
  bucket_usage <- function(counts) {
    d <- if (is.null(counts)) base else filter(base, cnt %in% counts)
    d |> count(pitch_type, name = "n") |> mutate(pct = round(n / sum(n) * 100, 1)) |>
      select(pitch_type, pct)
  }
  imap(buckets, \(counts, nm) bucket_usage(counts) |> rename(!!nm := pct)) |>
    reduce(full_join, by = "pitch_type") |>
    arrange(pitch_type) |>
    mutate(across(-pitch_type, \(x) replace_na(x, 0)))
}
outings <- list(
  "Alcantara season"      = alc,
  "Harris season"         = win(663687),
  "Wheeler 08-02"         = win(554430, "2026-08-02", "2026-08-02"),
  "Barnett 08-17"         = win(686930, "2026-08-17", "2026-08-17"),
  "Skenes 03-26"          = win(694973, "2026-03-26", "2026-03-26"))
empties <- 0
for (nm in names(outings)) for (hd in c("All", "R", "L")) {
  d <- outings[[nm]]
  if (hd != "All" && !any(d$stand == hd)) next
  new <- count_usage_tbl(d, hd)
  empties <- empties + sum(colSums(as.matrix(new[, -1])) == 0)
  expect(sprintf("%s %s: Overall identical to the old table", nm, hd),
         identical(new, old_count_usage_tbl(d, hd)), TRUE)
}
# The .drop = FALSE path only differs from the old one when a bucket is empty,
# so the identity check above proves nothing unless at least one case had one.
expect("identity cases include an empty count bucket", empties > 0, TRUE)


cat("\n=== C. the server, shiny::testServer ===\n")
ids <- c(harris = "663687", wheeler = "554430", barnett = "686930", cameron = "702070")
res <- try_or(shiny::testServer(shiny::shinyAppDir("."), {
  season <- as.Date(c("2026-03-01", "2026-11-01"))
  one    <- function(d) as.Date(c(d, d))
  shows  <- function() nzchar(paste(unlist(output$tto_toggle$html), collapse = ""))

  session$setInputs(pitcher = ids[["harris"]], dates = season, hand = "All")
  expect("Harris, season: no toggle", shows(), FALSE)
  expect("Harris, season: view is Overall", tto(), "All")

  session$setInputs(pitcher = ids[["wheeler"]], dates = one("2026-08-02"))
  expect("Wheeler 08-02, 32-pitch start: no toggle", shows(), FALSE)

  session$setInputs(pitcher = ids[["barnett"]], dates = one("2026-08-17"))
  expect("Barnett 08-17, long relief: toggle shows", shows(), TRUE)
  expect("Barnett 08-17: 3rd+ is disabled",
         grepl("disabled", pill_tag(output$tto_toggle$html, "3"), fixed = TRUE), TRUE)
  session$setInputs(tto = "2")
  expect("Barnett 08-17: 2nd applies", tto(), "2")
  expect("Barnett 08-17: 2nd time through is 45 pitches",
         nrow(filter_tto(pitcher_data(), tto())), 45L)
  expect("Barnett 08-17: the table says which view it is",
         grepl("2nd time through", output$usage_table$html, fixed = TRUE), TRUE)
  session$setInputs(tto = "3")
  expect("Barnett 08-17: a 3rd+ pick he has none of falls back", tto(), "All")

  # The trap the tto() reactive exists for: the input keeps "2" after the
  # toggle disappears, and must not follow the reader to a reliever.
  session$setInputs(tto = "2")
  session$setInputs(pitcher = ids[["harris"]], dates = season)
  expect("stale 2nd does not follow the reader to Harris", tto(), "All")
  expect("Harris: the table is untitled by time through",
         grepl("time through", output$usage_table$html, fixed = TRUE), FALSE)

  session$setInputs(pitcher = ids[["cameron"]], tto = "3")
  expect("Cameron, season: toggle shows, nothing disabled",
         shows() && !grepl("disabled", output$tto_toggle$html), TRUE)
  expect("Cameron, season: 3rd+ applies", tto(), "3")
}))
expect("testServer ran to the end", !identical(res, "<error>"), TRUE)


cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 11 TTO: ", if (!length(fails)) "PASS" else "FAIL", "\n", sep = "")
