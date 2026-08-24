# step7_search.R
#
# The pitch trait search, at the function level. Everything here runs on a
# LITERAL fixture rather than on a slice of app_data: the store grows every
# night, and a check built on "the pitcher who happens to sit at 95.0 today"
# stops exercising its boundary the moment he throws again. See CLAUDE.md entry
# 5, the fixture that moves with the mutation.
#
# Coverage, verified by mutation on the committed tree rather than assumed.
# Caught, each by a named failure and not by a stack trace:
#
#   p_throws dropped from the group_by          -> the grain gate
#   velo lower bound made strict                -> the 95.0 boundary row
#   the pitch floor ignored                     -> the 5-pitch arm appears
#   HB normalised to arm side                   -> the lefty flips to +15
#   slider bounds rounded inward                -> the hardest arm unreachable
#   every row given row 1's denominators        -> the small sample reads exact
#
# NOT caught, and worth knowing. "Hands pooled into one row" is not reachable at
# this grain: the aggregate groups by pitcher, and a pitcher has one hand, so
# dropping p_throws removes the COLUMN rather than merging any rows. The
# realistic version of that bug is arm-side normalisation, which is mutation
# four, and that is what the HB expectations actually guard.
#
# The trap this file exists for is the two hands. `p_throws` is the pitcher and
# `stand` is the batter, both narrow a search, and swapping them produces a
# plausible table rather than an error. A one-handed fixture cannot catch that,
# so the fixture holds both hands and both sides.
suppressMessages({library(dplyr); library(tidyr); library(purrr); library(forcats)
                  library(ggplot2); library(gt); library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- character()
expect <- function(l, got, want) if (!identical(got, want))
  fails <<- c(fails, sprintf("%s: got %s, wanted %s", l, deparse(got), deparse(want)))

# One pitcher's worth of identical pitches. Traits are exact, so every boundary
# below is a written-out number and not a quantile of live data.
arm <- function(id, name, throws, pt, k, velo, ivb, hb, stand = "R",
                spin = 2300, ext = 6.5, desc = "foul", iz = 1, wd = NA_real_,
                team = "AAA") {
  tibble::tibble(
    pitcher = id, player_name = name, p_throws = throws, pitch_type = pt,
    stand = rep(stand, k), release_speed = velo, ivb = ivb, hb = hb,
    release_spin_rate = spin, release_extension = ext, pitch_team = rep(team, k),
    description = rep(desc, k), in_zone = iz, woba_denom = wd,
    estimated_woba_using_speedangle = NA_real_)
}

# 94.9 / 95.0 / 95.1 against a 95.0 floor, and two hands that must not pool:
# a righty at HB +15 and a lefty at HB -15 average to zero if anything ever
# groups them together, and zero is a perfectly plausible straight four-seam.
fx <- bind_rows(
  arm(1L, "Under, Al",  "R", "FF", 60, 94.9, 17, 15),
  arm(2L, "Exact, Ed",  "R", "FF", 60, 95.0, 17, 15),
  arm(3L, "Over, Ollie","R", "FF", 60, 95.1, 17, 15),
  arm(4L, "Lefty, Lou", "L", "FF", 60, 95.0, 17, -15),
  arm(5L, "Thin, Theo", "R", "FF",  5, 95.0, 17, 15),
  # Faces only lefties, so the batter-side filter must remove him and the
  # pitcher-hand filter must not.
  arm(6L, "Vs L, Vic",  "R", "FF", 60, 95.0, 17, 15, stand = "L")
)

pool <- search_aggregate(fx, "All")

# Asserted before anything consumes the pool. Dropping p_throws from the
# group_by used to reach search_filter as NULL == "R", which dies with a stack
# trace and stops the file before the remaining checks run. CLAUDE.md entry 7:
# some invariants can only be violated by an error, and the check's job is to
# OWN the error rather than die on it.
cat("=== the pool carries the grain the rest of the file assumes ===\n")
expect("the pool keeps the pitcher hand", "p_throws" %in% names(pool), TRUE)
expect("the pool keeps the pitch type",   "pitch_type" %in% names(pool), TRUE)
expect("the pool names its denominators the way resolve_cell looks them up",
       all(c("pitches", "swings", "oz", "pa") %in% names(pool)), TRUE)

# A hard gate, not a soft expectation. Every check below indexes the pool by
# these columns, so a wrong grain does not produce a wrong answer, it produces a
# stack trace three checks later that hides everything after it. Failing here
# names the cause and stops.
if (length(fails)) {
  cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n")
  cat("STEP 7 SEARCH: FAIL\n"); quit(status = 1)
}

cat("\n=== the pool is one row per pitcher per pitch type ===\n")
cat(sprintf("  %d pitchers in, %d rows out\n", n_distinct(fx$pitcher), nrow(pool)))
expect("one row per pitcher", nrow(pool), 6L)
expect("every row carries its own pitch count",
       sort(pool$pitches), c(5L, 60L, 60L, 60L, 60L, 60L))

cat("\n=== hands are never pooled ===\n")
hb <- setNames(round(pool$hb, 1), pool$pitcher)
cat(sprintf("  righty HB %.1f, lefty HB %.1f\n", hb[["2"]], hb[["4"]]))
expect("the righty keeps arm-side positive", unname(hb[["2"]]), 15)
expect("the lefty keeps arm-side negative",  unname(hb[["4"]]), -15)
# The mutation this catches is dropping p_throws from the group_by, which would
# average +15 and -15 into a straight four-seam nobody threw.
expect("no row sits at zero HB", any(round(pool$hb, 1) == 0), FALSE)

cat("\n=== the velocity boundary is inclusive at both ends ===\n")
hit <- search_filter(pool, "R", "FF", bounds = list(velo = c(95.0, 96.0)), min_pitches = 25)
cat("  95.0 to 96.0 returns:", paste(sort(hit$player_name), collapse = ", "), "\n")
expect("94.9 is out, 95.0 and 95.1 are in, and so is Vic",
       sort(hit$pitcher), c(2L, 3L, 6L))
expect("the lefty is not in a righty query", 4L %in% hit$pitcher, FALSE)

cat("\n=== the two hands are different controls ===\n")
lefty <- search_filter(pool, "L", "FF", min_pitches = 25)
expect("p_throws = L returns the lefty alone", lefty$pitcher, 4L)
# stand is applied when the pool is built, not when it is filtered.
pool_vsL <- search_aggregate(fx, "L")
vsL <- search_filter(pool_vsL, "R", "FF", min_pitches = 25)
cat("  vs LHH, righties returned:", paste(sort(vsL$player_name), collapse = ", "), "\n")
expect("vs LHH leaves only the pitcher who faced lefties", vsL$pitcher, 6L)
# Swapping p_throws for stand anywhere would make these two agree.
expect("the batter side and the pitcher hand do not select the same pitchers",
       identical(sort(hit$pitcher), sort(vsL$pitcher)), FALSE)

cat("\n=== the sample floor bites, and it is an argument ===\n")
at25 <- search_filter(pool, "R", "FF", bounds = list(velo = c(95.0, 96.0)), min_pitches = 25)
at5  <- search_filter(pool, "R", "FF", bounds = list(velo = c(95.0, 96.0)), min_pitches = 5)
cat(sprintf("  floor 25: %d pitchers, floor 5: %d\n", nrow(at25), nrow(at5)))
expect("the 5-pitch arm is absent at a floor of 25", 5L %in% at25$pitcher, FALSE)
expect("the 5-pitch arm is present at a floor of 5", 5L %in% at5$pitcher, TRUE)

cat("\n=== sliders are seeded from the data, rounded outward ===\n")
rg <- search_ranges(pool, "R", "FF", min_pitches = 25)
v  <- rg[rg$trait == "velo", ]
cat(sprintf("  velo spans %.1f to %.1f over %d pitchers\n", v$lo, v$hi, v$n))
# 94.9 to 95.1 rounded outward at a 0.1 step. Rounding inward would make the
# hardest thrower unselectable, which is the opposite of the point.
expect("the low end reaches the slowest arm",  v$lo <= 94.9, TRUE)
expect("the high end reaches the hardest arm", v$hi >= 95.1, TRUE)
# 4, not 5: search_ranges() seeds one hand at a time, so the lefty is not in
# this population. The first version of this line said 5 and the check caught
# it, which is the half of "can it pass correct code" that usually goes untested.
expect("the count is the RIGHTIES above the floor, not the rows or both hands",
       v$n, 4L)

cat("\n=== sorting: descending first, ascending on the second click ===\n")
# Sort runs over the whole match set, so it has to be right before anything caps
# the table at 50. Character columns are the trap: order(-x) is fine on velocity
# and an error on PITCHER, and both are clickable headers.
srt <- function(col, d) paste(search_filter(pool, "R", "FF", min_pitches = 5,
                                            sort_by = col, desc = d)$player_name,
                              collapse = " ")
cat("  velo desc:", srt("velo", TRUE), "\n")
cat("  velo asc :", srt("velo", FALSE), "\n")
expect("velocity descending puts 95.1 first",
       search_filter(pool, "R", "FF", min_pitches = 5, sort_by = "velo")$pitcher[1], 3L)
expect("velocity ascending puts 94.9 first",
       search_filter(pool, "R", "FF", min_pitches = 5, sort_by = "velo",
                     desc = FALSE)$pitcher[1], 1L)
expect("the ends swap between the two clicks",
       c(search_filter(pool, "R", "FF", min_pitches = 5, sort_by = "velo")$pitcher[1],
         search_filter(pool, "R", "FF", min_pitches = 5, sort_by = "velo",
                       desc = FALSE)$pitcher[1]), c(3L, 1L))
# Three arms sit at exactly 95.0, and they keep their relative order in BOTH
# directions. That is what a stable sort buys, and it is the reason a second
# click is not a strict rev() of the first: reversing ties would shuffle rows
# the reader has no reason to see move. The first version of this check asserted
# a strict reverse and was wrong about the code rather than the other way round.
tie_order <- function(d) {
  o <- search_filter(pool, "R", "FF", min_pitches = 5, sort_by = "velo", desc = d)
  o$pitcher[o$velo == 95.0]
}
expect("ties hold their order whichever way the column is sorted",
       tie_order(TRUE), tie_order(FALSE))
# order(-x) does not return a wrong order on a name, it raises, and an uncaught
# raise here would stop the file and hide every check below it. Caught and turned
# into a marker so it fails the comparison it belongs to. CLAUDE.md entry 7.
sorted_first <- function(col, d) tryCatch(
  search_filter(pool, "R", "FF", min_pitches = 5, sort_by = col, desc = d)$player_name[1],
  error = function(e) paste("ERROR:", conditionMessage(e)))
expect("a text column sorts rather than erroring", sorted_first("player_name", FALSE), "Exact, Ed")
expect("and descends the other way",                sorted_first("player_name", TRUE),  "Vs L, Vic")
# NA belongs at the bottom of BOTH directions: a missing reading is not the
# smallest value, it is the absence of one.
ns <- search_aggregate(bind_rows(fx, arm(13L, "Nospin, Ned", "R", "FF", 60, 95, 17, 15,
                                         spin = NA_real_)), "All")
for (d in c(TRUE, FALSE)) {
  o <- search_filter(ns, "R", "FF", min_pitches = 5, sort_by = "spin", desc = d)
  expect(paste("the missing spin sorts last, desc =", d), o$pitcher[nrow(o)], 13L)
}

cat("\n=== the cap note names the column actually sorted on ===\n")
# It read "the 50 with the most pitches" whatever the header click had done, so
# the moment anyone sorted by whiff rate the note described a different table.
# Found by driving the app, not by the checks: every assertion about the cap was
# about the ROWS, and none of them read the sentence.
ref2  <- load_league_ref()
many  <- bind_rows(lapply(20:80, function(i)
  arm(i, paste0("Arm ", i), "R", "FF", 60, 90 + i / 20, 17, 15)))
mpool <- search_aggregate(many, "All")
note_for <- function(col, d) {
  res <- search_filter(mpool, "R", "FF", min_pitches = 5, sort_by = col, desc = d)
  h <- as.character(gt::as_raw_html(
    search_gt(utils::head(res, 5), "FF", "R", "All", max_rows = 5, n_total = nrow(res),
              sort_by = col, desc = d)))
  sub(".*(Showing the [^<]*matches)\\..*", "\\1", h)
}
cat("  ", note_for("velo", TRUE), "\n  ", note_for("whiff_pct", FALSE), "\n", sep = "")
expect("the note names the sorted column, descending",
       grepl("top 5 by VELO", note_for("velo", TRUE)), TRUE)
expect("and says bottom when the second click flips it",
       grepl("bottom 5 by WHIFF%", note_for("whiff_pct", FALSE)), TRUE)
expect("the total is the match count, not the capped count",
       grepl("of 61 matches", note_for("velo", TRUE)), TRUE)

cat("\n=== the team filter, and the traded pitcher it exists for ===\n")
# 96 pitchers threw for more than one club in 2026, so the question is not
# whether a traded pitcher shows up but WHICH pitches he is judged on. Team is
# filtered before the aggregate, beside the batter side, so a club query reads
# as the shape he threw while he was there. 40 pitches at 100 for BBB and 60 at
# 92 for CCC: no average of the two is 96, and none of the three answers here
# can be reached by mistake.
traded <- bind_rows(
  arm(11L, "Moved, Mo", "R", "FF", 40, 100.0, 17, 15, team = "BBB"),
  arm(11L, "Moved, Mo", "R", "FF", 60,  92.0, 17, 15, team = "CCC"),
  arm(12L, "Stayed, Stu","R", "FF", 60, 95.0, 17, 15, team = "BBB"))

all_t <- search_aggregate(traded, "All", "All")
bbb   <- search_aggregate(traded, "All", "BBB")
ccc   <- search_aggregate(traded, "All", "CCC")
mo <- function(p) p[p$pitcher == 11L, ]
cat(sprintf("  Mo: all teams %d pitches at %.1f, BBB %d at %.1f, CCC %d at %.1f\n",
            mo(all_t)$pitches, mo(all_t)$velo, mo(bbb)$pitches, mo(bbb)$velo,
            mo(ccc)$pitches, mo(ccc)$velo))
expect("all teams keeps ONE row per pitcher, not one per club", nrow(all_t), 2L)
expect("all teams averages both stints",  round(mo(all_t)$velo, 1), 95.2)
expect("a club query uses only that club's pitches", mo(bbb)$pitches, 40L)
expect("and only that club's shape",      round(mo(bbb)$velo, 1), 100.0)
expect("the other club, likewise",        round(mo(ccc)$velo, 1), 92.0)
expect("a pitcher who never threw there is absent", 12L %in% ccc$pitcher, FALSE)
expect("the label names every club in the window", mo(all_t)$team, "BBB/CCC")
expect("and just the one when filtered",  mo(bbb)$team, "BBB")
# The floor applies AFTER the team filter, which is the whole point: Mo's 40
# pitches for BBB are what a 50-pitch floor should be judging, not his 100.
expect("the floor sees the club's pitch count, not the season's",
       nrow(search_filter(bbb, "R", "FF", min_pitches = 50)), 1L)

cat("\n=== a missing reading is dropped, and SAID ===\n")
# A trait with no reading cannot satisfy a range, so the row goes even with every
# slider at full width. That arithmetic is right and the silence is not: the
# reader narrowed nothing and got fewer pitchers than the population. Live case
# on 2026-08-23 was Ty Blach, three pitch types, no spin.
no_spin <- bind_rows(fx, arm(10L, "Nospin, Ned", "R", "FF", 60, 95.0, 17, 15,
                             spin = NA_real_))
npool <- search_aggregate(no_spin, "All")
rg_ns <- search_ranges(npool, "R", "FF", 25)
full  <- setNames(lapply(rg_ns$trait, function(tr)
  c(rg_ns$lo[rg_ns$trait == tr], rg_ns$hi[rg_ns$trait == tr])), rg_ns$trait)
got   <- search_filter(npool, "R", "FF", full, 25)
miss  <- search_missing(npool, "R", "FF", 25)
cat(sprintf("  full-range search returns %d, missing-trait rows %d on %s\n",
            nrow(got), sum(miss$n), paste(miss$trait, collapse = ", ")))
expect("the no-spin arm is absent from a full-range search", 10L %in% got$pitcher, FALSE)
expect("and the page can say how many were dropped", sum(miss$n), 1L)
expect("and which reading was missing", miss$trait, "spin")
expect("nothing is reported missing when nothing is", nrow(search_missing(pool, "R", "FF", 25)), 0L)

cat("\n=== slider labels print each trait at its own precision ===\n")
# The label carries the range because the tick grid had to go: ionRangeSlider
# prettifies its grid interval and then always labels the max, so a span that is
# not a multiple of the interval collides, and RHP four-seams printed 100.6 over
# 101.6. Shiny exposes no control over the grid count.
#
# One sprintf cannot serve spin and extension at once, which is the whole reason
# fmt_bound() exists, so both are pinned here with literals.
cat(sprintf("  velo %s | spin %s | ext %s\n",
            fmt_bound(101.6, 1), fmt_bound(2725, 0), fmt_bound(6.5, 1)))
expect("velocity keeps one decimal",        fmt_bound(101.6, 1), "101.6")
expect("spin is whole and comma grouped",   fmt_bound(2725, 0),  "2,725")
expect("extension keeps one decimal",       fmt_bound(6.5, 1),   "6.5")
expect("a negative HB bound keeps its sign", fmt_bound(-17, 1),  "-17.0")

cat("\n=== an empty query is an empty table, not an error ===\n")
none <- search_filter(pool, "R", "FF", bounds = list(velo = c(101, 105)), min_pitches = 25)
expect("no match returns zero rows", nrow(none), 0L)
expect("and keeps its columns", "player_name" %in% names(none), TRUE)

cat("\n=== no swings gives NA, never NaN ===\n")
# Every pitch in the fixture is a foul, which IS a swing, so build one arm that
# never got offered at.
taken <- search_aggregate(bind_rows(fx, arm(7L, "Taken, Ted", "R", "FF", 60, 95, 17, 15,
                                            desc = "called_strike")), "All")
tt <- taken[taken$pitcher == 7, ]
cat(sprintf("  0 swings: whiff_pct is %s\n", format(tt$whiff_pct)))
expect("whiff% is NA and not NaN", is.na(tt$whiff_pct) && !is.nan(tt$whiff_pct), TRUE)

cat("\n=== league context resolves PER ROW, not per pitch type ===\n")
# The trap resolve_search() exists for. resolve_table() matches its denominator
# frame by pitch type, and every row here shares one, so it would hand every
# pitcher the first row's denominators. Two arms with the same whiff rate and
# wildly different sample sizes must NOT resolve to the same state.
ref  <- load_league_ref()
wide <- bind_rows(
  arm(8L, "Many, Mo",  "R", "FF", 400, 95, 17, 15, desc = "swinging_strike"),
  arm(9L, "Few, Fred", "R", "FF",  30, 95, 17, 15, desc = "swinging_strike"))
wpool <- search_aggregate(wide, "All")
wtbl  <- search_filter(wpool, "R", "FF", min_pitches = 5)
ctx   <- resolve_search(wtbl, ref, "R", "All", "FF")
wc <- ctx$cells[ctx$cells$column == "whiff_pct", ]
wc <- wc[order(match(wtbl$pitcher[wc$row], c(8L, 9L))), ]
cat(sprintf("  400 swings -> %s, 30 swings -> %s\n", wc$state[1], wc$state[2]))
expect("the big sample clears the whiff floor",   wc$state[1], "exact")
expect("the small sample is held below floor",    wc$state[2], "below_floor")
expect("every column resolved", sort(unique(ctx$cells$column)),
       sort(names(SEARCH_METRIC_COLS)))

cat("\n", strrep("-", 58), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 7 SEARCH: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
