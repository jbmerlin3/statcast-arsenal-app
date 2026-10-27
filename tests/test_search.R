# Pitch trait search on a literal fixture with both pitcher hands and both batter
# sides: grain, slider bounds, filters, sorting, traded pitchers and missing traits.
suppressMessages({library(dplyr); library(tidyr); library(purrr); library(forcats)
                  library(ggplot2); library(gt); library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- character()
expect <- function(l, got, want) if (!identical(got, want))
  fails <<- c(fails, sprintf("%s: got %s, wanted %s", l, deparse(got), deparse(want)))

# One pitcher of identical pitches; every trait default keeps the column contract
arm <- function(id, name, throws, pt, k, velo, ivb, hb, stand = "R",
                spin = 2300, ext = 6.5, rel_ht = 5.9, desc = "foul", iz = 1,
                wd = NA_real_, team = "AAA", vaa = -4.5, rel_x = -1.8) {
  tibble::tibble(
    pitcher = id, player_name = name, p_throws = throws, pitch_type = pt,
    stand = rep(stand, k), release_speed = velo, ivb = ivb, hb = hb,
    release_spin_rate = spin, release_extension = ext, release_pos_z = rel_ht,
    vaa = vaa, release_pos_x = rel_x, pitch_team = rep(team, k),
    description = rep(desc, k), in_zone = iz, woba_denom = wd,
    estimated_woba_using_speedangle = NA_real_)
}

# 94.9 / 95.0 / 95.1 against a 95.0 floor; a righty at HB +15 and a lefty at -15
# would average to zero if hands ever pooled
fx <- bind_rows(
  arm(1L, "Under, Al",  "R", "FF", 60, 94.9, 17, 15),
  arm(2L, "Exact, Ed",  "R", "FF", 60, 95.0, 17, 15),
  arm(3L, "Over, Ollie","R", "FF", 60, 95.1, 17, 15),
  arm(4L, "Lefty, Lou", "L", "FF", 60, 95.0, 17, -15),
  arm(5L, "Thin, Theo", "R", "FF",  5, 95.0, 17, 15),
  # Faces only lefties
  arm(6L, "Vs L, Vic",  "R", "FF", 60, 95.0, 17, 15, stand = "L")
)

pool <- search_aggregate(fx, "All")

# Check the pool's grain first and stop if it's wrong
cat("=== the pool carries the grain the rest of the file assumes ===\n")
expect("the pool keeps the pitcher hand", "p_throws" %in% names(pool), TRUE)
expect("the pool keeps the pitch type",   "pitch_type" %in% names(pool), TRUE)
expect("the pool names its denominators the way resolve_cell looks them up",
       all(c("pitches", "swings", "oz", "pa") %in% names(pool)), TRUE)

if (length(fails)) {
  cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n")
  cat("SEARCH: FAIL\n"); quit(status = 1)
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
# Pooled hands would average to HB 0
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
# Batter side is applied when the pool is built
pool_vsL <- search_aggregate(fx, "L")
vsL <- search_filter(pool_vsL, "R", "FF", min_pitches = 25)
cat("  vs LHH, righties returned:", paste(sort(vsL$player_name), collapse = ", "), "\n")
expect("vs LHH leaves only the pitcher who faced lefties", vsL$pitcher, 6L)
# Swapping p_throws and stand would make these agree
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
# Bounds round outward
expect("the low end reaches the slowest arm",  v$lo <= 94.9, TRUE)
expect("the high end reaches the hardest arm", v$hi >= 95.1, TRUE)
# Ranges are per pitcher hand: the lefty isn't counted
expect("the count is the RIGHTIES above the floor, not the rows or both hands",
       v$n, 4L)

cat("\n=== sorting: descending first, ascending on the second click ===\n")
# Sorting runs over the whole match set; character columns must sort too
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
# Ties keep their order in both directions (stable sort)
tie_order <- function(d) {
  o <- search_filter(pool, "R", "FF", min_pitches = 5, sort_by = "velo", desc = d)
  o$pitcher[o$velo == 95.0]
}
expect("ties hold their order whichever way the column is sorted",
       tie_order(TRUE), tie_order(FALSE))
# Catch a sort error and fail the comparison instead of stopping the file
sorted_first <- function(col, d) tryCatch(
  search_filter(pool, "R", "FF", min_pitches = 5, sort_by = col, desc = d)$player_name[1],
  error = function(e) paste("ERROR:", conditionMessage(e)))
expect("a text column sorts rather than erroring", sorted_first("player_name", FALSE), "Exact, Ed")
expect("and descends the other way",                sorted_first("player_name", TRUE),  "Vs L, Vic")
# NA sorts last in both directions
ns <- search_aggregate(bind_rows(fx, arm(13L, "Nospin, Ned", "R", "FF", 60, 95, 17, 15,
                                         spin = NA_real_)), "All")
for (d in c(TRUE, FALSE)) {
  o <- search_filter(ns, "R", "FF", min_pitches = 5, sort_by = "spin", desc = d)
  expect(paste("the missing spin sorts last, desc =", d), o$pitcher[nrow(o)], 13L)
}

cat("\n=== the cap note names the column actually sorted on ===\n")
# The cap note names the active sort column
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
# Traded pitcher: 40 pitches at 100 for BBB and 60 at 92 for CCC
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
# The pitch floor applies after the team filter
expect("the floor sees the club's pitch count, not the season's",
       nrow(search_filter(bbb, "R", "FF", min_pitches = 50)), 1L)

cat("\n=== a missing reading is dropped, and SAID ===\n")
# A missing trait can't match any range; the count must say so
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

cat("\n=== an empty query is an empty table, not an error ===\n")
none <- search_filter(pool, "R", "FF", bounds = list(velo = c(101, 105)), min_pitches = 25)
expect("no match returns zero rows", nrow(none), 0L)
expect("and keeps its columns", "player_name" %in% names(none), TRUE)

cat("\n=== no swings gives NA, never NaN ===\n")
# An arm that never got a swing
taken <- search_aggregate(bind_rows(fx, arm(7L, "Taken, Ted", "R", "FF", 60, 95, 17, 15,
                                            desc = "called_strike")), "All")
tt <- taken[taken$pitcher == 7, ]
cat(sprintf("  0 swings: whiff_pct is %s\n", format(tt$whiff_pct)))
expect("whiff% is NA and not NaN", is.na(tt$whiff_pct) && !is.nan(tt$whiff_pct), TRUE)

cat("\n", strrep("-", 58), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("SEARCH: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
