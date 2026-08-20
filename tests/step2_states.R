# Step 2 acceptance. The four states, resolved through resolve_cell() only.
#
# Coverage, verified by mutation on the committed tree rather than assumed.
# Caught: direction ignored so a low metric fills as high; a fallback cell
# resolving exact; a below-floor cell resolving no_reference or exact; a
# no_reference cell losing its marker; below_floor losing its n; and
# resolve_column growing a branch resolve_cell does not have.
#
# NOT caught, and worth knowing. Nudging a floor from 50 to 49 passes, because
# no cell here sits on a boundary: the below-floor case is 13 swings against 50
# and the exact cases clear it by a wide margin. Same shape of gap as the
# KDE_MIN_N one recorded in scripts/phase1_check.R. A cell chosen to sit at
# floor and floor-1 would close it.
#
# The four-way greyscale check is a backstop, not the primary guard. Every
# mutation above was caught by a sharper single expectation first; the four-way
# check fired only when a mutation collapsed two states outright.
#
#
# Denominators are computed here the same way build_league_ref.R computes them,
# because arsenal_table() does not return them until step 3.
suppressMessages({library(dplyr);library(tidyr);library(purrr);library(forcats)
                  library(ggplot2);library(gt);library(readr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

ad  <- readRDS("data/app_data.rds")
ref <- readRDS("data/league_ref.rds")

denoms <- function(d) {
  d |> group_by(pitch_type) |>
    summarise(pitches = n(),
              swings  = sum(description %in% swing_only),
              oz      = sum(in_zone == 0),
              pa      = sum(woba_denom, na.rm = TRUE), .groups = "drop")
}
metric_value <- function(d, metric) switch(metric,
  ivb       = mean(d$ivb, na.rm = TRUE),
  spin      = mean(d$release_spin_rate, na.rm = TRUE),
  strike_pct= mean(d$type %in% c("S", "X")) * 100,
  csw_pct   = mean(d$description %in% c("called_strike", whiff_desc)) * 100,
  zone_pct  = mean(d$in_zone, na.rm = TRUE) * 100,
  chase_pct = sum(d$in_zone == 0 & d$description %in% swing_only) / sum(d$in_zone == 0) * 100,
  whiff_pct = sum(d$description %in% whiff_desc) / sum(d$description %in% swing_only) * 100,
  xwoba     = sum(d$estimated_woba_using_speedangle * d$woba_denom, na.rm = TRUE) /
                sum(d$woba_denom, na.rm = TRUE),
  velo      = mean(d$release_speed, na.rm = TRUE),
  hb        = mean(d$hb, na.rm = TRUE),
  stop("no value helper for ", metric))

cell_for <- function(mlb_id, pt, hand, metric, dates = NULL) {
  pl <- build_pitch_level(ad, mlb_id)
  if (!is.null(dates)) pl <- filter(pl, game_date >= dates[1], game_date <= dates[2])
  d  <- filter(pl, stand == hand, pitch_type == pt)
  resolve_cell(ref, metric_value(d, metric), metric, pt,
               unique(d$p_throws)[1], hand, "All Counts",
               as.list(denoms(d)[1, -1]))
}

show <- function(lbl, c) cat(sprintf(
  "%-34s state=%-13s fill=%-8s text=%-8s %s/%-6s mark=%-7s n=%-5s pctile=%-4s nref=%-4s grain=%s\n",
  lbl, c$state, c$fill, c$text_color, c$font_style, c$font_weight,
  paste0("'", c$marker, "'"), c$n, c$pctile, c$n_pitchers, c$grain))

fails <- character()
expect <- function(lbl, got, want) if (!identical(got, want))
  fails <<- c(fails, sprintf("%s: got %s, wanted %s", lbl, deparse(got), deparse(want)))

cat("=== THE FALLBACK CASE: Shane Baz, KC, whiff% vs RHH ===\n")
baz <- cell_for(669358, "KC", "R", "whiff_pct")
show("Baz KC whiff% vs RHH", baz)
cat("  state note: ", baz$state_note, "\n", sep = "")
expect("baz state",      baz$state,      "fallback")
expect("baz grain",      baz$grain,      "pitch_type x p_throws x count")
expect("baz n_pitchers", baz$n_pitchers, 22L)
expect("baz marker",     baz$marker,     "†")
expect("baz weight",     baz$font_weight, "bold")
expect("baz has_ref",    baz$has_ref,    TRUE)

cat("\n=== THE OTHER THREE STATES ===\n")
# Below floor uses a real, non-zero denominator rather than an empty pitch type.
# A 0-swing row resolves below_floor for the wrong reason and would pass even if
# the floor comparison were broken.
ex <- cell_for(669358, "FF", "R", "velo");   show("exact        Baz FF velo vs RHH", ex)
# FF has a thick reference, 172 pitchers, so this isolates the pitcher's own
# thin sample from any question about the league side.
bf <- cell_for(669358, "FF", "R", "whiff_pct", c("2026-08-04", "2026-08-17"))
                                             show("below floor  Baz FF whiff%, 2wk", bf)
# xwOBA is thin on BOTH sides for a secondary pitch: 7 PA against a 50 floor,
# and the league cell itself tops out at 17 pitchers. Below floor wins.
xw <- cell_for(669358, "KC", "R", "xwoba", c("2026-08-04", "2026-08-17"))
                                             show("both thin    Baz KC xwOBA, 2wk", xw)
# Waldron's knuckleball is the only pitch type with no reference row at any
# grain, so it is the only way to reach no_reference on live data. The same cell
# under whiff% is below floor AND no reference at once, which is the precedence
# case: 29 swings against a 50 floor, and no KN reference either way.
nr <- cell_for(663362, "KN", "R", "velo");   show("no reference Waldron KN velo", nr)
pr <- cell_for(663362, "KN", "R", "whiff_pct"); show("precedence   Waldron KN whiff%", pr)
hb <- cell_for(669358, "FF", "R", "hb");     show("neutral      Baz FF hb vs RHH", hb)
cat("  below floor note: ", bf$state_note, "\n", sep = "")
cat("  no reference note: ", nr$state_note, "\n", sep = "")

expect("exact state",  ex$state, "exact")
expect("exact grain",  ex$grain, NA_character_)
expect("exact marker", ex$marker, "")
expect("exact weight", ex$font_weight, "normal")

expect("below floor state",  bf$state, "below_floor")
expect("below floor n is real, not zero", bf$n > 0, TRUE)
expect("below floor is under its floor",  bf$n < bf$floor, TRUE)
expect("below floor marker", bf$marker, paste0(" (", bf$n, ")"))
expect("below floor fill",   bf$fill,  PCTILE_UNFILLED)
expect("below floor italic", bf$font_style, "italic")
expect("below floor had a reference all along", bf$has_ref, TRUE)

expect("no reference state",  nr$state, "no_reference")
expect("no reference clears its floor", nr$n >= nr$floor, TRUE)
expect("no reference marker", nr$marker, "\u2021")
expect("no reference fill",   nr$fill,  PCTILE_UNFILLED)
expect("no reference has no n in the marker", grepl("(", nr$marker, fixed = TRUE), FALSE)
expect("no reference has_ref", nr$has_ref, FALSE)

# Precedence: below floor wins, and has_ref still records the missing reference.
expect("precedence resolves below floor", pr$state, "below_floor")
expect("precedence keeps the diagnostic",  pr$has_ref, FALSE)
expect("xwoba both-thin resolves below floor", xw$state, "below_floor")
expect("xwoba both-thin keeps the diagnostic", xw$has_ref, FALSE)

expect("hb neutral fill", hb$fill, pctile_fill(hb$pctile, "neutral"))
expect("hb is not on the diverging scale", hb$fill == pctile_fill(hb$pctile, "high"), FALSE)
expect("hb carries its note", is.character(hb$metric_note), TRUE)
expect("velo carries no note", ex$metric_note, NA_character_)

cat("\n=== the four states are separable without hue ===\n")
four <- list(exact = ex, fallback = baz, below_floor = bf, no_reference = nr)
key  <- vapply(four, function(c) paste(c$fill != PCTILE_UNFILLED, c$font_weight,
                                       c$font_style, gsub("[0-9]+", "N", c$marker)), character(1))
for (nm in names(key)) cat(sprintf("  %-13s %s\n", nm, key[[nm]]))
expect("all four differ on non-hue channels", length(unique(key)), 4L)

cat("\n=== DIRECTION: xwOBA inverts, whiff% does not ===\n")
cat("  high  p=90 ->", pctile_fill(90, "high"), " p=10 ->", pctile_fill(10, "high"), "\n")
cat("  low   p=90 ->", pctile_fill(90, "low"),  " p=10 ->", pctile_fill(10, "low"),  "\n")
expect("low is the mirror of high", pctile_fill(90, "low"), pctile_fill(10, "high"))
expect("xwoba direction from spec", METRIC_SPEC$direction[METRIC_SPEC$metric == "xwoba"], "low")
expect("unknown direction stops",
       tryCatch({pctile_fill(50, "up"); "no error"}, error = function(e) "stopped"), "stopped")

cat("\n=== every resolved fill is a usable colour ===\n")
# M3 last turn set below_floor to filled and was caught by a crash inside
# gt::cell_fill(color = NA), because a below-floor cell's percentile can be NA.
# A crash proves gt is intolerant, not that the guard works. This asserts the
# invariant gt was accidentally enforcing: fill is always a real colour, so any
# state that starts producing NA is named here instead of aborting a render.
all_fills <- unlist(lapply(unname(ARSENAL_METRIC_COLS), function(m) {
  pl2 <- build_pitch_level(ad, 669358) |> filter(stand == "R")
  d2  <- denoms(pl2)
  vapply(levels(droplevels(pl2$pitch_type)), function(pt) {
    dd <- filter(pl2, pitch_type == pt)
    resolve_cell(ref, tryCatch(metric_value(dd, m), error = function(e) NA_real_),
                 m, pt, "R", "R", "All Counts", denoms(dd))$fill
  }, character(1))
}))
cat("  ", length(all_fills), " fills, ", sum(is.na(all_fills)), " NA\n", sep = "")
expect("no resolved fill is NA", sum(is.na(all_fills)), 0L)
expect("every fill is a 6-digit hex", all(grepl("^#[0-9A-Fa-f]{6}$", all_fills)), TRUE)

cat("\n=== FLOOR BOUNDARY: floor and floor-1 on a rate metric ===\n")
# The value and the league reference are real: Baz FF whiff% vs RHH against a
# 172-pitcher cell. Only the denominator is pinned, to 50 and 49, which is the
# only way to sit exactly on the boundary. No real pitch type is guaranteed to
# land on 50 swings, and one that did would drift on the next data pull.
#
# The denominators are written as LITERALS, not as `fl` and `fl - 1`, and that
# is the whole point of this block. A test that reads the same constant the code
# reads slides along with it: nudge the floor to 49 and a `swings = fl - 1` cell
# becomes a 48-swing cell, still below floor, still passing. It would test the
# comparison operator and never the parameter. Verified by mutation, that
# earlier version passed a 50 -> 49 nudge.
#
# So changing a floor deliberately now requires editing this block. That is
# intended: the floors come from a measured resampling study recorded in
# theme.R, not from taste, and a silent edit to one is exactly what this guards.
fl   <- METRIC_SPEC$floor[METRIC_SPEC$metric == "whiff_pct"]
pl_r <- build_pitch_level(ad, 669358) |> filter(stand == "R", pitch_type == "FF")
v_ff <- metric_value(pl_r, "whiff_pct")
at_floor <- resolve_cell(ref, v_ff, "whiff_pct", "FF", "R", "R", "All Counts",
                         list(pitches = 500, swings = 50, oz = 200, pa = 100))
below_1  <- resolve_cell(ref, v_ff, "whiff_pct", "FF", "R", "R", "All Counts",
                         list(pitches = 500, swings = 49, oz = 200, pa = 100))
show("at floor     swings=50", at_floor)
show("floor - 1    swings=49", below_1)

# Pins the parameter itself, independently of the two cells above.
expect("the rate floor is still the measured 50", fl, 50)

# n == floor must be eligible. This pins the comparison operator: a >= quietly
# changed to > flips the at-floor cell and nothing else in the file.
expect("at floor is eligible",     at_floor$state, "exact")
expect("at floor is filled",       at_floor$fill != PCTILE_UNFILLED, TRUE)
expect("floor - 1 is below floor", below_1$state,  "below_floor")
expect("floor - 1 is unfilled",    below_1$fill,   PCTILE_UNFILLED)
expect("floor - 1 reports its n",  below_1$marker, " (49)")
expect("the two sides of the boundary differ", at_floor$state == below_1$state, FALSE)

cat("\n=== resolve_column matches resolve_cell, cell by cell ===\n")
pl  <- build_pitch_level(ad, 669358) |> filter(stand == "R")
dn  <- denoms(pl)
vals <- vapply(dn$pitch_type, function(pt) metric_value(filter(pl, pitch_type == pt), "whiff_pct"), numeric(1))
col <- resolve_column(ref, vals, "whiff_pct", dn$pitch_type, "R", "R", "All Counts", dn[, -1])
print(col[, c("pitch_type","state","fill","marker","n","pctile","n_pitchers")])
scalar <- vapply(seq_along(vals), function(i)
  resolve_cell(ref, vals[[i]], "whiff_pct", as.character(dn$pitch_type[[i]]),
               "R", "R", "All Counts", as.list(dn[i, -1]))$state, character(1))
expect("column equals scalar", col$state, scalar)
expect("column keeps one row per pitch type", nrow(col), length(vals))

cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 2: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
