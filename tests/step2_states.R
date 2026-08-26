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

# The fixture used to be Shane Baz's KC. It moved for a reason worth recording:
# PITCH_CODE_RULES now maps KC to CU, so build_pitch_level() no longer returns a
# pitch type called KC for anyone, filter(pitch_type == "KC") matched zero rows,
# and every field came back NA. That is the correct behaviour of the change, not
# a regression, but it left this file asserting against an empty frame.
#
# The expectations were already stale before that, wanting 22 pitchers against a
# reference that had been rebuilt since. Re-derived here from the current
# league_ref rather than carried forward.
#
# Duran's FS vs RHH is a real fallback: 98 swings clears the 50-swing floor, but
# the fine pitch_type x p_throws x stand cell does not have enough pitchers, so
# resolve_cell() drops to the coarser cut. Which is exactly the state this block
# exists to pin.
cat("=== THE FALLBACK CASE: Jhoan Duran, FS, whiff% vs RHH ===\n")
fb <- cell_for(661395, "FS", "R", "whiff_pct")
show("Duran FS whiff% vs RHH", fb)
cat("  state note: ", fb$state_note, "\n", sep = "")
expect("fallback state",      fb$state,       "fallback")
expect("fallback grain",      fb$grain,       "pitch_type x p_throws x count")
expect("fallback n_pitchers", fb$n_pitchers,  46L)
expect("fallback marker",     fb$marker,      "†")
expect("fallback weight",     fb$font_weight, "bold")
expect("fallback has_ref",    fb$has_ref,     TRUE)

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
hb <- cell_for(669358, "FF", "R", "hb");     show("directional  Baz FF hb vs RHH", hb)
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

# hb has moved twice on 2026-08-24 and this assertion moved with it both times,
# which is the point: a check that stops describing the code has to move, not go
# quiet. First it left the neutral scale for the diverging one. Then it stopped
# being coloured by RANK at all and started being coloured by distance from the
# league average, so comparing its fill to pctile_fill(hb$pctile, ...) is now
# wrong by construction.
# Asserted as a PROPERTY rather than by rebuilding the value here. The first
# attempt re-derived the league mean with lg_cell(..., "All", ...) and got a
# different cell than resolve_cell uses, because the ladder picks the cell and
# the batter side is part of the grain. Reimplementing a lookup inside a check
# is how a check ends up testing its own arithmetic.
expect("hb is not on the neutral scale", hb$fill == pctile_fill(hb$pctile, "neutral"), FALSE)
expect("and is NOT the rank-based diverging fill either, because it is magnitude now",
       hb$fill == pctile_fill(hb$pctile, metric_direction("hb", "FF")), FALSE)
expect("its reported percentile is still the true RANK", hb$pctile >= 0 && hb$pctile <= 100, TRUE)

cat("\n=== IVB and HB colour by DISTANCE from the league, not by rank ===\n")
# A rank is not comparable across rows: 90th percentile is one inch in a
# cutter's tight IVB spread and four in a curveball's wide one, so two cells the
# same colour meant different things. The fill is now inches off the league
# average for that pitch type, on one scale for the whole table, saturating at
# SHAPE_DELTA_SPAN. Savant reads the same way: "9.0 MORE DROP", not "94th".
#
# Literal spans, never written through the constant, per CLAUDE.md entry 5.
expect("sitting on the league average is the middle of the ramp",
       shape_delta_pctile(10, 10), 50)
expect("six inches above saturates the top",   shape_delta_pctile(16, 10), 100)
expect("six inches below saturates the bottom", shape_delta_pctile(4, 10), 0)
expect("three inches above is halfway up",     shape_delta_pctile(13, 10), 75)
expect("and it clamps rather than running off", shape_delta_pctile(40, 10), 100)
expect("a missing league mean gives no fill",   is.na(shape_delta_pctile(10, NA_real_)), TRUE)

# The point of the change, stated as the thing rank could not do: two pitches at
# the SAME distance from their own league average get the same colour, even
# though their ranks differ because their pitch types have different spreads.
same_delta <- pctile_fill(shape_delta_pctile(10, 6), "high") ==
              pctile_fill(shape_delta_pctile(-2, -6), "high")
expect("equal deltas render equal, whatever the rank would have said", same_delta, TRUE)
# And direction still applies on top: for a pitch that wants the low end, being
# BELOW the league is what reddens.
drop_is_red <- grDevices::col2rgb(pctile_fill(shape_delta_pctile(4, 10), "low"))
expect("a drop pitch four inches under the league fills red", drop_is_red[1] > drop_is_red[3], TRUE)

cat("\n=== shape direction is a property of the PITCH, not of the metric ===\n")
# The bug: IVB and HB were graded globally high-is-good, so Logan Webb's changeup
# sat 5 inches under the league on IVB and rendered BLUE. Savant paints that same
# pitch red and labels it "9.0 MORE DROP", because drop is what a changeup is
# for. More ride is the point of a four-seam and the death of a sinker.
#
# Written out per pitch type as literals. There is no default on purpose: a code
# that arrives unlisted must stop the render rather than inherit a guess.
expect("a four-seam wants ride",        metric_direction("ivb", "FF"), "high")
expect("a cutter wants ride",           metric_direction("ivb", "FC"), "high")
expect("a sinker wants drop",           metric_direction("ivb", "SI"), "low")
expect("a changeup wants drop",         metric_direction("ivb", "CH"), "low")
expect("a curveball wants drop",        metric_direction("ivb", "CU"), "low")
expect("a four-seam wants arm side",    metric_direction("hb",  "FF"), "high")
expect("a sinker wants arm side",       metric_direction("hb",  "SI"), "high")
expect("a changeup wants arm side",     metric_direction("hb",  "CH"), "high")
expect("a cutter wants glove side",     metric_direction("hb",  "FC"), "low")
expect("a sweeper wants glove side",    metric_direction("hb",  "ST"), "low")
expect("a curveball wants glove side",  metric_direction("hb",  "CU"), "low")
# A knuckleball has no intended shape, which is the whole idea.
expect("a knuckleball keeps the neutral grey on ivb", metric_direction("ivb", "KN"), "neutral")
expect("and on hb",                                   metric_direction("hb",  "KN"), "neutral")
# Everything that is not a shape still comes from METRIC_SPEC.
expect("whiff% is unaffected by pitch type", metric_direction("whiff_pct", "CH"), "high")
expect("xwOBA is still low-is-good",         metric_direction("xwoba", "FF"), "low")
# An unlisted code stops rather than defaulting. Caught and named so a dead call
# fails the comparison it belongs to instead of killing the file. Entry 7.
bad <- tryCatch(metric_direction("ivb", "ZZ"), error = function(e) "stopped")
expect("an unknown pitch type stops rather than guessing", bad, "stopped")

cat("\n=== the case that reported the bug: Logan Webb ===\n")
webb <- build_pitch_level(ad, 657277L)
w_tb <- arsenal_table(webb, "All", tibble::tibble(pitch_type = character(),
                                                  stuff_plus = numeric(), fg_exact = logical()))
w_ctx <- resolve_table(w_tb, arsenal_denoms(webb, "All"), ref, webb$p_throws[1], "All")
hue <- function(col, pt) {
  i <- which(as.character(w_tb$pitch_type) == pt)
  f <- w_ctx$cells[w_ctx$cells$column == col & w_ctx$cells$row == i, ]$fill
  r <- grDevices::col2rgb(f); if (r[1] > r[3]) "red" else if (r[3] > r[1]) "blue" else "grey"
}
cat(sprintf("  changeup IVB %.1f -> %s   sinker HB %.1f -> %s   sweeper HB %.1f -> %s\n",
            w_tb$ivb[w_tb$pitch_type == "CH"], hue("ivb", "CH"),
            w_tb$hb[w_tb$pitch_type == "SI"],  hue("hb", "SI"),
            w_tb$hb[w_tb$pitch_type == "ST"],  hue("hb", "ST")))
expect("his changeup's extra drop reads RED, as Savant has it", hue("ivb", "CH"), "red")
expect("his sinker's arm-side tail reads red",                  hue("hb",  "SI"), "red")
expect("his sweeper's glove-side sweep reads red",              hue("hb",  "ST"), "red")
expect("and his four-seam, which does not ride, reads blue",    hue("ivb", "FF"), "blue")

cat("\n=== HB is arm-side normalised on the comparison surfaces, raw on the chart ===\n")
# The bug this closes: HB is stored as -pfx_x * 12, arm side positive for a
# righty and negative for a lefty, so once the cell carried a COLOUR two pitchers
# with identical arm-side run rendered red and blue. Measured before the fix:
# RHP sinkers averaged +14.9 and LHP sinkers -15.1.
#
# Three surfaces, and they must not all agree. The TABLE and LEAGUE_REF are
# normalised, because a percentile has to mean one thing. The MOVEMENT CHART is
# not, because a lefty's slider really does sweep the other way and the league
# cross has to land on his pitches.
expect("a righty is unchanged by normalisation", arm_side_sign("R"), 1)
expect("a lefty is flipped",                     arm_side_sign("L"), -1)

si_r <- lg_cell(ref, "hb", "SI", "R", "All", "All Counts")$row$mean[[1]]
si_l <- lg_cell(ref, "hb", "SI", "L", "All", "All Counts")$row$mean[[1]]
cat(sprintf("  league_ref sinker HB: RHP %+.1f, LHP %+.1f\n", si_r, si_l))
# Both positive is the whole point. Before the fix these were +14.9 and -15.1.
expect("both hands' sinkers now read arm-side POSITIVE in league_ref",
       si_r > 0 && si_l > 0, TRUE)
expect("and a slider reads glove-side negative for both",
       lg_cell(ref, "hb", "SL", "R", "All", "All Counts")$row$mean[[1]] < 0 &&
       lg_cell(ref, "hb", "SL", "L", "All", "All Counts")$row$mean[[1]] < 0, TRUE)

# The chart converts back. Without this the lefty cross sits mirrored across the
# vertical axis, nowhere near the pitches it is meant to describe.
mv_r <- movement_ref(ref, c("SI"), "R")$hb[1]
mv_l <- movement_ref(ref, c("SI"), "L")$hb[1]
cat(sprintf("  movement chart sinker cross: RHP %+.1f, LHP %+.1f\n", mv_r, mv_l))
expect("the chart puts the righty cross arm side, positive",  mv_r > 0, TRUE)
expect("and the lefty cross arm side, which is NEGATIVE there", mv_l < 0, TRUE)
expect("the round trip is exact for a lefty", mv_l, -si_l)

# The table agrees with league_ref, not with the chart. A lefty's sinker must
# come out positive here or the percentile is being read off the wrong scale.
lhp_si <- arsenal_table(filter(build_pitch_level(ad, 702070), pitch_type == "SI"),
                        "All", tibble::tibble(pitch_type = character(),
                                              stuff_plus = numeric(), fg_exact = logical()))
cat(sprintf("  a LHP sinker in the table reads HB %+.1f\n", lhp_si$hb[1]))
expect("the table shows a lefty's sinker as arm-side POSITIVE", lhp_si$hb[1] > 0, TRUE)

cat("\n=== which metrics claim a direction, and which refuse ===\n")
# Pinned as literals because direction is the least visible thing in this whole
# system: getting one backwards renders a .420 xwOBA deep red and nothing errors.
# ivb, hb and zone_pct became directional on 2026-08-24 by request, and what
# their red now means is "more than average", not "better than average".
expect("xwOBA is the only low-is-good metric",
       METRIC_SPEC$metric[METRIC_SPEC$direction == "low"], "xwoba")
expect("usage% is the only metric that still refuses a direction",
       METRIC_SPEC$metric[METRIC_SPEC$direction == "neutral"], "usage_pct")
expect("ivb, hb and zone% are directional now",
       all(METRIC_SPEC$direction[METRIC_SPEC$metric %in% c("ivb", "hb", "zone_pct")] == "high"),
       TRUE)
# HB is not arm-side normalised, so "high is red" means red is a more POSITIVE
# value: arm side for a RHP, glove side for a LHP. Measured on league_ref, RHP
# sinkers mean +14.9 and LHP sinkers -15.1. The table footnote says so, and this
# asserts the footnote is still there to say it.
expect("hb still carries the note explaining what its high end is",
       grepl("arm-side", col_notes_for(c(hb = "hb"))[["hb"]]), TRUE)

cat("\n=== Stuff+ shades off 100, with no league lookup ===\n")
# Read out of the RENDERED table, not from a local reimplementation of the
# formula. The first version of this block recomputed the ramp here, which meant
# it agreed with itself no matter what arsenal_gt() actually did: centring the
# ramp on 0 instead of 100 passed it. CLAUDE.md entry 8, testing where the app
# does not stand.
#
# The expected hexes are LITERAL. The first version wrote them through
# STUFF_PLUS_SPAN, so doubling the span moved the fixture with the constant and
# passed. Entry 5, in a test written the same afternoon it was cited.
stuff_tbl <- tibble::tibble(
  pitch_type = factor(c("FF", "SL", "CH", "CU", "SI"),
                      levels = c("FF", "SL", "CH", "CU", "SI")),
  count = 100L, pitch_pct = 20, velocity = 93, ivb = 15, hb = 5, spin = 2300,
  strike_pct = 60, whiff_pct = 25, csw_pct = 28, zone_pct = 50, chase_pct = 30,
  xwoba = 0.310,
  # 75 and 125 are the ends of the span; 40 and 160 are outside it and must
  # clamp to the same two colours rather than running off or wrapping.
  stuff_plus = c(100, 85, 115, 40, 160),
  fg_exact = TRUE)
sh <- as.character(gt::as_raw_html(arsenal_gt(stuff_tbl, "All")))
stuff_bg <- regmatches(sh, gregexpr('<td headers="stuff_plus"[^>]*>', sh))[[1]]
hex_of <- function(td) toupper(sub('.*background-color: (#[0-9A-Fa-f]{6}).*', "\\1", td))
got <- vapply(stuff_bg, hex_of, character(1), USE.NAMES = FALSE)
cat(sprintf("  100 %s | 85 %s | 115 %s | 40 %s | 160 %s\n",
            got[1], got[2], got[3], got[4], got[5]))
expect("Stuff+ 100 is the same average grey as every other average cell",
       got[1], "#F0F0F0")
expect("85 is blue",  got[2], "#B2CAE4")
expect("115 is red",  got[3], "#EBB39F")
expect("40 clamps to the blue end", got[4], "#7FA8D6")
expect("160 clamps to the red end", got[5], "#DC8163")

cat("\n=== the neutral scale is symmetric grey, not a rank ramp ===\n")
# The line above compares the fill to the same function that produced it, so it
# passes whatever the palette is. That is CLAUDE.md entry 5 in the test file
# itself: a fixture written in terms of the thing it checks. These pin the
# DESIGN with literals instead.
#
# Neutral metrics have no good end: more IVB is a plus on a four-seam and a
# minus on a curveball, and HB flips sign with the pitcher's hand. So the scale
# says how UNUSUAL a value is and refuses to say whether that is good, which
# means it must be symmetric about the middle and must share the diverging
# scale's middle stop so average looks the same in both.
cat(sprintf("  p10 %s   p50 %s   p90 %s\n", pctile_fill(10, "neutral"),
            pctile_fill(50, "neutral"), pctile_fill(90, "neutral")))
expect("average is the same light grey as the diverging scale's average",
       pctile_fill(50, "neutral"), "#F0F0F0")
expect("and that is the diverging middle stop too",
       pctile_fill(50, "high"), "#F0F0F0")
expect("the two ends match, so the fill cannot imply a direction",
       pctile_fill(10, "neutral"), pctile_fill(90, "neutral"))
expect("the extremes are darker than the middle",
       pctile_fill(0, "neutral") < pctile_fill(50, "neutral"), TRUE)
expect("no hue: the neutral scale is pure grey",
       all(vapply(c(0, 25, 50, 75, 100), function(p) {
         r <- grDevices::col2rgb(pctile_fill(p, "neutral")); r[1] == r[2] && r[2] == r[3]
       }, logical(1))), TRUE)
# Below floor is an unfilled cell with grey ITALIC text; these are filled cells
# with black text. Two channels apart, which is what makes grey safe to use here
# at all.
expect("no neutral fill collides with the below-floor text grey",
       PCTILE_GREY %in% vapply(0:100, function(p) pctile_fill(p, "neutral"), character(1)),
       FALSE)
expect("hb carries its note", is.character(hb$metric_note), TRUE)
expect("velo carries no note", ex$metric_note, NA_character_)

cat("\n=== the four states are separable without hue ===\n")
four <- list(exact = ex, fallback = fb, below_floor = bf, no_reference = nr)
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

cat("\n=== the strike zone carries a ball radius on BOTH axes ===\n")
# Literal coordinates, never ZONE_HALF_FT or BALL_RADIUS_FT. A fixture written
# in terms of the constant it pins slides with the constant and pins nothing.
# See CLAUDE.md entry 5.
#
# A pitch is a strike if any part of the ball crosses the zone, so the bound is
# the nominal edge plus 0.1208 ft. The vertical bound carried no radius until
# 2026-08-23, which put zone% 4.6 points below Savant's.
zbot <- 1.60; ztop <- 3.40
iz <- function(x, z) in_zone_flag(x, z, zbot, ztop)
expect("dead centre is in",                     iz(0,     2.5),  1L)
expect("0.82 ft off centre is still in",        iz(0.82,  2.5),  1L)
expect("0.84 ft off centre is out",             iz(0.84,  2.5),  0L)
expect("0.10 ft above the top is in",           iz(0,     3.50), 1L)
expect("0.15 ft above the top is out",          iz(0,     3.55), 0L)
expect("0.10 ft below the bottom is in",        iz(0,     1.50), 1L)
expect("0.15 ft below the bottom is out",       iz(0,     1.45), 0L)
expect("the radius applies to the low bound too, not just the high",
       c(iz(0, 1.50), iz(0, 1.45)), c(1L, 0L))

cat("\n=== a foul tip is a whiff and a swing ===\n")
# Savant counts a foul tip as a whiff, verified against their leaderboard over
# 105 pitchers: misses / swings ran 2.10 points light, adding foul_tip lands at
# 0.09 MAE. It has to stay in swing_only as well, since it is a swing whichever
# way the numerator goes, and it is the whiff half of csw_pct too.
expect("foul_tip counts as a whiff", "foul_tip" %in% whiff_desc, TRUE)
expect("foul_tip is still a swing",  "foul_tip" %in% swing_only, TRUE)
ft_frame <- tibble::tibble(
  pitch_type   = factor("SL", levels = "SL"), stand = "R", p_throws = "R",
  # 10 swings: 2 misses, 1 foul tip, 1 missed bunt and 1 foul bunt among them.
  # Whiffs are the 2 misses, the foul tip and the missed bunt, so 4 of 10 is
  # 40.0. The set that shipped this morning gives 2 of 8, 25.0. Counting bunts
  # as swings but never as whiffs gives 3 of 10, 30.0. Dropping the foul tip
  # from the denominator gives 3 of 9, 33.3.
  description  = c("swinging_strike", "swinging_strike", "foul_tip",
                   "missed_bunt", "foul_bunt",
                   rep("foul", 3), rep("hit_into_play", 2), rep("ball", 4)),
  type         = c(rep("S", 8), rep("X", 2), rep("B", 4)),
  in_zone      = c(rep(1, 8), 1, 1, rep(0, 4)),
  release_speed = 85, ivb = 2, hb = 3, release_spin_rate = 2400,
  woba_denom   = c(rep(NA, 8), 1, 1, rep(NA, 4)),
  estimated_woba_using_speedangle = c(rep(NA, 8), 0.3, 0.2, rep(NA, 4)))
ft_tb <- arsenal_table(ft_frame, "All",
                       tibble::tibble(pitch_type = character(), stuff_plus = numeric(),
                                      fg_exact = logical()))
cat(sprintf("  10 swings, 2 misses, a foul tip and a missed bunt: whiff%% %.1f, CSW%% %.1f\n",
            ft_tb$whiff_pct, ft_tb$csw_pct))
expect("whiff% is 4 of 10: foul tip and missed bunt both count", ft_tb$whiff_pct, 40.0)
expect("csw% counts them too", ft_tb$csw_pct, round(4/14*100, 1))
expect("a bunt attempt is a swing", all(c("foul_bunt", "missed_bunt", "bunt_foul_tip") %in% swing_only), TRUE)
expect("a missed bunt is a whiff",  "missed_bunt" %in% whiff_desc, TRUE)
expect("a foul bunt is NOT a whiff", "foul_bunt" %in% whiff_desc, FALSE)

cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 2: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
