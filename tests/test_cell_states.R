# The four cell states through resolve_cell(): which state each case lands in, how it
# renders (fill, text, marker), direction per pitch type, HB mirroring, floor
# boundaries, zone geometry and the whiff and swing definitions.
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

# Find a fallback cell in the current league_ref instead of pinning one (they
# fill in as the season grows). Fails if none exists.
cat("=== THE FALLBACK CASE, found in the current league_ref ===\n")
find_fallback <- function() {
  cands <- ad |>
    group_by(pitcher, pitch_type, stand) |>
    summarise(n = n(), sw = sum(description %in% swing_only), .groups = "drop") |>
    filter(sw >= 60, n >= 80) |>
    slice_sample(n = 400)
  for (i in seq_len(nrow(cands))) {
    r <- cands[i, ]
    c1 <- tryCatch(cell_for(r$pitcher, as.character(r$pitch_type),
                            r$stand, "whiff_pct"), error = function(e) NULL)
    if (!is.null(c1) && identical(c1$state, "fallback"))
      return(list(cell = c1, who = r))
  }
  NULL
}
set.seed(4)
hit <- find_fallback()
expect("a fallback cell exists in the current reference", !is.null(hit), TRUE)
if (!is.null(hit)) {
  fb <- hit$cell
  show(sprintf("id %s %s vs %sHH whiff%%", hit$who$pitcher,
               hit$who$pitch_type, hit$who$stand), fb)
  cat("  grain: ", fb$grain, " | n_pitchers: ", fb$n_pitchers, "\n", sep = "")
  cat("  state note: ", fb$state_note, "\n", sep = "")
  expect("fallback state",       fb$state,       "fallback")
  expect("fallback names a coarser grain", is.character(fb$grain) && nzchar(fb$grain), TRUE)
  expect("fallback grain is not the finest cut",
         grepl("stand", fb$grain, fixed = TRUE), FALSE)
  # Fallback renders like an exact cell: no marker, not bold
  expect("fallback has no marker", fb$marker,     "")
  expect("fallback is not bold",  fb$font_weight, "normal")
  expect("fallback is filled",   fb$fill != PCTILE_UNFILLED, TRUE)
  expect("fallback has_ref",     fb$has_ref,     TRUE)
  expect("fallback quotes a real pitcher count", fb$n_pitchers >= MIN_REF_PITCHERS, TRUE)
}

cat("\n=== exact, below floor and no reference ===\n")
# Below floor on a real, non-zero denominator
ex <- cell_for(669358, "FF", "R", "velo");   show("exact        Baz FF velo vs RHH", ex)
# Thick league cell (172 pitchers), thin pitcher sample
bf <- cell_for(669358, "FF", "R", "whiff_pct", c("2026-08-04", "2026-08-17"))
                                             show("below floor  Baz FF whiff%, 2wk", bf)
# Thin on both sides: below floor wins
xw <- cell_for(669358, "KC", "R", "xwoba", c("2026-08-04", "2026-08-17"))
                                             show("both thin    Baz KC xwOBA, 2wk", xw)
# Waldron's knuckleball has no reference at any grain
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
# A thin cell is filled with its real percentile; its (n) marker must stay
expect("below floor is filled with its real percentile",
       bf$fill != PCTILE_UNFILLED, TRUE)
expect("below floor is normal text, not italic", bf$font_style, "normal")
expect("below floor is not greyed",              bf$text_color, "#000000")
expect("below floor still reports its n",        bf$marker, paste0(" (", bf$n, ")"))
expect("below floor had a reference all along", bf$has_ref, TRUE)

expect("no reference state",  nr$state, "no_reference")
expect("no reference clears its floor", nr$n >= nr$floor, TRUE)
expect("no reference marker", nr$marker, "\u2021")
expect("no reference fill",   nr$fill,  PCTILE_UNFILLED)
expect("no reference has no n in the marker", grepl("(", nr$marker, fixed = TRUE), FALSE)
expect("no reference has_ref", nr$has_ref, FALSE)

# Below floor wins precedence; has_ref still records the missing reference
expect("precedence resolves below floor", pr$state, "below_floor")
expect("precedence keeps the diagnostic",  pr$has_ref, FALSE)
expect("xwoba both-thin resolves below floor", xw$state, "below_floor")
expect("xwoba both-thin keeps the diagnostic", xw$has_ref, FALSE)

# HB shades by distance from the league mean, not by rank
expect("hb is not on the neutral scale", hb$fill == pctile_fill(hb$pctile, "neutral"), FALSE)
expect("and is not the rank-based diverging fill",
       hb$fill == pctile_fill(hb$pctile, metric_direction("hb", "FF")), FALSE)
expect("its reported percentile is still the true RANK", hb$pctile >= 0 && hb$pctile <= 100, TRUE)

cat("\n=== IVB and HB colour by DISTANCE from the league, not by rank ===\n")
# Literal spans, not SHAPE_DELTA_SPAN
expect("sitting on the league average is the middle of the ramp",
       shape_delta_pctile(10, 10), 50)
expect("six inches above saturates the top",   shape_delta_pctile(16, 10), 100)
expect("six inches below saturates the bottom", shape_delta_pctile(4, 10), 0)
expect("three inches above is halfway up",     shape_delta_pctile(13, 10), 75)
expect("and it clamps rather than running off", shape_delta_pctile(40, 10), 100)
expect("a missing league mean gives no fill",   is.na(shape_delta_pctile(10, NA_real_)), TRUE)

# Same distance from the league mean, same color, whatever the rank
same_delta <- pctile_fill(shape_delta_pctile(10, 6), "high") ==
              pctile_fill(shape_delta_pctile(-2, -6), "high")
expect("equal deltas render equal, whatever the rank would have said", same_delta, TRUE)
# For a pitch that wants the low end, below the league reddens
drop_is_red <- grDevices::col2rgb(pctile_fill(shape_delta_pctile(4, 10), "low"))
expect("a drop pitch four inches under the league fills red", drop_is_red[1] > drop_is_red[3], TRUE)

cat("\n=== shape direction is a property of the PITCH, not of the metric ===\n")
# Direction per pitch type, as literals; an unlisted code must stop
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
# A knuckleball has no intended shape
expect("a knuckleball keeps the neutral grey on ivb", metric_direction("ivb", "KN"), "neutral")
expect("and on hb",                                   metric_direction("hb",  "KN"), "neutral")
# Non-shape metrics come from METRIC_SPEC
expect("whiff% is unaffected by pitch type", metric_direction("whiff_pct", "CH"), "high")
expect("xwOBA is still low-is-good",         metric_direction("xwoba", "FF"), "low")
# Unlisted code: caught and named, not a crash
bad <- tryCatch(metric_direction("ivb", "ZZ"), error = function(e) "stopped")
expect("an unknown pitch type stops rather than guessing", bad, "stopped")

cat("\n=== direction by pitch type: Logan Webb ===\n")
webb <- build_pitch_level(ad, 657277L)
w_tb <- arsenal_table(webb, "All", tibble::tibble(pitch_type = character(),
                                                  stuff_plus = numeric()))
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
# HB: league_ref is arm-side positive for both hands; the table shows the raw sign
expect("a righty is unchanged by normalisation", arm_side_sign("R"), 1)
expect("a lefty is flipped",                     arm_side_sign("L"), -1)

si_r <- lg_cell(ref, "hb", "SI", "R", "All", "All Counts")$row$mean[[1]]
si_l <- lg_cell(ref, "hb", "SI", "L", "All", "All Counts")$row$mean[[1]]
cat(sprintf("  league_ref sinker HB: RHP %+.1f, LHP %+.1f\n", si_r, si_l))
expect("both hands' sinkers read arm side positive in league_ref",
       si_r > 0 && si_l > 0, TRUE)
expect("and a slider reads glove-side negative for both",
       lg_cell(ref, "hb", "SL", "R", "All", "All Counts")$row$mean[[1]] < 0 &&
       lg_cell(ref, "hb", "SL", "L", "All", "All Counts")$row$mean[[1]] < 0, TRUE)

# The table shows the raw sign: a lefty's sinker reads negative
lhp <- filter(build_pitch_level(ad, 702070), pitch_type == "SI")
no_stuff <- tibble::tibble(pitch_type = character(), stuff_plus = numeric())
lhp_si <- arsenal_table(lhp, "All", no_stuff)
cat(sprintf("  a LHP sinker in the table reads HB %+.1f\n", lhp_si$hb[1]))
expect("the table shows a lefty's sinker with the chart's RAW sign, negative",
       lhp_si$hb[1] < 0, TRUE)

# The ranking must still use the mirrored value
lhp_cells <- resolve_table(traits_tbl(lhp_si), arsenal_denoms(lhp, "All"),
                           ref, "L", "All")$cells
hb_cell <- lhp_cells[lhp_cells$metric == "hb", ][1, ]
p_mirrored <- lg_pctile(ref, -lhp_si$hb[1], "hb", "SI", "L", "All", "All Counts")$pctile
p_raw      <- lg_pctile(ref,  lhp_si$hb[1], "hb", "SI", "L", "All", "All Counts")$pctile
cat(sprintf("  lefty SI hb percentile: shown %d, mirrored %d, raw %d\n",
            as.integer(hb_cell$pctile), as.integer(p_mirrored), as.integer(p_raw)))
expect("the percentile is read off the arm-side mirrored value",
       as.integer(hb_cell$pctile), as.integer(p_mirrored))
# Guard the guard: raw and mirrored must rank differently here
expect("and the raw value would have ranked differently",
       identical(as.integer(p_mirrored), as.integer(p_raw)), FALSE)

cat("\n=== which metrics claim a direction, and which refuse ===\n")
# Directions pinned as literals
expect("xwOBA is the only low-is-good metric",
       METRIC_SPEC$metric[METRIC_SPEC$direction == "low"], "xwoba")
expect("usage% is the only metric that still refuses a direction",
       METRIC_SPEC$metric[METRIC_SPEC$direction == "neutral"], "usage_pct")
expect("ivb, hb and zone% are directional now",
       all(METRIC_SPEC$direction[METRIC_SPEC$metric %in% c("ivb", "hb", "zone_pct")] == "high"),
       TRUE)
# HB keeps its explanatory footnote
expect("hb still carries the note explaining what its high end is",
       grepl("arm-side", col_notes_for(c(hb = "hb"))[["hb"]]), TRUE)

cat("\n=== Stuff+ shades off 100, with no league lookup ===\n")
# Stuff+ shading, read from the rendered table; expected colors are literals
stuff_tbl <- tibble::tibble(
  pitch_type = factor(c("FF", "SL", "CH", "CU", "SI"),
                      levels = c("FF", "SL", "CH", "CU", "SI")),
  count = 100L, pitch_pct = 20, velocity = 93, ivb = 15, hb = 5, spin = 2300,
  # Columns the real arsenal_table() returns
  vaa = -5.0, ext = 6.5, rel_ht = 5.9, rel_side = 2.0,
  gb_pct = 42.0, rv = 1.0,
  strike_pct = 60, whiff_pct = 25, csw_pct = 28, zone_pct = 50, chase_pct = 30,
  xwoba = 0.310,
  # 75 and 125 are the span ends; 40 and 160 must clamp
  stuff_plus = c(100, 85, 115, 40, 160))
# Stuff+ renders through traits_gt()
sh <- as.character(gt::as_raw_html(traits_gt(traits_tbl(stuff_tbl), "All")))
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
# Neutral scale: symmetric, sharing the diverging middle stop
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
# Neutral fills never use the no-reference text grey
expect("no neutral fill collides with the below-floor text grey",
       PCTILE_GREY %in% vapply(0:100, function(p) pctile_fill(p, "neutral"), character(1)),
       FALSE)
expect("hb carries its note", is.character(hb$metric_note), TRUE)
expect("velo carries no note", ex$metric_note, NA_character_)

cat("\n=== states are separable without hue (exact and fallback look the same) ===\n")
four <- list(exact = ex, fallback = fb, below_floor = bf, no_reference = nr)
key  <- vapply(four, function(c) paste(c$fill != PCTILE_UNFILLED, c$font_weight,
                                       c$font_style, gsub("[0-9]+", "N", c$marker)), character(1))
for (nm in names(key)) cat(sprintf("  %-13s %s\n", nm, key[[nm]]))
expect("exact and fallback render alike", key[["exact"]] == key[["fallback"]], TRUE)
expect("the other states differ on non-hue channels", length(unique(key)), 3L)

cat("\n=== DIRECTION: xwOBA inverts, whiff% does not ===\n")
cat("  high  p=90 ->", pctile_fill(90, "high"), " p=10 ->", pctile_fill(10, "high"), "\n")
cat("  low   p=90 ->", pctile_fill(90, "low"),  " p=10 ->", pctile_fill(10, "low"),  "\n")
expect("low is the mirror of high", pctile_fill(90, "low"), pctile_fill(10, "high"))
expect("xwoba direction from spec", METRIC_SPEC$direction[METRIC_SPEC$metric == "xwoba"], "low")
expect("unknown direction stops",
       tryCatch({pctile_fill(50, "up"); "no error"}, error = function(e) "stopped"), "stopped")

cat("\n=== every resolved fill is a usable colour ===\n")
# Every resolved fill is a real color, never NA
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
# Floor boundary with literal denominators 50 and 49 (fl and fl - 1 would move
# with the constant). Changing a floor means editing this block.
fl   <- METRIC_SPEC$floor[METRIC_SPEC$metric == "whiff_pct"]
pl_r <- build_pitch_level(ad, 669358) |> filter(stand == "R", pitch_type == "FF")
v_ff <- metric_value(pl_r, "whiff_pct")
at_floor <- resolve_cell(ref, v_ff, "whiff_pct", "FF", "R", "R", "All Counts",
                         list(pitches = 500, swings = 50, oz = 200, pa = 100))
below_1  <- resolve_cell(ref, v_ff, "whiff_pct", "FF", "R", "R", "All Counts",
                         list(pitches = 500, swings = 49, oz = 200, pa = 100))
show("at floor     swings=50", at_floor)
show("floor - 1    swings=49", below_1)

# Pin the floor itself
expect("the rate floor is still the measured 50", fl, 50)

# n == floor is eligible (pins >= vs >)
expect("at floor is eligible",     at_floor$state, "exact")
expect("at floor is filled",       at_floor$fill != PCTILE_UNFILLED, TRUE)
# A cell with no computable value stays white
na_cell <- resolve_cell(ref, NA_real_, "whiff_pct", "FF", "R", "All", "All Counts",
                        counts = list(pitches = 40, swings = 0, oz = 12, pa = 6))
expect("a value with no sample is below floor", na_cell$state, "below_floor")
expect("a value with no sample stays WHITE",    na_cell$fill,  PCTILE_UNFILLED)
expect("and its fill is a real colour, never NA", is.na(na_cell$fill), FALSE)

expect("floor - 1 is below floor", below_1$state,  "below_floor")
# Below floor is still filled
expect("floor - 1 is filled anyway", below_1$fill != PCTILE_UNFILLED, TRUE)
expect("floor - 1 carries its n",  below_1$marker, paste0(" (", below_1$n, ")"))
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
# Zone edges as literal coordinates: nominal edge + 0.1208 ft ball radius
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
# Foul tips are whiffs (matches Savant)
expect("foul_tip counts as a whiff", "foul_tip" %in% whiff_desc, TRUE)
expect("foul_tip is still a swing",  "foul_tip" %in% swing_only, TRUE)
ft_frame <- tibble::tibble(
  pitch_type   = factor("SL", levels = "SL"), stand = "R", p_throws = "R",
  # 10 swings, 4 whiffs (2 misses, a foul tip, a missed bunt) = 40.0
  description  = c("swinging_strike", "swinging_strike", "foul_tip",
                   "missed_bunt", "foul_bunt",
                   rep("foul", 3), rep("hit_into_play", 2), rep("ball", 4)),
  type         = c(rep("S", 8), rep("X", 2), rep("B", 4)),
  in_zone      = c(rep(1, 8), 1, 1, rep(0, 4)),
  release_speed = 85, ivb = 2, hb = 3, release_spin_rate = 2400,
  # Constant release columns
  vaa = -7.0, release_extension = 6.4, release_pos_x = -1.9, release_pos_z = 5.9,
  # bb_type is "" off balls in play; run value held at 0
  bb_type = ifelse(description == "hit_into_play", "ground_ball", ""),
  delta_pitcher_run_exp = 0,
  woba_denom   = c(rep(NA, 8), 1, 1, rep(NA, 4)),
  estimated_woba_using_speedangle = c(rep(NA, 8), 0.3, 0.2, rep(NA, 4)))
ft_tb <- arsenal_table(ft_frame, "All",
                       tibble::tibble(pitch_type = character(), stuff_plus = numeric()))
cat(sprintf("  10 swings, 2 misses, a foul tip and a missed bunt: whiff%% %.1f, CSW%% %.1f\n",
            ft_tb$whiff_pct, ft_tb$csw_pct))
expect("whiff% is 4 of 10: foul tip and missed bunt both count", ft_tb$whiff_pct, 40.0)
expect("csw% counts them too", ft_tb$csw_pct, round(4/14*100, 1))
expect("a bunt attempt is a swing", all(c("foul_bunt", "missed_bunt", "bunt_foul_tip") %in% swing_only), TRUE)
expect("a missed bunt is a whiff",  "missed_bunt" %in% whiff_desc, TRUE)
expect("a foul bunt is not a whiff", "foul_bunt" %in% whiff_desc, FALSE)

cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("CELL STATES: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
