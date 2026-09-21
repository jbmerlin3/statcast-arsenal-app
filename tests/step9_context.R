# step9_context.R
#
#   Rscript tests/step9_context.R
#
# The Context tab's engine: R/release_context.R and R/cohorts.R.
#
# What this file is guarding, in order of how quietly each would fail:
#
#  1. The residual is orthogonal to its own predictors. If it ever correlates
#     with arm angle or height, the fit is broken and every number on the tab is
#     measuring the model instead of the pitcher. Silent: the page renders.
#  2. Low-support arms are FLAGGED, never dropped and never capped. The tab
#     exists to surface strange release points, so deleting the strangest ones
#     would defeat it while looking tidier.
#  3. A cohort never contains its own target. Leaving it in shrinks every delta
#     toward zero by 1/n, in a known direction, invisibly.
#  4. min_n is reported, never reached by widening. Auto-widening would turn an
#     honest "no peer group" into a confident wrong answer.
#  5. The velocity control actually changes something. A control that silently
#     no-ops is worse than no control, because the footnote claims it ran.
suppressMessages({library(dplyr);library(tidyr);library(purrr);library(forcats)
                  library(ggplot2);library(gt);library(readr);library(tibble)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

fails <- character(0)
expect <- function(what, got, want) {
  ok <- isTRUE(all.equal(got, want))
  cat(sprintf("  %-58s %s\n", what, if (ok) "ok" else
    paste0("FAIL got ", paste(format(got), collapse = ","),
           " wanted ", paste(format(want), collapse = ","))))
  if (!ok) fails <<- c(fails, what)
}

ad <- readRDS("data/app_data.rds")
ht <- load_pitcher_heights()

cat("=== the height lookup ===\n")
expect("height lookup loads", is.data.frame(ht), TRUE)
expect("keyed by pitcher, joinable to app_data", all(c("pitcher","height_in") %in% names(ht)), TRUE)
expect("no duplicate ids", anyDuplicated(ht$pitcher), 0L)
# A lookup that joins to nobody is the failure mode that renders as a blank
# column rather than an error, so the coverage is pinned, not assumed.
prof <- pitcher_release_profile(ad, ht) |> expected_release_height() |> release_rarity()
cat(sprintf("  profiles %d, matched to a height %d\n", nrow(prof), sum(!is.na(prof$height_in))))
expect("every qualified pitcher matched a listed height", sum(is.na(prof$height_in)), 0L)

cat("\n=== the residual is orthogonal to what produced it ===\n")
f <- prof[is.finite(prof$rel_z_resid), ]
expect("residual is uncorrelated with arm angle", abs(cor(f$rel_z_resid, f$arm)) < 0.05, TRUE)
expect("residual is uncorrelated with listed height", abs(cor(f$rel_z_resid, f$height_in)) < 0.05, TRUE)
expect("residual is centred on zero", abs(mean(f$rel_z_resid)) < 0.01, TRUE)
# Extension is NOT controlled for, deliberately: height and arm slot are who a
# pitcher is, extension is what he does, and subtracting stride would remove
# part of what makes a release point interesting. Measured -0.44 on 2026-09-12.
# Pinned so the decision is visible rather than assumed, and so a future edge
# that quietly adds extension to the fit has to come here and change it.
ce <- cor(f$rel_z_resid, f$ext, use = "complete.obs")
cat(sprintf("  residual vs extension r = %+.3f (deliberately uncontrolled)\n", ce))
expect("extension is still in the residual, not fitted out", ce < -0.2, TRUE)

cat("\n=== low-support arms are flagged, not removed ===\n")
expect("some arms are flagged", sum(prof$low_support, na.rm = TRUE) > 0, TRUE)
expect("flagged arms are still in the frame", all(is.finite(
  prof$rel_z_resid[which(prof$low_support)[1]])), TRUE)
# Both tails, which is the point of defining the flag on local support rather
# than on submarine arm angles. A rule that only caught low slots would pass a
# test written around Tyler Rogers and miss Alex Vesia at 69.9 degrees.
fl <- prof$arm[which(prof$low_support)]
expect("the flag catches the LOW tail", any(fl < 10, na.rm = TRUE), TRUE)
expect("and the HIGH tail", any(fl > 60, na.rm = TRUE), TRUE)
expect("well-supported arms are not flagged",
       any(prof$low_support[prof$arm_support >= prof$support_min], na.rm = TRUE), FALSE)

cat("\n=== scouting labels are total over the league ===\n")
lab <- arm_slot_label(prof$arm)
expect("every finite arm angle gets a slot name", sum(is.na(lab[is.finite(prof$arm)])), 0L)
expect("rarity labels are total", sum(is.na(rarity_label(prof$rarity_n[!is.na(prof$rarity_n)]))), 0L)

cat("\n=== one row per pitcher, not per pitcher per team ===\n")
shp <- pitch_shape(ad)
dupes <- shp |> count(pitcher, pitch_type) |> filter(n > 1)
# pitch_team was a grouping key until 2026-09-12, so a traded pitcher appeared
# once per club and took two slots in any cohort he joined. 148 duplicated
# combinations across 63 pitchers, and Headrick's 25-peer cohort held 21 men.
expect("no pitcher appears twice for one pitch type", nrow(dupes), 0L)
expect("the team column survives the fix", all(!is.na(shp$pitch_team)), TRUE)
mar <- shp |> filter(grepl("Marinaccio", player_name), pitch_type == "FF")
expect("a traded pitcher is one row", nrow(mar), 1L)
# His pitch total must be the SUM of both stints, not one of them, or the fix
# has quietly dropped half a season instead of merging it.
mar_raw <- ad |> filter(grepl("Marinaccio", player_name), pitch_type == "FF") |> nrow()
expect("and carries every pitch from both clubs", mar$pitches[1], mar_raw)

cat("\n=== cohorts ===\n")
tid <- prof$pitcher[which(prof$p_throws == "L" & prof$n > 800)[1]]
co  <- build_cohort(shp, prof, tid, "FF", match_on = c("release_height", "arm_angle"))
expect("a cohort is returned", !is.null(co), TRUE)
expect("the target is NOT in its own cohort", tid %in% co$members$pitcher, FALSE)
expect("every member shares the target's hand",
       all(co$members$p_throws == co$hand), TRUE)
expect("members are sorted nearest first", !is.unsorted(co$members$distance), TRUE)
tp <- prof[prof$pitcher == tid, ]

# In WINDOW mode membership is the stated tolerance and nothing else. A cohort
# that quietly reaches past its window to find friends is the failure this pins,
# and the mode is named explicitly rather than relied on as the default, which
# is exactly what broke this assertion when fixed_k became the default.
cw <- build_cohort(shp, prof, tid, "FF", match_on = c("release_height","arm_angle"),
                   mode = "window")
expect("window mode: every member is inside the release-height window",
       all(abs(cw$members$rel_z - tp$rel_z) <= COHORT_TOLERANCES$release_height), TRUE)
expect("window mode: every member is inside the arm-angle window",
       all(abs(cw$members$arm - tp$arm) <= COHORT_TOLERANCES$arm_angle), TRUE)
expect("window mode reports its units", cw$distance_units, "tolerance widths")

# In FIXED_K mode the point is that the rungs are the same size, so the sizes
# are what get pinned. Equal N across matched baselines is the property the
# whole mode exists to provide: without it the delta column is partly a
# statement about window width.
expect("fixed_k is the default", co$mode, "fixed_k")
expect("fixed_k mode reports its units", co$distance_units, "pooled SDs")
ck <- build_cohort(shp, prof, tid, "FF", match_on = "arm_angle", mode = "fixed_k", k = 25)
expect("fixed_k returns exactly K peers", nrow(ck$members), 25L)
kb <- nested_baselines(shp, prof, tid, "FF", metrics = "ivb", mode = "fixed_k", k = 25)
expect("every matched baseline is the same size", length(unique(kb$cohort_n[2:3])), 1L)
# League matches on nothing, so every same-hand peer is equally near and taking
# an arbitrary K would be taking an arbitrary K. It stays the whole league.
expect("League is NOT truncated to K", kb$cohort_n[1] > 25, TRUE)
expect("and League is identical across modes",
       kb$cohort_n[1],
       nested_baselines(shp, prof, tid, "FF", metrics = "ivb", mode = "window")$cohort_n[1])
# A K larger than the pool must return the pool, not error and not recycle.
big <- build_cohort(shp, prof, tid, "FF", match_on = "arm_angle",
                    mode = "fixed_k", k = 10000)
expect("K past the pool size returns the pool", nrow(big$members) < 10000, TRUE)
expect("an unknown match_on stops",
       tryCatch({build_cohort(shp, prof, tid, "FF", match_on = "wingspan"); "no error"},
                error = function(e) "stopped"), "stopped")

cat("\n=== the nested baselines ===\n")
fb <- nested_baselines(shp, prof, tid, "FF", metrics = "ivb")
expect("three rows, one per baseline", nrow(fb), 3L)
expect("ordered least to most conditioned",
       fb$baseline, c("League", "Release-height match", "Arm-angle match"))
# Conditioning can only ever shrink a cohort. If a matched baseline is bigger
# than League the filter ran backwards.
expect("conditioning never grows the cohort", fb$cohort_n[1] >= max(fb$cohort_n[2:3]), TRUE)
expect("a thin baseline keeps its row and is marked",
       all(fb$below_min_n[fb$cohort_n < COHORT_MIN_N]), TRUE)
expect("a thin baseline withholds its percentile",
       all(is.na(fb$pctile[fb$cohort_n < 10])), TRUE)

cat("\n=== the velocity control does something ===\n")
adj <- nested_baselines(shp, prof, tid, "FF", metrics = "ivb", control_for = "velo")
raw <- nested_baselines(shp, prof, tid, "FF", metrics = "ivb", control_for = NULL)
cat(sprintf("  league IVB delta: adjusted %+.2f, raw %+.2f\n", adj$delta[1], raw$delta[1]))
expect("adjusting is flagged as having happened", adj$adjusted[1], TRUE)
expect("and it changes the number", isTRUE(all.equal(adj$delta[1], raw$delta[1])), FALSE)
# Controlling a metric on itself is a perfect fit and a zero delta, which would
# render as a finding. Velo asked to control for velo must fall back to raw.
v <- nested_baselines(shp, prof, tid, "FF", metrics = "velo", control_for = "velo")
expect("velo is never controlled on itself", any(isTRUE(v$adjusted[1])), FALSE)
expect("and its delta is not zeroed", abs(v$delta[1]) > 0, TRUE)

cat("\n=== z is divided by the RIGHT spread ===\n")
# An adjusted delta is a residual from a within-cohort fit, so it must be read
# against the spread that fit LEAVES, not the spread it started with. Dividing
# by the raw cohort SD made z depend on how much velocity variation happened to
# be in that cohort, which is the one comparison the column exists to support.
za <- nested_baselines(shp, prof, tid, "FF", metrics = "ivb", control_for = "velo")
zr <- nested_baselines(shp, prof, tid, "FF", metrics = "ivb", control_for = NULL)
cat(sprintf("  adjusted: cohort SD %.3f, z denominator %.3f\n",
            za$cohort_sd[1], za$z_sd[1]))
expect("adjusted z uses a denominator that is NOT the raw cohort SD",
       isTRUE(all.equal(za$z_sd[1], za$cohort_sd[1])), FALSE)
# Controlling removes variation, so the residual spread cannot exceed the raw
# spread. If it does, the wrong quantity is being returned.
expect("and it is smaller than the raw cohort SD", za$z_sd[1] < za$cohort_sd[1], TRUE)
expect("unadjusted z still uses the raw cohort SD", zr$z_sd[1], zr$cohort_sd[1])
expect("z is delta over its own denominator",
       round(za$z[1], 6), round(za$delta[1] / za$z_sd[1], 6))

cat("\n=== baselines report how far they are the same men ===\n")
ovb <- nested_baselines(shp, prof, tid, "FF", metrics = "ivb", mode = "fixed_k")
ov  <- baseline_overlap(ovb)
expect("an overlap frame is returned", is.data.frame(ov), TRUE)
cat(sprintf("  %s vs %s: %d shared of %d and %d\n",
            ov$a[1], ov$b[1], ov$shared[1], ov$n_a[1], ov$n_b[1]))
expect("League is excluded from the overlap", any(c(ov$a, ov$b) == "League"), FALSE)
expect("shared cannot exceed either cohort",
       ov$shared[1] <= min(ov$n_a[1], ov$n_b[1]), TRUE)

cat("\n=== outcome metrics arrive with their denominators ===\n")
ob <- nested_baselines(shp, prof, tid, "FF",
                       metrics = c("whiff_pct", "chase_pct", "xwoba"))
expect("three outcome metrics resolve", nrow(ob), 9L)
expect("each carries the target's own denominator",
       all(is.finite(ob$target_denom)), TRUE)
# The floors must be the app's, not a second set invented here.
expect("floors come from METRIC_SPEC",
       sort(unique(ob$denom_floor)),
       sort(unique(METRIC_SPEC$floor[METRIC_SPEC$metric %in%
                                     c("whiff_pct","chase_pct","xwoba")])))
# Reuse, not reimplementation: the shape table's rates must BE the Search tab's.
sa <- search_aggregate(ad, hand = "All")
sh <- pitch_shape(ad, hand = "All")
j  <- merge(sa[, c("pitcher","pitch_type","whiff_pct","xwoba")],
            sh[, c("pitcher","pitch_type","whiff_pct","xwoba")],
            by = c("pitcher","pitch_type"))
expect("whiff% is byte-identical to the Search tab's", j$whiff_pct.x, j$whiff_pct.y)
expect("xwOBA is byte-identical to the Search tab's", j$xwoba.x, j$xwoba.y)

cat("\n=== direction lives in METRIC_SPEC, not in the plot ===\n")
expect("velo is high-is-better",  context_better("velo"),      "high")
expect("xwOBA is low-is-better",  context_better("xwoba"),     "low")
expect("whiff% is high-is-better",context_better("whiff_pct"), "high")
# IVB has no better end. More ride is the point of a four-seam and the death of
# a sinker, so a tab that tinted it green would assert something it cannot
# support. This is also why context_better is its OWN column: METRIC_SPEC's
# `direction` says ivb is "high", and reading that field for ivb is documented
# as a bug.
expect("IVB has no better end",   context_better("ivb"),       "none")
expect("and it is not the same field as the colour ramp's direction",
       METRIC_SPEC$direction[METRIC_SPEC$metric == "ivb"], "high")
expect("every metric declares one", sum(is.na(METRIC_SPEC$context_better)), 0L)
expect("and only from the allowed set",
       setdiff(unique(METRIC_SPEC$context_better), c("high","low","none")), character(0))

cat("\n=== strip data is deterministic and holds the target ===\n")
co25 <- build_cohort(shp, prof, tid, "FF", match_on = "release_height",
                     mode = "fixed_k", k = 25)
sd1 <- strip_data(co25, "velo"); sd2 <- strip_data(co25, "velo")
# Deterministic because nearPoints() matches on DATA coordinates: a jittered
# strip answers a hover with the wrong peer, and it also redraws on every
# reactive tick and reads as the data moving.
expect("identical across calls, no seed needed", identical(sd1, sd2), TRUE)
expect("the target is in the frame", sum(sd1$is_target), 1L)
expect("peers plus target", nrow(sd1), nrow(co25$members) + 1L)
expect("every point carries a name", sum(is.na(sd1$player_name)), 0L)

cat("\n=== verdicts are derived, and the middle third is not forced ===\n")
v <- strip_verdict(co25, "velo", " mph")
cat("  velo verdict:", v$verdict, "\n")
expect("rank is within the frame", v$rank >= 1 && v$rank <= v$n, TRUE)
expect("n counts the target too", v$n, nrow(co25$members) + 1L)
expect("delta is target minus peer mean",
       round(v$delta, 9), round(v$target - v$mean, 9))
# A pitcher at the cohort median must NOT be called a yes or a no. Forcing a
# binary there makes the header assert a difference the strip visibly denies.
mid <- strip_verdict(co25, "ivb", " in", more = "More", less = "Less")
pos <- (mid$rank - 0.5) / mid$n
cat(sprintf("  ivb at position %.3f -> %s\n", pos, mid$word))
expect("middle third reads 'about the same'",
       if (pos > 1/3 && pos < 2/3) mid$word == "About the same as his peers"
       else mid$word %in% c("More", "Less"), TRUE)
# Ranking must follow the metric's better end, or xwOBA would rank backwards.
vx <- strip_verdict(co25, "xwoba", "", digits = 3, strip_zero = TRUE)
dx <- strip_data(co25, "xwoba")
expect("low-is-better ranks ascending",
       vx$rank, which(order(dx$x) == which(dx$is_target)))

cat("\n=== league percentiles, said as a sentence ===\n")
ref_lg <- readRDS("data/league_ref.rds")
# The same reference the Characteristics tab shades from. If these ever stop
# agreeing, one of the two surfaces is lying about the same pitcher.
expect("league_ref carries every metric the phrases name",
       setdiff(names(LEAGUE_PHRASE), unique(ref_lg$metric)), character(0))

lhp <- shp[shp$pitcher == tid & as.character(shp$pitch_type) == "FF", , drop = FALSE]
lp  <- league_percentiles(lhp, ref_lg)
expect("a percentile frame comes back", is.data.frame(lp) && nrow(lp) > 0, TRUE)
expect("percentiles are in range", all(lp$pctile >= 0 & lp$pctile <= 100), TRUE)
# The comparative must always name the side he is ON, so the share is what he
# beats in that direction and never needs a mental flip.
expect("share is never below half", all(lp$share >= 50), TRUE)
expect("share agrees with the percentile",
       all(round(ifelse(lp$pctile >= 50, lp$pctile, 100 - lp$pctile)) == round(lp$share)), TRUE)

# THE BUG THIS PINS. league_ref stores hb arm-side positive for both hands,
# pitch_shape() reports the raw sign the movement chart draws, and a lefty's
# raw negative HB ranked inside a positive distribution lands under all of it.
# Observed 2026-09-18 before the fix: Noah Cameron's four-seam, -6.0 in of raw
# HB, reported as "less run than 100% of LHP four-seams". It rendered, it
# parsed, and it was nonsense.
hb_row <- lp[lp$metric == "hb", , drop = FALSE]
cat(sprintf("  lefty FF hb %.1f in -> %s %.0f%%\n",
            hb_row$value[1], hb_row$phrase[1], hb_row$share[1]))
expect("a lefty's HB is not pinned to an extreme by the sign convention",
       hb_row$share[1] < 99, TRUE)
# And prove the mirror is what saves it: the raw value would rank differently.
raw_p <- lg_pctile(ref_lg, hb_row$value[1], "hb", "FF", "L", "All", "All Counts")$pctile
mir_p <- lg_pctile(ref_lg, hb_row$value[1] * arm_side_sign("L"), "hb", "FF", "L",
                   "All", "All Counts")$pctile
cat(sprintf("  raw lookup %.0f  vs mirrored lookup %.0f\n", raw_p, mir_p))
expect("the reported percentile came from the MIRRORED lookup", hb_row$pctile[1], mir_p)
expect("and the raw lookup would have differed",
       identical(as.numeric(raw_p), as.numeric(mir_p)), FALSE)

# A righty is unaffected, which is what made this survive: arm_side_sign is 1.
rhp <- shp[shp$p_throws == "R" & as.character(shp$pitch_type) == "FF" &
           shp$pitches > 500, , drop = FALSE][1, , drop = FALSE]
lpr <- league_percentiles(rhp, ref_lg)
expect("a righty's HB needs no mirror",
       lg_pctile(ref_lg, rhp$hb[1], "hb", "FF", "R", "All", "All Counts")$pctile,
       lpr$pctile[lpr$metric == "hb"])

cat("\n=== vaa and zone_pct reach the cohort engine ===\n")
expect("vaa is in the shape table", "vaa" %in% names(shp), TRUE)
expect("zone_pct is in the shape table", "zone_pct" %in% names(shp), TRUE)
expect("vaa is negative, as an approach angle must be",
       all(shp$vaa[is.finite(shp$vaa)] < 0), TRUE)
expect("zone_pct is a percentage",
       all(shp$zone_pct[is.finite(shp$zone_pct)] >= 0 &
           shp$zone_pct[is.finite(shp$zone_pct)] <= 100), TRUE)
vb <- nested_baselines(shp, prof, tid, "FF", metrics = c("vaa", "zone_pct"))
expect("both resolve as cohort metrics", nrow(vb), 6L)
expect("and carry finite cohort means", all(is.finite(vb$cohort_mean)), TRUE)

cat("\n=== percentiles agree with an INDEPENDENT recomputation ===\n")
# The one check that can catch search_aggregate() and build_league_ref.R
# drifting apart. Both compute a pitcher-by-pitch-type value; the app displays
# one and ranks it inside a distribution built from the other. If a definition
# changes on one side only, every percentile on the Context tab shifts and
# nothing else in this suite notices: the numbers still render, still sit in
# 0-100, and still look plausible.
#
# So this rebuilds the reference population from app_data the way
# build_league_ref.R does, and compares the empirical rank to what lg_pctile()
# answers. Agreement is expected to be close but not exact: league_ref stores
# percentiles on a 1-point grid, so roughly half a point of quantisation is the
# floor, not an error.
ref_lg2 <- readRDS("data/league_ref.rds")
au_cells <- ad |>
  dplyr::group_by(pitcher, pitch_type, p_throws) |>
  dplyr::summarise(pitches = dplyr::n(),
                   swings = sum(description %in% swing_only),
                   velo = mean(release_speed, na.rm = TRUE),
                   ivb  = mean(ivb, na.rm = TRUE),
                   hb   = mean(hb, na.rm = TRUE) * arm_side_sign(p_throws[1]),
                   vaa  = mean(vaa, na.rm = TRUE),
                   rel_ht = mean(release_pos_z, na.rm = TRUE),
                   .groups = "drop")
au_shape <- pitch_shape(ad, hand = "All", min_pitches = 1)
devs <- c()
for (m in c("velo","ivb","hb","vaa","rel_ht")) {
  spm <- METRIC_SPEC[METRIC_SPEC$metric == m, ]
  pop <- au_cells[is.finite(au_cells[[m]]) & au_cells[[spm$denom]] >= spm$floor, ]
  tg  <- au_shape[is.finite(au_shape[[m]]) & au_shape[[spm$denom]] >= spm$floor, ]
  tg  <- utils::head(tg, 120)
  for (i in seq_len(nrow(tg))) {
    pt <- as.character(tg$pitch_type[i]); hd <- tg$p_throws[i]; v <- tg[[m]][i]
    lk <- if (m == "hb") v * arm_side_sign(hd) else v
    g  <- lg_pctile(ref_lg2, lk, m, pt, hd, "All", "All Counts")
    grp <- pop[[m]][pop$pitch_type == pt & pop$p_throws == hd]
    if (!is.finite(g$pctile) || length(grp) < 20) next
    devs <- c(devs, g$pctile - 100 * mean(grp <= lk))
  }
}
cat(sprintf("  %d comparisons, median |dev| %.2f, p95 %.2f, max %.2f pctile points\n",
            length(devs), median(abs(devs)), quantile(abs(devs), .95), max(abs(devs))))
expect("enough comparisons to mean something", length(devs) > 300, TRUE)
# 2 points is comfortably above the 1-point storage grid and far below any
# definition mismatch, which shifts a whole distribution.
expect("median deviation is quantisation, not drift", median(abs(devs)) < 2, TRUE)
expect("and no systematic bias in either direction", abs(median(devs)) < 1.5, TRUE)
# unname(): quantile() returns a NAMED value, and all.equal() compares the name
# attribute, so this reported "got TRUE wanted TRUE" and failed.
expect("95% of lookups land within 5 points",
       unname(quantile(abs(devs), .95) < 5), TRUE)

cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("STEP 9: ", if (!length(fails)) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (!length(fails)) 0 else 1)
