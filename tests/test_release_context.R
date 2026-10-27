# Context tab engine (release_context.R, pitch_shape.R): the release residual is
# orthogonal to its predictors, low-support arms are flagged not dropped, a lefty's HB
# is mirrored before ranking, shape floors match the data, and percentiles agree with
# an independent recompute of the league.
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
# The height lookup must join to nearly everyone
prof <- pitcher_release_profile(ad, ht) |> expected_release_height() |> release_rarity()
cat(sprintf("  profiles %d, matched to a height %d\n", nrow(prof), sum(!is.na(prof$height_in))))
expect("every qualified pitcher matched a listed height", sum(is.na(prof$height_in)), 0L)

cat("\n=== the residual is orthogonal to what produced it ===\n")
f <- prof[is.finite(prof$rel_z_resid), ]
expect("residual is uncorrelated with arm angle", abs(cor(f$rel_z_resid, f$arm)) < 0.05, TRUE)
expect("residual is uncorrelated with listed height", abs(cor(f$rel_z_resid, f$height_in)) < 0.05, TRUE)
expect("residual is centred on zero", abs(mean(f$rel_z_resid)) < 0.01, TRUE)
# Extension is not in the fit, by design
ce <- cor(f$rel_z_resid, f$ext, use = "complete.obs")
cat(sprintf("  residual vs extension r = %+.3f (deliberately uncontrolled)\n", ce))
expect("extension is still in the residual, not fitted out", ce < -0.2, TRUE)

cat("\n=== low-support arms are flagged, not removed ===\n")
expect("some arms are flagged", sum(prof$low_support, na.rm = TRUE) > 0, TRUE)
expect("flagged arms are still in the frame", all(is.finite(
  prof$rel_z_resid[which(prof$low_support)[1]])), TRUE)
# Both tails get flagged
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
# Traded pitchers appear once per pitch type
expect("no pitcher appears twice for one pitch type", nrow(dupes), 0L)
expect("every row has a team", all(!is.na(shp$pitch_team)), TRUE)
mar <- shp |> filter(grepl("Marinaccio", player_name), pitch_type == "FF")
expect("a traded pitcher is one row", nrow(mar), 1L)
# ...with both stints' pitches summed
mar_raw <- ad |> filter(grepl("Marinaccio", player_name), pitch_type == "FF") |> nrow()
expect("and carries every pitch from both clubs", mar$pitches[1], mar_raw)

cat("\n=== the shape table matches the Search tab ===\n")
# Shape rates are the Search tab's own
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
# Shape traits have no better end
expect("IVB has no better end",   context_better("ivb"),       "none")
expect("and it is not the same field as the colour ramp's direction",
       METRIC_SPEC$direction[METRIC_SPEC$metric == "ivb"], "high")
expect("every metric declares one", sum(is.na(METRIC_SPEC$context_better)), 0L)
expect("and only from the allowed set",
       setdiff(unique(METRIC_SPEC$context_better), c("high","low","none")), character(0))

cat("\n=== league percentiles, said as a sentence ===\n")
ref_lg <- readRDS("data/league_ref.rds")
tid <- prof$pitcher[which(prof$p_throws == "L" & prof$n > 800)[1]]
# Same league_ref the Characteristics tab uses
expect("league_ref carries every metric the phrases name",
       setdiff(names(LEAGUE_PHRASE), unique(ref_lg$metric)), character(0))

lhp <- shp[shp$pitcher == tid & as.character(shp$pitch_type) == "FF", , drop = FALSE]
lp  <- league_percentiles(lhp, ref_lg)
expect("a percentile frame comes back", is.data.frame(lp) && nrow(lp) > 0, TRUE)
expect("percentiles are in range", all(lp$pctile >= 0 & lp$pctile <= 100), TRUE)
# The phrase names the side he's on: the share is at least 50
expect("share is never below half", all(lp$share >= 50), TRUE)
expect("share agrees with the percentile",
       all(round(ifelse(lp$pctile >= 50, lp$pctile, 100 - lp$pctile)) == round(lp$share)), TRUE)

# A lefty's raw negative HB must be mirrored before ranking
hb_row <- lp[lp$metric == "hb", , drop = FALSE]
cat(sprintf("  lefty FF hb %.1f in -> %s %.0f%%\n",
            hb_row$value[1], hb_row$phrase[1], hb_row$share[1]))
expect("a lefty's HB is not pinned to an extreme by the sign convention",
       hb_row$share[1] < 99, TRUE)
# Without the mirror it would rank differently
raw_p <- lg_pctile(ref_lg, hb_row$value[1], "hb", "FF", "L", "All", "All Counts")$pctile
mir_p <- lg_pctile(ref_lg, hb_row$value[1] * arm_side_sign("L"), "hb", "FF", "L",
                   "All", "All Counts")$pctile
cat(sprintf("  raw lookup %.0f  vs mirrored lookup %.0f\n", raw_p, mir_p))
expect("the reported percentile came from the mirrored lookup", hb_row$pctile[1], mir_p)
expect("and the raw lookup would have differed",
       identical(as.numeric(raw_p), as.numeric(mir_p)), FALSE)

# Righties are unaffected
rhp <- shp[shp$p_throws == "R" & as.character(shp$pitch_type) == "FF" &
           shp$pitches > 500, , drop = FALSE][1, , drop = FALSE]
lpr <- league_percentiles(rhp, ref_lg)
expect("a righty's HB needs no mirror",
       lg_pctile(ref_lg, rhp$hb[1], "hb", "FF", "R", "All", "All Counts")$pctile,
       lpr$pctile[lpr$metric == "hb"])

cat("\n=== vaa and zone_pct reach the shape table ===\n")
expect("vaa is in the shape table", "vaa" %in% names(shp), TRUE)
expect("zone_pct is in the shape table", "zone_pct" %in% names(shp), TRUE)
expect("vaa is negative, as an approach angle must be",
       all(shp$vaa[is.finite(shp$vaa)] < 0), TRUE)
expect("zone_pct is a percentage",
       all(shp$zone_pct[is.finite(shp$zone_pct)] >= 0 &
           shp$zone_pct[is.finite(shp$zone_pct)] <= 100), TRUE)

cat("\n=== release side reaches the chart with the app's sign convention ===\n")
expect("rel_side is in the shape table", "rel_side" %in% names(shp), TRUE)
# Release side: righty positive, lefty negative
rs <- shp |> group_by(p_throws) |> summarise(m = median(rel_side, na.rm = TRUE), .groups = "drop")
cat(sprintf("  median rel_side: RHP %+.2f ft, LHP %+.2f ft\n",
            rs$m[rs$p_throws == "R"], rs$m[rs$p_throws == "L"]))
expect("a righty releases toward third", rs$m[rs$p_throws == "R"] > 0, TRUE)
expect("a lefty toward first", rs$m[rs$p_throws == "L"] < 0, TRUE)
expect("and it ranks", "rel_side" %in% league_percentiles(lhp, ref_lg)$metric, TRUE)

cat("\n=== shape floors are derived from the data, not asserted ===\n")
# shape_floor values re-derived from the data: n = (8r)^2, where r is the median
# within-pitcher SD over the SD of pitcher means
fl_src <- c(velo = "release_speed", ivb = "ivb", hb = "hb", vaa = "vaa",
            spin = "release_spin_rate", ext = "release_extension",
            rel_ht = "release_pos_z", rel_side = "release_pos_x")
fl_pt <- c("FF","SI","SL","CH","CU","FC","ST","KC","FS")
for (mm in names(fl_src)) {
  g <- ad |> filter(pitch_type %in% fl_pt) |>
    group_by(pitcher, p_throws, pitch_type) |>
    summarise(n = n(), w = sd(.data[[fl_src[[mm]]]], na.rm = TRUE),
              mu = mean(.data[[fl_src[[mm]]]], na.rm = TRUE), .groups = "drop") |>
    filter(n >= 100)
  b <- g |> group_by(p_throws, pitch_type) |> filter(n() >= 20) |>
    summarise(bsd = sd(mu), wsd = median(w), .groups = "drop")
  hat <- ceiling((8 * median(b$wsd / b$bsd))^2)
  have <- shape_floor(mm)
  ratio <- have / hat
  cat(sprintf("  %-9s stored %3d, data says %3d\n", mm, have, hat))
  # Within a factor of 1.6
  expect(sprintf("%s floor matches the data", mm), ratio > 0.62 && ratio < 1.6, TRUE)
}
# Release traits settle fastest, break traits slowest
fo <- vapply(c("rel_ht","rel_side","ext","velo","spin","ivb","hb","vaa"), shape_floor, numeric(1))
expect("release traits settle before velocity", max(fo[c("rel_ht","rel_side")]) < fo[["velo"]], TRUE)
expect("run settles last", fo[["hb"]], max(fo))
expect("and adjusting VAA moved it out of the loose end", fo[["vaa"]] < fo[["ivb"]], TRUE)
expect("every shape metric declares a floor",
       sum(is.na(METRIC_SPEC$shape_floor[METRIC_SPEC$kind == "mean"])), 0L)
expect("and no rate does",
       sum(!is.na(METRIC_SPEC$shape_floor[METRIC_SPEC$kind == "rate"])), 0L)

cat("\n=== percentiles agree with an independent recomputation ===\n")
# Rebuild the league population as build_league_ref.R does and compare ranks with
# lg_pctile(). Quantiles are stored on a 1-point grid (about half a point of error).
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
# 2 points: above the storage grid, far below a definition mismatch
expect("median deviation is quantisation, not drift", median(abs(devs)) < 2, TRUE)
expect("and no systematic bias in either direction", abs(median(devs)) < 1.5, TRUE)
# unname(): quantile() returns a named value
expect("95% of lookups land within 5 points",
       unname(quantile(abs(devs), .95) < 5), TRUE)

cat("\n", strrep("-", 60), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("RELEASE CONTEXT: ", if (!length(fails)) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (!length(fails)) 0 else 1)
