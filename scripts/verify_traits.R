# verify_traits.R
# Accuracy checks for the pitch trait columns, in four tiers:
#   A. Every derived column recomputed from the season store's raw inputs, per pitch
#   B. Table values vs an independently written groupby, per pitcher and pitch type
#   C. Invariants and physics (the only check VAA can have; no source publishes it)
#   D. Column freshness: no stale NA bands in arm angle
#   Rscript scripts/verify_traits.R [n_pitchers]

suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr)})
invisible(lapply(sort(list.files("R", full.names = TRUE)), source))

args <- commandArgs(trailingOnly = TRUE)
N    <- if (length(args)) as.integer(args[1]) else 120

STORE <- Sys.getenv("STATCAST_STORE",
                    unset = path.expand("~/baseball-store/statcast_clean_2026.rds"))
sc <- readRDS(STORE)
ad <- readRDS("data/app_data.rds")
cat("store ", nrow(sc), " rows | app_data ", nrow(ad), " rows\n\n", sep = "")

fails <- character()
chk <- function(label, ok, detail = "") {
  cat(sprintf("  %-52s %s %s\n", label, if (ok) "PASS" else "FAIL", detail))
  if (!ok) fails <<- c(fails, label)
}

# ---- A. Row-level recompute --------------------------------------------------
cat("A. ROW-LEVEL RECOMPUTE, every pitch, against raw store inputs\n")
# Joined on the pitch key, not row order
key <- c("game_pk", "at_bat_number", "pitch_number")
raw <- sc |>
  filter(!is.na(pitch_type), pitch_type != "") |>
  transmute(across(all_of(key)), pitcher, game_date,
            r_hb  = -pfx_x * 12,
            r_ivb =  pfx_z * 12,
            r_ext =  release_extension,
            r_relht = release_pos_z,
            r_relsd = -release_pos_x,
            r_zone = in_zone_flag(plate_x, plate_z, sz_bot, sz_top),
            pitch_type, plate_z,
            .t   = (-vy0 - sqrt(vy0^2 - 2 * ay * (50 - 17/12))) / ay,
            r_vaa_raw = atan((vz0 + az * .t) / abs(vy0 + ay * .t)) * 180 / pi) |>
  select(-.t)
# Location adjustment fit over the whole filtered store, as in build_app_data()
raw$r_vaa <- adjust_vaa_for_location(
  data.frame(vaa = raw$r_vaa_raw, plate_z = raw$plate_z, pitch_type = raw$pitch_type))

# Compare against the shipped file, matched to its store rows
adk <- sc |> filter(!is.na(pitch_type), pitch_type != "") |>
  transmute(across(all_of(key))) |> bind_cols(
    ad |> select(hb, ivb, vaa, in_zone,
                 a_ext = release_extension,
                 a_relht = release_pos_z, a_relsd = release_pos_x))

j <- inner_join(raw, adk, by = key)
chk("row count matches", nrow(j) == nrow(ad), sprintf("(%d)", nrow(j)))
mx <- function(a, b) max(abs(a - b), na.rm = TRUE)
chk("hb  identical",       mx(j$r_hb,  j$hb)  < 1e-9, sprintf("max|d| %.2e", mx(j$r_hb, j$hb)))
chk("ivb identical",       mx(j$r_ivb, j$ivb) < 1e-9, sprintf("max|d| %.2e", mx(j$r_ivb, j$ivb)))
chk("vaa identical",       mx(j$r_vaa, j$vaa) < 1e-9, sprintf("max|d| %.2e", mx(j$r_vaa, j$vaa)))
chk("extension identical", mx(j$r_ext, j$a_ext) < 1e-9)
chk("rel height identical", mx(j$r_relht, j$a_relht) < 1e-9)
# app_data stores release_pos_x raw; the table negates it
chk("rel side raw, unnegated in app_data", mx(j$r_relsd, -j$a_relsd) < 1e-9)
chk("in_zone identical",   all(j$r_zone == j$in_zone, na.rm = TRUE))

# ---- B. Aggregate recompute --------------------------------------------------
cat("\nB. AGGREGATE RECOMPUTE, ", N, " pitchers, independent groupby\n", sep = "")
set.seed(7)
elig <- ad |> count(pitcher) |> filter(n >= 300) |> slice_sample(n = N) |> pull(pitcher)
empty <- tibble(pitch_type = character(), stuff_plus = numeric())

got <- map_dfr(elig, function(id) {
  pl <- build_pitch_level(ad, id)
  arsenal_table(pl, "All", empty) |>
    transmute(pitcher = id, pitch_type = as.character(pitch_type),
              velocity, ivb, hb, vaa, spin, ext, rel_ht, rel_side)
})
# Independent groupby with the table's rounding and reconciled pitch codes.
# VAA is adjusted over the whole store first, as in the chain.
want <- sc |> filter(!is.na(pitch_type), pitch_type != "") |>
  mutate(.t = (-vy0 - sqrt(vy0^2 - 2*ay*(50 - 17/12))) / ay,
         vaa = atan((vz0 + az*.t)/abs(vy0 + ay*.t)) * 180/pi)
want$v <- adjust_vaa_for_location(want)
want <- want |> filter(pitcher %in% elig) |>
  reconcile_pitch_codes() |>
  mutate(pitch_type = as.character(pitch_type)) |>
  group_by(pitcher, pitch_type) |>
  summarise(w_velocity = round(mean(release_speed, na.rm = TRUE), 1),
            w_ivb  = round(mean(pfx_z, na.rm = TRUE) * 12, 1),
            w_hb   = round(mean(-pfx_x * 12, na.rm = TRUE), 1),
            w_vaa  = round(mean(v, na.rm = TRUE), 1),
            w_spin = round(mean(release_spin_rate, na.rm = TRUE), 0),
            w_ext  = round(mean(release_extension, na.rm = TRUE), 1),
            w_rel_ht = round(mean(release_pos_z, na.rm = TRUE), 2),
            w_rel_side = round(-mean(release_pos_x, na.rm = TRUE), 2),
            .groups = "drop")

cmp <- inner_join(got, want, by = c("pitcher", "pitch_type"))
cat("  matched cells: ", nrow(cmp), "\n", sep = "")
for (m in c("velocity","ivb","hb","vaa","spin","ext","rel_ht","rel_side")) {
  d <- abs(cmp[[m]] - cmp[[paste0("w_", m)]])
  chk(sprintf("%s  MAE %.4f  max %.3f", m, mean(d, na.rm=TRUE), max(d, na.rm=TRUE)),
      max(d, na.rm = TRUE) < 0.051)
}

# ---- C. Invariants and physics -----------------------------------------------
cat("\nC. INVARIANTS. The only tier that can catch a wrong DEFINITION.\n")
p <- ad |> group_by(pitcher, p_throws) |>
  summarise(n = n(), relht = mean(release_pos_z, na.rm = TRUE),
            relsd = -mean(release_pos_x, na.rm = TRUE),
            # release_extension has some NAs
            ext = mean(release_extension, na.rm = TRUE),
            vaa = mean(vaa, na.rm = TRUE), .groups = "drop") |>
  filter(n >= 200)
p <- left_join(p, distinct(ad, pitcher, player_name), by = "pitcher")

# A few pitches sail over the catcher and arrive rising: test rarity, not sign
pos <- sum(ad$vaa >= 0, na.rm = TRUE)
chk("VAA non-negative only for freak pitches (<0.01%)",
    pos / sum(!is.na(ad$vaa)) < 1e-4,
    sprintf("%d of %d, max plate_z %.2f ft", pos, sum(!is.na(ad$vaa)),
            max(ad$plate_z[ad$vaa >= 0], na.rm = TRUE)))
# Velocity-gated bounds: eephus pitches arrive at -34 degrees legitimately.
# Bounced pitches (negative plate_z) are checked separately.
fast <- ad$release_speed >= 70 & ad$plate_z > 0
chk("VAA within (-14, +2) for >= 70 mph that reached the plate",
    all(ad$vaa[fast] > -14 & ad$vaa[fast] < 2, na.rm = TRUE),
    sprintf("range %.1f to %.1f over %d pitches",
            min(ad$vaa[fast], na.rm=TRUE), max(ad$vaa[fast], na.rm=TRUE), sum(fast)))
bnc <- ad$plate_z <= 0 & ad$release_speed >= 70
chk("bounced pitches are steeper still, not broken",
    all(ad$vaa[bnc] > -25, na.rm = TRUE),
    sprintf("%d bounced at >= 70 mph, min %.1f", sum(bnc, na.rm = TRUE),
            min(ad$vaa[bnc], na.rm = TRUE)))
chk("slow lobs are steep, not broken",
    all(ad$vaa[!fast] > -40, na.rm = TRUE),
    sprintf("min %.1f over %d pitches under 70 mph",
            min(ad$vaa[!fast], na.rm=TRUE), sum(!fast)))
chk("no VAA is NA", !any(is.na(ad$vaa)))

# Lower bound 1 ft: Tyler Rogers releases at 1.26
chk("release height within (1, 8) ft", all(p$relht > 1 & p$relht < 8),
    sprintf("min %.2f (%s)", min(p$relht), p$player_name[which.min(p$relht)]))
chk("extension within (4, 8) ft", all(p$ext > 4 & p$ext < 8))
chk("no NA in pitcher-level release means",
    !any(is.na(p$relht)) && !any(is.na(p$ext)) && !any(is.na(p$relsd)))
sgn <- mean(sign(p$relsd) == ifelse(p$p_throws == "R", 1, -1))
chk("rel side sign tracks handedness", sgn > 0.98, sprintf("%.1f%% of arms", sgn*100))

# Raw VAA flattens with plate height; the shipped VAA shouldn't
ffj <- j |> filter(pitch_type == "FF", is.finite(r_vaa_raw), is.finite(plate_z))
r_raw <- cor(ffj$plate_z, ffj$r_vaa_raw)
r_adj <- cor(ffj$plate_z, ffj$vaa)
chk("higher plate_z -> flatter RAW VAA", r_raw > 0.4, sprintf("r = %.2f", r_raw))
chk("shipped VAA carries no plate_z confound", abs(r_adj) < 0.05, sprintf("r = %.3f", r_adj))
ff <- ad |> filter(pitch_type == "FF", !is.na(vaa), !is.na(ivb))

# More ride arrives flatter once plate height, release height and velocity are held
# fixed. Near-deterministic physics: the bar is 0.9.
rz <- function(f) residuals(lm(f, data = ff))
ctrl <- "~ poly(plate_z, 2) + release_pos_z + release_speed"
r_ivb <- cor(rz(as.formula(paste("ivb", ctrl))), rz(as.formula(paste("vaa", ctrl))))
chk("more IVB -> flatter VAA, holding height, slot and velo",
    r_ivb > 0.9, sprintf("partial r = %.3f (raw r = %.2f)", r_ivb, cor(ff$ivb, ff$vaa)))

lg <- ad |> group_by(pitch_type) |> summarise(v = round(mean(vaa, na.rm=TRUE),1), n=n()) |>
  filter(n > 20000)
cat("\n  league VAA by pitch type (published values: FF about -4.6, CU about -9.5)\n")
print(as.data.frame(lg), row.names = FALSE)
chk("curveball steeper than four-seam",
    lg$v[lg$pitch_type=="CU"] < lg$v[lg$pitch_type=="FF"] - 3)

# ---- D. Column freshness -----------------------------------------------------
# The newest dates are exempt: Savant fills arm angle in late
FRESH_LAG_DAYS <- 3L
STALE_NA_RATE  <- 0.25

cat("\nD. COLUMN FRESHNESS. Catches a silent NA band that reconciles with itself.\n")
# `gd`, not `.d`, which partial-matches mutate()'s .data argument
by_date <- ad |>
  mutate(gd = as.Date(game_date)) |>
  filter(gd <= max(gd) - FRESH_LAG_DAYS) |>
  group_by(gd) |>
  summarise(n = n(), na_rate = mean(is.na(arm_angle)), .groups = "drop")
bad <- by_date[by_date$na_rate > STALE_NA_RATE, ]
chk(sprintf("arm_angle populated on every date older than %d days", FRESH_LAG_DAYS),
    nrow(bad) == 0,
    sprintf("%d of %d dates over %.0f%% NA", nrow(bad), nrow(by_date), 100 * STALE_NA_RATE))
if (nrow(bad)) {
  cat("     stale dates (re-run the chain with a wider repull_days):\n")
  for (i in seq_len(min(nrow(bad), 12)))
    cat(sprintf("       %s  %5d pitches  %.0f%% NA\n", bad$gd[i], bad$n[i], 100 * bad$na_rate[i]))
  if (nrow(bad) > 12) cat("       ... and ", nrow(bad) - 12, " more\n", sep = "")
}
chk("season-wide arm_angle NA under 5%",
    mean(is.na(ad$arm_angle)) < 0.05,
    sprintf("%.2f%%", 100 * mean(is.na(ad$arm_angle))))

cat("\n", strrep("-", 66), "\n", sep = "")
if (length(fails)) { cat("FAILURES:\n"); for (f in fails) cat("  ", f, "\n") }
cat("VERIFY TRAITS: ", if (length(fails)) "FAIL" else "PASS", "\n", sep = "")
cat("\nNOT COVERED HERE, and not coverable internally:\n",
    " - VAA has NO external ground truth. Savant publishes no such column, so\n",
    "   tier C is the whole check. A definition error that is self-consistent\n",
    "   and passes the physics would survive this harness.\n",
    " - Release position per pitch type is not on Savant's custom leaderboard,\n",
    "   so it gets tiers A and C but no external diff.\n",
    " - Extension is NOT served by Savant's custom leaderboard either. An earlier
   version of this note claimed it was; five selection names were tried and
   every one returns an all-NA column.
 - The one external check that exists is arm angle, in
   scripts/verify_savant.R. It agrees at MAE 0.13 degrees over 110 pitchers,
   which validates the population, window and aggregation behind every
   release trait, but not the DEFINITION of any of them.\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
