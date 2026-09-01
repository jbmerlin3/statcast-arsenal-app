# verify_traits.R
#
#   Rscript scripts/verify_traits.R [n_pitchers]
#
# Accuracy harness for the pitch TRAITS columns, added 2026-08-31 with the
# Characteristics tab split. Answers "are these numbers right", which the rest of
# the suite does not: phase1_check compares the app to an older version of the
# app, and step3_* compare it to a snapshot of itself. Both are regression
# checks. Neither can catch a number that has been wrong since the day it landed.
#
# THREE TIERS, and they catch different things. Reporting them as one number
# would hide which kind of error is present.
#
#   A. ROW-LEVEL RECOMPUTE. Every derived column in app_data, recomputed for all
#      589k pitches straight from the season store's raw inputs, compared
#      exactly. Catches a wrong source column, a dropped sign, a bad join, a
#      filter that differs between the store and the artifact.
#
#      What it CANNOT catch: a wrong definition. Both sides use mine. A VAA
#      formula with the plate at the back of the zone would agree with itself
#      perfectly here.
#
#   B. AGGREGATE RECOMPUTE. Table values against an independently written
#      groupby over the store, per pitcher and pitch type, reported as MAE over
#      many pitchers. CLAUDE.md's rule: one pitcher matching can be luck.
#
#   C. INVARIANTS AND PHYSICS. The only tier that can catch a wrong definition
#      without an external source, and the only check VAA has at all. Savant
#      publishes no VAA column, so there is no ground truth to diff against;
#      what is checkable is that the number obeys the physics it claims to
#      describe and reproduces known league values by pitch type.
#
# TIER B IS NOT AVAILABLE FOR RELEASE POSITION OR VAA AGAINST AN EXTERNAL
# SOURCE. Savant's custom leaderboard serves extension but not release_pos_x/z
# per pitch type, and nothing serves VAA. See docs and the note at the bottom.

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
# Keyed on the store's own row identity, not on order, so a reordering upstream
# cannot make this pass by coincidence.
cat("A. ROW-LEVEL RECOMPUTE, every pitch, against raw store inputs\n")
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
            .t   = (-vy0 - sqrt(vy0^2 - 2 * ay * (50 - 17/12))) / ay,
            r_vaa = atan((vz0 + az * .t) / abs(vy0 + ay * .t)) * 180 / pi) |>
  select(-.t)

# app_data carries no key columns, so join on the store rows it was built from.
# Rebuilding it here rather than reading data/app_data.rds would compare the
# recompute to itself; this compares the SHIPPED artifact.
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
# app_data stores release_pos_x RAW; the negation happens in the table layer.
chk("rel side raw, unnegated in app_data", mx(j$r_relsd, -j$a_relsd) < 1e-9)
chk("in_zone identical",   all(j$r_zone == j$in_zone, na.rm = TRUE))

# ---- B. Aggregate recompute --------------------------------------------------
cat("\nB. AGGREGATE RECOMPUTE, ", N, " pitchers, independent groupby\n", sep = "")
set.seed(7)
elig <- ad |> count(pitcher) |> filter(n >= 300) |> slice_sample(n = N) |> pull(pitcher)
empty <- tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical())

got <- map_dfr(elig, function(id) {
  pl <- build_pitch_level(ad, id)
  arsenal_table(pl, "All", empty) |>
    transmute(pitcher = id, pitch_type = as.character(pitch_type),
              velocity, ivb, hb, vaa, spin, ext, rel_ht, rel_side)
})
# Written separately from arsenal_table(): raw store columns, own expressions,
# same rounding. Shared rounding is deliberate, the table is what ships.
# reconcile_pitch_codes() is applied here too, and leaving it out was a harness
# bug that reported velocity, hb and spin as failing at max|d| of 0.1, 0.1 and 3.
# shape_arsenal() remaps CS to CU and KC to CU before the table groups, so an
# unreconciled groupby compares a pitcher's CU against a DIFFERENT set of
# pitches. The metrics that moved were exactly the ones whose rounding is fine
# enough to notice one or two extra pitches in a mean.
want <- sc |> filter(pitcher %in% elig, !is.na(pitch_type), pitch_type != "") |>
  reconcile_pitch_codes() |>
  mutate(.t = (-vy0 - sqrt(vy0^2 - 2*ay*(50 - 17/12))) / ay,
         v = atan((vz0 + az*.t)/abs(vy0 + ay*.t)) * 180/pi) |>
  mutate(pitch_type = as.character(pitch_type)) |>
  group_by(pitcher, pitch_type) |>
  summarise(w_velocity = round(mean(release_speed, na.rm = TRUE), 1),
            w_ivb  = round(mean(pfx_z, na.rm = TRUE) * 12, 1),
            w_hb   = round(mean(-pfx_x * 12, na.rm = TRUE) * arm_side_sign(p_throws[1]), 1),
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

# ---- C. Invariants and physics ----------------------------------------------
cat("\nC. INVARIANTS. The only tier that can catch a wrong DEFINITION.\n")
p <- ad |> group_by(pitcher, p_throws) |>
  summarise(n = n(), relht = mean(release_pos_z, na.rm = TRUE),
            relsd = -mean(release_pos_x, na.rm = TRUE),
            # na.rm, and it is load bearing: release_extension is NA on 610
            # pitches spread across 77 qualified arms, so without it this whole
            # tier evaluates to NA and every comparison below crashes rather
            # than failing. The table layer already uses na.rm here.
            ext = mean(release_extension, na.rm = TRUE),
            vaa = mean(vaa, na.rm = TRUE), .groups = "drop") |>
  filter(n >= 200)
p <- left_join(p, distinct(ad, pitcher, player_name), by = "pitcher")

# VAA is negative for essentially every pitch, but NOT for literally every one,
# and the exceptions are real rather than bad tracking. Two four-seams in the
# 2026 season cross the front of the plate still ascending, at plate_z 8.24 and
# 8.63 ft: balls that sailed several feet over the catcher. A pitch released at
# 5.9 ft and arriving at 8.6 ft went UP, so a positive approach angle is the
# correct answer and an "always negative" assertion was simply wrong.
#
# So the invariant is rarity, not sign. A formula error would not produce two
# exceptions in 589k rows, it would produce thousands.
pos <- sum(ad$vaa >= 0, na.rm = TRUE)
chk("VAA non-negative only for freak pitches (<0.01%)",
    pos / sum(!is.na(ad$vaa)) < 1e-4,
    sprintf("%d of %d, max plate_z %.2f ft", pos, sum(!is.na(ad$vaa)),
            max(ad$plate_z[ad$vaa >= 0], na.rm = TRUE)))
# Bounded CONDITIONAL ON VELOCITY, because a flat bound is either wrong or
# useless. Eephus pitches at 30 mph legitimately arrive at -34 degrees, so a
# -16 floor called 536 correct rainbows errors; widening the floor to -40 to
# admit them would then accept a genuinely broken four-seam. Gating on velocity
# keeps the bound tight over the 99.9% of pitches where tightness is worth
# having, and lets the lobs through on their own terms.
# Gated on plate_z > 0 as well as velocity, because plate_z can be NEGATIVE.
# Statcast extrapolates the trajectory to the front-of-plate plane whether or not
# the ball got there in the air, so a curveball spiked in the dirt reports a
# plate_z of -2.5 ft and a correspondingly steep -16 degree approach. Both
# numbers are right. Without this gate the bound flagged bounced breaking balls
# as broken data.
fast <- ad$release_speed >= 70 & ad$plate_z > 0
chk("VAA within (-14, +2) for >= 70 mph that reached the plate",
    all(ad$vaa[fast] > -14 & ad$vaa[fast] < 2, na.rm = TRUE),
    sprintf("range %.1f to %.1f over %d pitches",
            min(ad$vaa[fast], na.rm=TRUE), max(ad$vaa[fast], na.rm=TRUE), sum(fast)))
# Gated on velocity too, so this and the lob check below PARTITION the data
# rather than overlap. Ungated it caught 34 mph eephus pitches that also bounced,
# at -28 degrees, where the two effects compound: those belong to the lob check,
# which allows them, not to this one.
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

# The lower bound has to admit submariners. Tyler Rogers releases at 1.26 ft,
# the lowest in baseball, and a 3 ft floor called the league's most distinctive
# release point a data error. Bounds on this metric must be set by what arms
# actually do, not by what a typical arm does.
chk("release height within (1, 8) ft", all(p$relht > 1 & p$relht < 8),
    sprintf("min %.2f (%s)", min(p$relht), p$player_name[which.min(p$relht)]))
chk("extension within (4, 8) ft", all(p$ext > 4 & p$ext < 8))
chk("no NA in pitcher-level release means",
    !any(is.na(p$relht)) && !any(is.na(p$ext)) && !any(is.na(p$relsd)))
sgn <- mean(sign(p$relsd) == ifelse(p$p_throws == "R", 1, -1))
chk("rel side sign tracks handedness", sgn > 0.98, sprintf("%.1f%% of arms", sgn*100))

# Higher pitches arrive flatter. This is the location confound the raw metric is
# known to carry, and its PRESENCE is evidence the number is a real approach
# angle rather than a shape statistic wearing its name.
ff <- ad |> filter(pitch_type == "FF", !is.na(vaa), !is.na(ivb))
r_pz <- cor(ff$plate_z, ff$vaa)
chk("higher plate_z -> flatter VAA", r_pz > 0.4, sprintf("r = %.2f", r_pz))

# THE STRONGEST CHECK IN THIS FILE, and it took three tries to state correctly.
#
# Fix the two endpoints and the flight time and the algebra is forced: with
# dz = vz0*t + 0.5*az*t^2 pinned, vz_final = dz/t + 0.5*az*t, so vz_final rises
# with az. More ride must arrive flatter. That is a near-deterministic identity,
# not a tendency, so the bar is 0.9 rather than the 0.4 a mere tendency earns.
#
# The control set is the whole point:
#
#   plate_z only                      partial r = 0.11
#   plate_z + release height          partial r = 0.83
#   plate_z + release height + velo   partial r = 0.99
#
# Release height is the term I first left out, and omitting it makes the physics
# invisible: this league runs from Tyler Rogers at 1.26 ft to 7.12 ft, and that
# spread swamps ride. The raw correlation is 0.05. An earlier version of this
# check asserted the RAW correlation exceeded 0.4 and failed on correct data,
# which is worse than having no check, since it trains the reader to ignore it.
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
