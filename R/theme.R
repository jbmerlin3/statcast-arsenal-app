# theme.R
#
# Constants shared across the other R/ files, plus one pure label helper. No
# library() calls, nothing here executes against data.
#
# Source this first. pitch_text_colors is derived from pitch_colors at top level,
# so it is the one ordering dependency in the file.


# ---- Pitch colors ------------------------------------------------------------

# Single source of truth for pitch color, per CLAUDE.md. Copied from
# "Learning to pull stats.R", which generated the existing PDF reports, so the
# app and the reports stay visually consistent.
#
# Eleven codes: the ten CLAUDE.md lists plus KN, see below. A pitch type outside
# this vector can no longer reach a plot at all, because reconcile_pitch_codes()
# in features.R remaps or drops it first. It used to arrive here and throw
# "subscript out of bounds", which is what PITCH_CODE_RULES exists to prevent.
pitch_colors <- c(
  FF = "#FF0000", SI = "#FFA500", CU = "#8B008B", CH = "#228B22",
  SL = "#FFFF00", FS = "#00CED1", FC = "#8B4513", ST = "#DDB100",
  KC = "#6A0DAD", SV = "#7CFC00",
  # Eleventh code, beyond the ten CLAUDE.md lists. A knuckleball cannot be
  # mapped to anything else, and one genuine knuckleballer throws 182 of them,
  # about 30% of his arsenal. Dropping it would make his report wrong rather
  # than incomplete.
  KN = "#808080")

# Slider and sweeper yellow is legible as a fill but not as text on white, so
# table text uses a darkened gold for those two codes only. Every other code
# reuses its fill color.
pitch_text_colors <- pitch_colors
pitch_text_colors[c("SL", "ST")] <- c("#B59410", "#B59410")


# ---- Pitch code reconciliation ------------------------------------------------

# Savant emits codes beyond the ones this app charts. pitch_colors[[pt]] throws
# "subscript out of bounds" on any of them, which crashed the characteristics
# and usage tables for anyone throwing a slow curve.
#
# What separates a map from a drop is the median season workload of the code's
# throwers, measured over 2026:
#
#   EP  773 pitches, throwers average    31 pitches on the season -> position players
#   FA  642 pitches, throwers average    43                       -> position players
#   FO  389 pitches, throwers average   910  (Senga, Sasaki)      -> a real pitch
#   CS  135 pitches, throwers average 1,668  (Lugo, Peralta)      -> a real pitch
#
# A code absent from this table AND from pitch_colors is dropped and reported,
# so a code Savant adds next season degrades visibly instead of crashing.
PITCH_CODE_RULES <- data.frame(
  code   = c("CS",   "FO",   "EP",   "FA",   "PO",   "UN"),
  action = c("map",  "map",  "drop", "drop", "drop", "drop"),
  target = c("CU",   "FS",   NA,     NA,     NA,     NA),
  reason = c(
    "slow curve; Savant's own player page folds it into the curve",
    "forkball, a splitter variant; FG_TO_SAVANT already folds FanGraphs FO into FS",
    "eephus; 32 of its 35 throwers are position players mopping up",
    "unclassified fastball; 30 of its 33 throwers are position players",
    "pitchout, a play rather than a pitch type",
    "unknown, a tracking failure"
  ),
  stringsAsFactors = FALSE
)


# ---- Batter side labels ------------------------------------------------------

#' Display label for a handedness filter
#'
#' switch() with no fallthrough, so an unrecognised value stops here. The two gt
#' functions previously wrote `if (hand == "R") "vs RHH" else "vs LHH"`, and that
#' else meant a third value would have been titled "vs LHH" over pooled data:
#' wrong label, right numbers, no error.
#'
#' "All" means no stand filter at all, not a third batter side.
hand_label <- function(hand) {
  switch(hand,
    R   = "vs RHH",
    L   = "vs LHH",
    All = "vs All Batters",
    stop("unknown hand: ", hand, ". Expected R, L, or All.", call. = FALSE))
}


# ---- Sample floors and smoothing ---------------------------------------------

# Pitch types below this are dropped in build_pitch_level() before any rate is
# computed. A 3-pitch sample produces rate columns that read as real.
MIN_PITCH_COUNT <- 5

# Bandwidth for the heatmap KDE, in feet, x then z.
KDE_BW <- c(1.0, 1.2)

# Heatmap panels below this fall back to a white-dot scatter instead of a KDE.
# A density surface fitted to a handful of pitches invents structure that is not
# in the data. See CLAUDE.md hard rule 5, report the n rather than smoothing it.
KDE_MIN_N <- 15


# ---- League reference metrics and their floors -------------------------------

# MIN_PITCH_COUNT above decides whether a pitch type is SHOWN. This table
# decides whether a PERCENTILE is shown for it, which is a much higher bar, and
# the two are deliberately separate constants.
#
# Measured on 2026 data by resampling windows for the same pitcher and pitch and
# comparing the window percentile against his own full-season percentile:
#
#   whiff%, 5-10 swings  : median error 27 points, 53% off by more than 25
#   whiff%, 51-75 swings : median error 12 points, 21% off by more than 25
#   velocity, 5-10 pitches: median error 4.9 points, 2.8% off by more than 25
#
# A mean of a physical quantity stabilises an order of magnitude faster than a
# rate, so one floor for every metric would be wrong either way it was set.
#
# `denom` is the column the metric actually divides by, not pitch count. The
# conversions differ a lot: 50 swings is about 106 pitches, 50 out-of-zone about
# 87, and 50 PA-ending rows for xwOBA about 198.
#
# `direction` decides which fill scale a percentile is drawn on, and it is the
# one column here that changes a number's meaning rather than its precision.
#
#   high     a high percentile is a good pitch, drawn good-to-bad diverging
#   low      a high VALUE is bad, so the percentile is inverted before filling.
#            xwoba only. Without this a .420 xwOBA renders green
#   neutral  the metric ranks but does not grade, drawn monochrome so the cell
#            reads as rank rather than as quality
#
# ivb is neutral because it depends on the pitch: high IVB is good on a
# four-seam and bad on a curveball, where drop is the point. zone_pct is a
# choice rather than a virtue, a pitcher can live off the plate on purpose.
#
# hb is neutral for a second and worse reason, see METRIC_NOTES below.
#
# ext and usage_pct are not on the arsenal table, so nothing renders either one
# today. They still carry a real value rather than a placeholder, because the
# next person to add an extension column inherits whatever is written here
# silently. ext is high: more extension is less reaction time. usage_pct is
# neutral, it is genuinely directionless.
METRIC_SPEC <- data.frame(
  metric = c("velo", "ivb", "hb", "spin", "ext",
             "strike_pct", "csw_pct", "zone_pct", "usage_pct",
             "whiff_pct", "chase_pct", "xwoba"),
  kind   = c(rep("mean", 5), rep("rate", 7)),
  denom  = c(rep("pitches", 5),
             rep("pitches", 4),
             "swings", "oz", "pa"),
  floor  = c(rep(25, 5),
             rep(50, 4),
             50, 50, 50),
  direction = c("high", "neutral", "neutral", "high", "high",
                "high", "high", "neutral", "neutral",
                "high", "high", "low"),
  stringsAsFactors = FALSE
)

# Metrics whose percentile is correct but whose reading is not self-evident.
# Rendered as a footnote on that column, keyed by metric so tables.R asks
# "does this metric have a note" rather than testing for hb by name.
#
# hb is the only entry and is worse than directionless. IVB is pfx_z * 12 and HB
# is -pfx_x * 12, so arm side reads positive for a RHP only: median SI HB is
# +15.5 for a righty and -15.6 for a lefty. The percentile is right, because the
# reference keeps p_throws in the grain and never drops it, but "84th percentile
# HB" means arm-side run for one hand and glove-side for the other. See
# CLAUDE.md, formula conventions.
METRIC_NOTES <- c(
  hb = paste("HB is not arm-side normalized, so a high value is arm-side run",
             "for a RHP and glove-side for a LHP. Percentiles are within hand.")
)

# Arsenal table column name to METRIC_SPEC metric name. Only `velocity` actually
# differs, which is the whole reason this map has to exist rather than being
# assumed: one silent mismatch drops a column's context with no error.
#
# The ten keys here are exactly the columns that take percentile fill. PITCH,
# COUNT and PITCH% keep the pitch-colour row fill so the left of the table stays
# the identity block, and Stuff+ has no league reference to sit against.
ARSENAL_METRIC_COLS <- c(
  velocity   = "velo",
  ivb        = "ivb",
  hb         = "hb",
  spin       = "spin",
  strike_pct = "strike_pct",
  whiff_pct  = "whiff_pct",
  csw_pct    = "csw_pct",
  zone_pct   = "zone_pct",
  chase_pct  = "chase_pct",
  xwoba      = "xwoba"
)


# ---- Percentile fill palettes ------------------------------------------------

# Two scales, because a directional metric and a directionless one are answering
# different questions and must not look alike.
#
# Diverging, for direction "high" and "low". Blue poor, near-white average, red
# plus, which is the Savant convention a scout already reads without a legend.
# Stops are deliberately light: the plan puts black text on these cells, so every
# stop stays above roughly 7:1 against black. A saturated Savant pill needs white
# text and would force a second text rule.
#
# Monochrome, for direction "neutral". A single low-chroma hue reads as rank
# rather than as quality. Violet on purpose: it avoids the diverging scale's blue
# and red, and it avoids grey, which the below-floor state already owns.
PCTILE_PAL_DIVERGING <- c("#7FA8D6", "#BFD3E8", "#F0F0F0", "#EFC0AE", "#DC8163")
PCTILE_PAL_NEUTRAL   <- c("#F6F2F9", "#E3D9EC", "#D0C0DE", "#BDA7D1", "#AA8EC3")

# Below floor and no reference both render unfilled with grey italic text. Grey
# is reserved for them: half the cells in a two-week window land here, so it has
# to read as calm and normal rather than as a warning.
PCTILE_GREY <- "#767676"

# Minimum contributing pitchers before a reference cell may be quoted. Below
# this the lookup falls back to a coarser cell. At the full grain the Two
# Strikes cells hold a median of 4 eligible pitchers for whiff%, and a
# percentile off 4 pitchers is a fabrication.
MIN_REF_PITCHERS <- 20

# The six overlapping count buckets, matching count_usage_tbl() in tables.R.
# They are situational views and not a partition, so they do not sum to 100.
COUNT_BUCKETS <- list(
  "All Counts"      = NULL,
  "Early Count"     = c("0-0","0-1","1-0"),
  "Pitcher Ahead"   = c("0-1","0-2","1-2","2-2"),
  "Pitcher Behind"  = c("1-0","2-0","3-0","2-1","3-1"),
  "Pre Two Strikes" = c("0-0","0-1","1-0","1-1","2-1","3-1"),
  "Two Strikes"     = c("0-2","1-2","2-2","3-2")
)


# ---- Outcome description sets ------------------------------------------------

# Savant `description` values. These are matched with %in% against live data, so
# any edit here must be checked against unique(df$description) first. A
# "called strike" typo (missing underscore) matched zero rows once already and
# put a wrong CSW% into a finished report.

# Denominator for whiff%, which is off swings and not off total pitches.
swing_only <- c("swinging_strike","swinging_strike_blocked","foul","foul_tip","hit_into_play")

whiff_desc <- c("swinging_strike","swinging_strike_blocked")
