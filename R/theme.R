# theme.R
#
# Constants shared across the other R/ files. No functions, no library() calls,
# nothing here executes against data.
#
# Source this first. pitch_text_colors is derived from pitch_colors at top level,
# so it is the one ordering dependency in the file.


# ---- Pitch colors ------------------------------------------------------------

# Single source of truth for pitch color, per CLAUDE.md. Copied from
# "Learning to pull stats.R", which generated the existing PDF reports, so the
# app and the reports stay visually consistent.
#
# The roster is the ten Savant codes CLAUDE.md lists. A pitch type outside it
# fails loudly at pitch_colors[[pt]] in tables.R rather than rendering in a
# default color, which is the intended behaviour for an unrecognized code.
pitch_colors <- c(
  FF = "#FF0000", SI = "#FFA500", CU = "#8B008B", CH = "#228B22",
  SL = "#FFFF00", FS = "#00CED1", FC = "#8B4513", ST = "#DDB100",
  KC = "#6A0DAD", SV = "#7CFC00")

# Slider and sweeper yellow is legible as a fill but not as text on white, so
# table text uses a darkened gold for those two codes only. Every other code
# reuses its fill color.
pitch_text_colors <- pitch_colors
pitch_text_colors[c("SL", "ST")] <- c("#B59410", "#B59410")


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
  stringsAsFactors = FALSE
)

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
