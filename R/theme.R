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


# ---- Outcome description sets ------------------------------------------------

# Savant `description` values. These are matched with %in% against live data, so
# any edit here must be checked against unique(df$description) first. A
# "called strike" typo (missing underscore) matched zero rows once already and
# put a wrong CSW% into a finished report.

# Denominator for whiff%, which is off swings and not off total pitches.
swing_only <- c("swinging_strike","swinging_strike_blocked","foul","foul_tip","hit_into_play")

whiff_desc <- c("swinging_strike","swinging_strike_blocked")
