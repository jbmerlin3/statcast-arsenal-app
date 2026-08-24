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
  # usage_pct's denominator is the CUT's total pitches, not the pitch type's.
  # It is a share: its precision comes from how many pitches the share was
  # measured over, not from how many were of this type. Flooring it on the pitch
  # type's own count greys a well-measured 30% share because the pitch is
  # uncommon, which is backwards.
  denom  = c(rep("pitches", 5),
             "pitches", "pitches", "pitches", "cut_pitches",
             "swings", "oz", "pa"),
  floor  = c(rep(25, 5),
             rep(50, 4),
             50, 50, 50),
  # NOTE: the direction here is NOT used for ivb or hb. Those two are looked up
  # per pitch type in PITCH_SHAPE_DIRECTION, because more ride is the point of a
  # four-seam and the death of a sinker. The entries below are kept so the frame
  # stays rectangular and so METRIC_SPEC remains the one list of metrics, and
  # metric_direction() is the only thing that should ever be asked for a
  # direction. Reading spec$direction for ivb or hb is a bug.
  #
  # zone_pct is treated as high-is-good outright, which is a simplification: a
  # pitcher can live in the zone and get hit.
  #
  # usage_pct stays neutral, and is now the only metric that is. A pitch type's
  # share of an arsenal has no good end at all.
  direction = c("high", "high", "high", "high", "high",
                "high", "high", "high", "neutral",
                "high", "high", "low"),
  stringsAsFactors = FALSE
)

# Metrics whose percentile is correct but whose reading is not self-evident.
# Rendered as a footnote on that column, keyed by metric so tables.R asks
# "does this metric have a note" rather than testing for hb by name.
#
# hb is the only entry, and the note changed on 2026-08-24 because the thing it
# warned about was fixed rather than documented.
#
# It used to read "HB is not arm-side normalized, so a high value is arm-side run
# for a RHP and glove-side for a LHP", which was true and unhelpful: it told the
# reader the column meant two different things and left them to do the flip. Once
# the cell carried a COLOUR as well as a number that stopped being tenable, since
# a righty and a lefty with identical arm-side run were rendering red and blue.
#
# The table and league_ref now both store HB arm-side positive, so the note says
# what the number is rather than apologising for what it is not. The raw signed
# value still lives in app_data and still drives the movement chart.
METRIC_NOTES <- c(
  hb = paste("HB is arm-side normalized: positive is arm-side run for both",
             "hands, so a lefty and a righty with the same shape read the same.",
             "The movement chart keeps the true direction instead.")
)

# Arsenal table column name to METRIC_SPEC metric name. Only `velocity` actually
# differs, which is the whole reason this map has to exist rather than being
# assumed: one silent mismatch drops a column's context with no error.
#
# The ten keys here are exactly the columns that take percentile fill. PITCH,
# COUNT and PITCH% keep the pitch-colour row fill so the left of the table stays
# the identity block, and Stuff+ has no league reference to sit against.
# Shape direction, BY PITCH TYPE. The one place baseball knowledge enters the
# colour system.
#
# IVB and HB have no direction as metrics, only as pitch shapes. More ride is the
# point of a four-seam and the death of a sinker; more arm-side run is what a
# changeup wants and the opposite of what a sweeper wants. Grading them globally
# gets it backwards for half the arsenal: Logan Webb's changeup sits 5 inches
# under the league on IVB, which Savant paints red and calls "9.0 MORE DROP",
# and a global high-is-good rule painted it blue.
#
# So direction is looked up per (metric, pitch type) and every one of the eleven
# codes is written out. No default: a code that reaches here unlisted should stop
# the render rather than quietly inherit somebody's guess about what a good
# knuckleball looks like.
#
#   ivb  high  the pitch wants to RIDE       FF FC
#        low   the pitch wants to DROP       SI CH FS SL ST CU KC SV
#   hb   high  the pitch wants ARM SIDE      FF SI CH FS
#        low   the pitch wants GLOVE SIDE    FC SL ST CU KC SV
#
# KN is neutral on both. A knuckleball has no intended shape, which is the whole
# idea, so it keeps the symmetric grey.
PITCH_SHAPE_DIRECTION <- list(
  ivb = c(FF = "high", FC = "high",
          SI = "low",  CH = "low", FS = "low", SL = "low",
          ST = "low",  CU = "low", KC = "low", SV = "low",
          KN = "neutral"),
  hb  = c(FF = "high", SI = "high", CH = "high", FS = "high",
          FC = "low",  SL = "low",  ST = "low",  CU = "low",
          KC = "low",  SV = "low",
          KN = "neutral")
)


#' The direction a metric runs for one pitch type
#'
#' Most metrics have one direction whatever the pitch: a whiff is a whiff. IVB
#' and HB do not, so they carry a per-pitch-type table and everything else falls
#' through to METRIC_SPEC.
#'
#' Stops on an unknown pitch type rather than defaulting, for the same reason
#' pctile_fill() stops on an unknown direction: the failure it prevents is a cell
#' that renders confidently in the wrong colour.
metric_direction <- function(metric, pitch_type) {
  tbl <- PITCH_SHAPE_DIRECTION[[metric]]
  if (is.null(tbl)) {
    spec <- METRIC_SPEC[METRIC_SPEC$metric == metric, ]
    if (nrow(spec) != 1) stop("unknown metric: ", metric, call. = FALSE)
    return(spec$direction)
  }
  pt <- as.character(pitch_type)
  if (!pt %in% names(tbl)) {
    stop("no shape direction for ", metric, " on pitch type ", pt,
         ". Add it to PITCH_SHAPE_DIRECTION.", call. = FALSE)
  }
  unname(tbl[[pt]])
}


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


# ---- Results panel league context --------------------------------------------

# The seven panel numbers that take a league fill, as data rather than as
# conditionals inside results_panel(). TBF is deliberately absent: it is the
# sample the other three are measured over, not a result, and colouring it would
# say a busy reliever is better than a good one.
#
# `direction` is read by pctile_fill() and is never inferred from the metric
# name. Getting one of these backwards renders a .420 xwOBA deep red and is the
# single least visible bug this feature can have, which is why it is a column
# here and an assertion in the tests.
#
# `denom` names the count that decides whether a value is worth ranking, and
# `min_denom` is the absolute backstop under the scaled floor. `scales` is FALSE
# for IP alone: its denominator is games pitched, which is very nearly the thing
# IP measures, so scaling that floor with the window would grey out exactly the
# pitchers whose low IP is the finding.
RESULTS_METRIC_SPEC <- data.frame(
  metric    = c("k_bb",  "hh",    "xwoba", "ip",    "era",  "whip", "fip"),
  label     = c("K-BB%", "HH%",   "xwOBA", "IP",    "ERA",  "WHIP", "FIP"),
  source    = c("sc",    "sc",    "sc",    "gl",    "gl",   "gl",   "gl"),
  denom     = c("tbf",   "bbe",   "pa",    "games", "ip",   "ip",   "ip"),
  min_denom = c(10,      8,       10,      1,       2,      2,      2),
  scales    = c(TRUE,    TRUE,    TRUE,    FALSE,   TRUE,   TRUE,   TRUE),
  direction = c("high",  "low",   "low",   "high",  "low",  "low",  "low"),
  stringsAsFactors = FALSE
)

# The scaled floor is half the median denominator among pitchers who appeared in
# the selected window. Measured across window lengths, that keeps two thirds to
# five sixths of the league in the comparison at every length, where a fixed 50
# TBF leaves 16% of it over two weeks:
#
#   window        TBF floor   clears
#   full season      65        66%
#   30 days          23        78%
#   14 days          12        86%
#
# Half is a choice and not a derivation. It reads as "at least half the work a
# typical pitcher did in this window", and it is one constant rather than a
# table of window lengths nobody would maintain.
RESULTS_FLOOR_FRACTION <- 0.5


# ---- Pitch trait search ------------------------------------------------------

# The five traits the search tab puts a slider on, with the label, the rounding
# the slider steps in, and the number of decimals the table shows.
#
# Traits, not outcomes. A slider on whiff% would let you ask for pitches that
# already worked, which is a different and much less useful question than asking
# for a shape and then seeing whether it worked. The outcome columns are shown
# and coloured, never filtered on.
SEARCH_TRAITS <- data.frame(
  trait  = c("velo",  "ivb",  "hb",   "spin",  "ext"),
  label  = c("Velocity (mph)", "IVB (in)", "HB (in)", "Spin (rpm)", "Extension (ft)"),
  step   = c(0.1,     0.5,    0.5,    25,      0.1),
  digits = c(1L,      1L,     1L,     0L,      1L),
  stringsAsFactors = FALSE
)

# Search table column to METRIC_SPEC metric, the same map ARSENAL_METRIC_COLS is
# for the arsenal table. Extension takes a fill here and does not there, because
# the arsenal table has no extension column.
#
# There is no strike_pct, csw_pct or zone_pct here on purpose: the search table
# is already wide, and those three answer a question about a pitcher rather than
# about a pitch shape.
# How many matches the table draws, whatever the query returns. Measured on 457
# righty four-seams, which is what an unfiltered search returns: 5.5 s to render
# and 2.7 MB of HTML, against 1.0 s and 309 KB at 50 rows. The count line above
# the table always reports the true total, so the cap hides rows and never hides
# the answer to "how many".
SEARCH_MAX_ROWS <- 50L

SEARCH_METRIC_COLS <- c(
  velo      = "velo",
  ivb       = "ivb",
  hb        = "hb",
  spin      = "spin",
  ext       = "ext",
  whiff_pct = "whiff_pct",
  chase_pct = "chase_pct",
  xwoba     = "xwoba"
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
# Neutral, for direction "neutral": IVB, HB, zone% and usage%. These have no
# good end. More IVB is a plus on a four-seam and a minus on a curveball, and HB
# flips sign with the pitcher's hand, so painting them red and blue would assert
# a quality the app cannot support.
#
# SYMMETRIC GREY, sharing the diverging scale's middle stop exactly. Average is
# the same light grey in both scales, and a cell darkens as it moves away from
# average in EITHER direction, so it reads as "unusual" rather than as "good".
# One colour system on the page instead of two.
#
# This replaced a violet ramp on 2026-08-24. The violet was chosen to avoid grey,
# on the reasoning that the below-floor state owns grey; that turned out to be
# the wrong worry, because below floor is an UNFILLED cell with grey italic text
# and these are filled cells with black text, which is a difference in two
# channels. What the violet actually did was put a second hue on a table whose
# whole point was one.
PCTILE_PAL_DIVERGING <- c("#7FA8D6", "#BFD3E8", "#F0F0F0", "#EFC0AE", "#DC8163")
PCTILE_PAL_NEUTRAL   <- c("#C4C4C4", "#DEDEDE", "#F0F0F0", "#DEDEDE", "#C4C4C4")

# Below floor and no reference both render unfilled with grey italic text. Grey
# is reserved for them: half the cells in a two-week window land here, so it has
# to read as calm and normal rather than as a warning.
PCTILE_GREY <- "#767676"

# Arm-side normalisation for HB, and the one place the sign is decided.
#
# HB is stored as -pfx_x * 12, which is arm side POSITIVE for a righty and arm
# side NEGATIVE for a lefty. That is right for the movement chart, where the
# actual direction of the break is the whole point, and wrong everywhere a value
# is compared: measured on league_ref, RHP sinkers average +14.9 and LHP sinkers
# -15.1, so two pitchers with identical arm-side run sat at opposite ends of the
# scale and coloured red and blue.
#
# So the COMPARISON surfaces store and show HB arm-side normalised, positive
# means arm side for both hands, and the movement chart converts back with the
# same function before it draws. One helper, used in three places, rather than a
# sign flip written out four times and eventually written out wrong.
arm_side_sign <- function(p_throws) ifelse(p_throws == "R", 1, -1)


# How far from 100 Stuff+ has to sit to reach the end of the ramp.
#
# Stuff+ needs no league reference: it is centred on 100 by construction, so the
# anchor is already in the scale. What it does need is a span, and 25 is where
# the real distribution ends. Measured on the 2026 export, 2,202 pitch grades at
# 20+ IP: the 2nd and 98th percentiles are 75.3 and 131.1, and 94.2% of grades
# sit within 100 +/- 25. Beyond it the fill clamps rather than running off.
STUFF_PLUS_SPAN <- 25

# The unfilled states paint white explicitly rather than painting nothing.
#
# That was originally because a pitch-colour row fill ran under every body cell,
# so "no fill" would have shown the pitch colour through and read as a fill. The
# row fill is gone as of 2026-08-24 and the rows are white anyway, so this is now
# belt and braces rather than load-bearing. It stays explicit because
# resolve_cell() promises every cell a concrete fill, and a state that returned
# NULL would put a conditional back into tables.R that this table exists to keep
# out of it.
PCTILE_UNFILLED <- "#FFFFFF"

# How each of the four cell states renders, as data rather than as conditionals
# spread through league.R and tables.R. resolve_cell() joins against this and
# tables.R passes the columns straight to gt, so neither one re-derives a state.
#
# The four must survive greyscale: these tables get screenshotted into a
# portfolio, and four states separated only by hue collapse into one. They are
# separated on three channels that all survive it.
#
#   filled      splits {exact, fallback} from {below_floor, no_reference}
#   font_weight splits exact from fallback
#   marker/n    splits below_floor from no_reference
#
# Volume drives the weights, and the two rare-vs-common cases pull opposite
# ways. Fallback is 126 cells across all 802 pitchers, so bold plus a dagger
# costs nothing and it should be seen. Below floor is about half the cells in a
# two-week window, so it stays unbolded and unmarked: a half-grey table that
# shouts trains you to ignore it.
#
# Markers are a family with a grammar. The dagger pair says something about the
# LEAGUE reference, single for a coarser one and double for none at all. A
# parenthetical instead says something about the PITCHER's own sample. A reader
# who learns that once can read any cell without the footnote.
CELL_STATE_STYLE <- data.frame(
  state       = c("exact", "fallback",  "below_floor", "no_reference"),
  filled      = c(TRUE,    TRUE,        FALSE,         FALSE),
  text_color  = c("#000000", "#000000", PCTILE_GREY,   PCTILE_GREY),
  font_style  = c("normal", "normal",   "italic",      "italic"),
  font_weight = c("normal", "bold",     "normal",      "normal"),
  # below_floor's marker is built at runtime, since it carries its own n.
  marker      = c("",       "\u2020",   "",            "\u2021"),
  stringsAsFactors = FALSE
)

# Minimum contributing pitchers before a reference cell may be quoted. Below
# this the lookup falls back to a coarser cell. At the full grain the Two
# Strikes cells hold a median of 4 eligible pitchers for whiff%, and a
# percentile off 4 pitchers is a fabrication.
# Every denominator a metric can divide by. One list, so a frame of counts and
# METRIC_SPEC$denom cannot drift apart silently: a denom naming a column nobody
# supplies resolves to no sample at all and greys the metric everywhere.
DENOM_COLS <- c("pitches", "swings", "oz", "pa", "cut_pitches")

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
#
# Bunt attempts are swings. The batter offered, so an out-of-zone bunt attempt
# is a chase and a missed bunt is a swing and a miss. Verified against Savant
# rather than argued, over 105 pitchers with 100+ PA:
#
#   swing set                                  whiff MAE   chase MAE
#   no bunts anywhere                              0.087       0.192
#   bunts are swings, missed_bunt a whiff          0.036       0.060
#   + bunt_foul_tip a whiff too                    0.032       0.060
#
# An earlier pass measured this with foul_tip missing from the whiff numerator
# and read it as a wash, because the two errors ran in opposite directions. One
# change at a time, against the source, or a confounded measurement talks you
# out of a real fix.
#
# swinging_pitchout is left out: 1 row in the whole season, and the measurement
# cannot tell either way.
swing_only <- c("swinging_strike","swinging_strike_blocked","foul","foul_tip","hit_into_play",
                "foul_bunt","missed_bunt","bunt_foul_tip")

# foul_tip belongs here, verified against Savant rather than argued. A foul tip
# is a swing the bat barely reached that the catcher caught, and Savant counts it
# as a whiff. Over 105 pitchers with 100+ PA, misses / swings sat 2.10 points
# below Savant's whiff% on average and 3.4 at worst; adding foul_tip to the
# numerator lands at 0.09 MAE, exact to a tenth for 68 of them. Foul tips are
# 2.1% of swings.
#
# It is in swing_only as well, and has to be: a foul tip is a swing whichever way
# the numerator goes. This set is also the whiff half of csw_pct, so CSW% moves
# with it by design rather than by accident.
whiff_desc <- c("swinging_strike","swinging_strike_blocked","foul_tip",
                "missed_bunt","bunt_foul_tip")
