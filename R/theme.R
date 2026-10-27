# theme.R
# Constants and small lookup helpers shared across R/


# ---- Pitch colors ------------------------------------------------------------

# Same palette as the PDF advance reports
pitch_colors <- c(
  FF = "#FF0000", SI = "#FFA500", CU = "#8B008B", CH = "#228B22",
  SL = "#FFFF00", FS = "#00CED1", FC = "#8B4513", ST = "#DDB100",
  # KC is remapped to CU before lookup
  KC = "#6A0DAD", SV = "#7CFC00",
  KN = "#808080")

# Darker gold for slider and sweeper text on white
pitch_text_colors <- pitch_colors
pitch_text_colors[c("SL", "ST")] <- c("#B59410", "#B59410")


# ---- Pitch code reconciliation -----------------------------------------------

# Savant codes the app doesn't chart: real pitches are mapped, the rest dropped.
# Unknown codes are dropped too. The app reports every change above the tabs.
# KC folds into CU: Savant sometimes gives one pitcher's curveball both labels.
PITCH_CODE_RULES <- data.frame(
  code   = c("CS",   "KC",   "FO",   "EP",   "FA",   "PO",   "UN"),
  action = c("map",  "map",  "map",  "drop", "drop", "drop", "drop"),
  target = c("CU",   "CU",   "FS",   NA,     NA,     NA,     NA),
  reason = c(
    "slow curve; Savant's own player page folds it into the curve",
    "knuckle curve; one curveball, see the note above this table",
    "forkball, a splitter variant; Stuff+ grades fold FO into FS the same way",
    "eephus; 32 of its 35 throwers are position players mopping up",
    "unclassified fastball; 30 of its 33 throwers are position players",
    "pitchout, a play rather than a pitch type",
    "unknown, a tracking failure"
  ),
  stringsAsFactors = FALSE
)


# ---- Batter side labels ------------------------------------------------------

#' Display label for a batter side filter ("All" = no filter)
hand_label <- function(hand) {
  switch(hand,
    R   = "vs RHH",
    L   = "vs LHH",
    All = "vs All Batters",
    stop("unknown hand: ", hand, ". Expected R, L, or All.", call. = FALSE))
}


# ---- Sample floors and smoothing ---------------------------------------------

# Pitch types under both thresholds are left off the charts, not the tables
CHART_MIN_SHARE <- 0.01
CHART_MIN_N     <- 10

# Heat map KDE bandwidth in feet (x, z)
KDE_BW <- c(1.0, 1.2)

# Heat map panels under this many pitches show raw locations, not a density
KDE_MIN_N <- 15


# ---- League reference metrics and their floors -------------------------------

# Metrics that take a league percentile.
#   kind       mean of a physical quantity, or a rate
#   denom      the count the metric divides by (swings, out-of-zone pitches, PA, ...)
#   floor      minimum denominator for a full sample, from a resampling study
#   direction  high / low (inverted) / neutral (grey ramp) / extreme (distance from median)
# ivb, hb and vaa take direction per pitch type (PITCH_SHAPE_DIRECTION); their
# entries here are placeholders. Always use metric_direction().
METRIC_SPEC <- data.frame(
  metric = c("velo", "ivb", "hb", "vaa", "spin", "ext", "rel_ht", "rel_side",
             "strike_pct", "csw_pct", "zone_pct", "usage_pct",
             "whiff_pct", "chase_pct", "xwoba", "gb_pct", "z_whiff_pct"),
  kind   = c(rep("mean", 8), rep("rate", 9)),
  # usage floors on the pitcher's total pitches in the cut
  denom  = c(rep("pitches", 8),
             "pitches", "pitches", "pitches", "cut_pitches",
             "swings", "oz", "pa", "bbe", "z_swings"),
  floor  = c(rep(25, 8),
             rep(50, 4),
             50, 50, 50, 25, 60),
  # Pitches before a trait's percentile is stable (Context tab only)
  shape_floor = c(13, 31, 50, 19, 14, 10, 4, 4,
                  rep(NA, 4),
                  rep(NA, 5)),
  # Better end for the pitcher on the Context tab ("none" for shape traits)
  context_better = c("high", "none", "none", "none", "none", "high", "none", "none",
                     "high", "high", "none", "none",
                     "high", "high", "low",  "high", "high"),
  direction = c("high", "high", "high", "high", "high", "high",
                "extreme", "extreme",
                "high", "high", "high", "neutral",
                "high", "high", "low", "high", "high"),
  stringsAsFactors = FALSE
)

# Column footnotes
METRIC_NOTES <- c(
  hb = paste("HB is the true direction, as on the movement chart: positive is",
             "arm side for a righty and glove side for a lefty. Percentiles mirror",
             "lefties, so each is ranked by arm-side run."),
  rel_side = paste("Rel Side is measured from the middle of the rubber,",
                   "positive toward third base, so a righty reads positive and",
                   "a lefty negative. Percentiles are within hand, so each is",
                   "ranked against arms on his own side."),
  rel_ht = paste("Rel Ht and Rel Side are shaded by how FAR from league average",
                 "they sit, not by which way. Both tails fill the same, because",
                 "an unusual slot is the thing that plays; league-average is the",
                 "pale end. The value beside the shading says which way.")
)

# Shape direction by pitch type: ride helps a four-seam and hurts a sinker.
# No default: an unlisted code stops the render.
#   ivb / vaa  high = wants ride (flatter)   FF FC;  low = wants drop   the rest
#   hb         high = wants arm side  FF SI CH FS;   low = glove side   the rest
#   KN is neutral on all three.
PITCH_SHAPE_DIRECTION <- list(
  ivb = c(FF = "high", FC = "high",
          SI = "low",  CH = "low", FS = "low", SL = "low",
          ST = "low",  CU = "low", KC = "low", SV = "low",
          KN = "neutral"),
  hb  = c(FF = "high", SI = "high", CH = "high", FS = "high",
          FC = "low",  SL = "low",  ST = "low",  CU = "low",
          KC = "low",  SV = "low",
          KN = "neutral"),
  vaa = c(FF = "high", FC = "high",
          SI = "low",  CH = "low", FS = "low", SL = "low",
          ST = "low",  CU = "low", KC = "low", SV = "low",
          KN = "neutral")
)


#' The direction a metric runs for one pitch type
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


#' Characteristics columns that take a league fill -> METRIC_SPEC metric.
#' RV is a counting stat and takes none.
ARSENAL_METRIC_COLS <- c(
  # Traits
  velocity   = "velo",
  ivb        = "ivb",
  hb         = "hb",
  vaa        = "vaa",
  spin       = "spin",
  ext        = "ext",
  rel_ht     = "rel_ht",
  rel_side   = "rel_side",
  # Results
  strike_pct = "strike_pct",
  whiff_pct  = "whiff_pct",
  z_whiff_pct = "z_whiff_pct",
  csw_pct    = "csw_pct",
  zone_pct   = "zone_pct",
  chase_pct  = "chase_pct",
  xwoba      = "xwoba",
  gb_pct     = "gb_pct"
)


# ---- Results panel league context --------------------------------------------

# Results panel metrics. sc = Statcast, gl = game logs. The floor scales with
# the window except for IP, whose denominator is games.
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

# Results floor: this fraction of the median denominator in the window
RESULTS_FLOOR_FRACTION <- 0.5


# ---- Pitch trait search ------------------------------------------------------

# Search sliders (traits only, never outcomes)
SEARCH_TRAITS <- data.frame(
  trait  = c("velo",  "ivb",  "hb",   "spin",  "ext",  "rel_ht"),
  label  = c("Velocity (mph)", "IVB (in)", "HB (in)", "Spin (rpm)",
             "Extension (ft)", "Rel Height (ft)"),
  step   = c(0.1,     0.5,    0.5,    25,      0.1,    0.05),
  digits = c(1L,      1L,     1L,     0L,      1L,     2L),
  stringsAsFactors = FALSE
)

# Rows drawn in the search table; the count line reports the full total
SEARCH_MAX_ROWS <- 50L


# ---- Percentile fill palettes ------------------------------------------------

# Diverging (blue poor, red plus) for graded metrics; symmetric grey for neutral.
# Both share the middle stop and are light enough for black text.
PCTILE_PAL_DIVERGING <- c("#7FA8D6", "#BFD3E8", "#F0F0F0", "#EFC0AE", "#DC8163")
PCTILE_PAL_NEUTRAL   <- c("#C4C4C4", "#DEDEDE", "#F0F0F0", "#DEDEDE", "#C4C4C4")

# Text color for cells with no league reference
PCTILE_GREY <- "#767676"

# HB is arm side positive for a RHP only. Multiply by this to compare across hands.
arm_side_sign <- function(p_throws) ifelse(p_throws == "R", 1, -1)


# IVB and HB shade by inches from the league average, saturating at this many
# inches (about the 95th percentile of differences)
MAGNITUDE_METRICS <- c("ivb", "hb")
SHAPE_DELTA_SPAN  <- 6

# Stuff+ fill saturates at 100 +/- this
STUFF_PLUS_SPAN <- 25

PCTILE_UNFILLED <- "#FFFFFF"

# How each cell state renders. Exact and fallback look the same; the other two are
# marked without relying on hue.
#   exact         league cell at the full grain
#   fallback      coarser league cell
#   below_floor   shaded, with its own sample size in parentheses
#   no_reference  no league cell at all (double dagger, grey italic)
CELL_STATE_STYLE <- data.frame(
  state       = c("exact", "fallback",  "below_floor", "no_reference"),
  filled      = c(TRUE,    TRUE,        TRUE,          FALSE),
  text_color  = c("#000000", "#000000", "#000000",     PCTILE_GREY),
  font_style  = c("normal", "normal",   "normal",      "italic"),
  font_weight = c("normal", "normal",   "normal",      "normal"),
  # below_floor's marker is its own n, added at runtime
  marker      = c("",       "",         "",            "‡"),
  stringsAsFactors = FALSE
)

# Every denominator METRIC_SPEC$denom can name
DENOM_COLS <- c("pitches", "swings", "oz", "pa", "cut_pitches", "bbe", "z_swings")

# Both characteristics tables render at this total width
TABLE_WIDTH_PX <- 960L

# Shown as COUNT already, so no (n) marker for this denominator
DENOM_SHOWN_AS_COUNT <- "pitches"

# Minimum pitchers in a league cell before it is used
MIN_REF_PITCHERS <- 20

# Six overlapping count buckets (situational views, not a partition)
COUNT_BUCKETS <- list(
  "All Counts"      = NULL,
  "Early Count"     = c("0-0","0-1","1-0"),
  "Pitcher Ahead"   = c("0-1","0-2","1-2","2-2"),
  "Pitcher Behind"  = c("1-0","2-0","3-0","2-1","3-1"),
  "Pre Two Strikes" = c("0-0","0-1","1-0","1-1","2-1","3-1"),
  "Two Strikes"     = c("0-2","1-2","2-2","3-2")
)


# ---- Outcome description sets ------------------------------------------------

# Savant `description` values, matched exactly
# Bunt attempts are swings; foul tips and missed bunts are whiffs (matches Savant).
swing_only <- c("swinging_strike","swinging_strike_blocked","foul","foul_tip","hit_into_play",
                "foul_bunt","missed_bunt","bunt_foul_tip")

# Also the whiff half of CSW%
whiff_desc <- c("swinging_strike","swinging_strike_blocked","foul_tip",
                "missed_bunt","bunt_foul_tip")


#' Pitches before a shape trait's percentile is stable (NA = no floor)
shape_floor <- function(metric) {
  spec <- METRIC_SPEC[METRIC_SPEC$metric == metric, ]
  if (nrow(spec) != 1) stop("unknown metric: ", metric, call. = FALSE)
  spec$shape_floor
}


#' Better end of a metric for the pitcher on the Context tab
context_better <- function(metric) {
  spec <- METRIC_SPEC[METRIC_SPEC$metric == metric, ]
  if (nrow(spec) != 1) stop("unknown metric: ", metric, call. = FALSE)
  spec$context_better
}
