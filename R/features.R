# features.R
#
# Pitch-level construction. Turns the season-wide Statcast frame into the
# one-pitcher, trimmed frame every plot and table downstream expects.
#
# Requires theme.R for MIN_PITCH_COUNT.

library(dplyr)


# The zone a pitch is called in, in feet. A pitch is a strike if ANY part of the
# ball crosses the zone, so both bounds carry a ball radius. Half of a 17 inch
# plate is 0.7083 ft and the ball's radius is 0.1208 ft, which is where 0.8291
# comes from.
#
# The vertical bound carried no radius until 2026-08-23, and that was the bug:
# the same allowance was in one axis and not the other. Measured against
# Savant's own zone% and chase% over 105 pitchers with 100+ PA:
#
#   vertical pad   zone% MAE   zone% bias   chase% MAE
#   0 ft (before)      4.646       -4.646        2.193
#   0.1208 ft          0.105       +0.098        0.192
#
# A grid search over half width and pad lands on the same 0.12, so this is the
# ball radius and not a fitted fudge factor.
PLATE_HALF_FT  <- 0.7083
BALL_RADIUS_FT <- 0.1208
ZONE_HALF_FT   <- PLATE_HALF_FT + BALL_RADIUS_FT   # 0.8291


#' Strike zone membership, as a 0/1 integer
#'
#' Vertical bounds are the batter-specific zone Savant reports per pitch, not a
#' constant, widened by the ball radius the same way the half-width is.
#'
#' Its own function because the season store carries `in_zone` too and
#' `scripts/update_data.R` has to recompute it. Two copies of this geometry in
#' two files is exactly how a rate stat drifts from itself. The drawn zone box in
#' plot_heatmap() is deliberately NOT this: that one is a rendering coordinate
#' and must not move a rate.
in_zone_flag <- function(plate_x, plate_z, sz_bot, sz_top) {
  as.integer(plate_x >= -ZONE_HALF_FT & plate_x <= ZONE_HALF_FT &
               plate_z >= (sz_bot - BALL_RADIUS_FT) &
               plate_z <= (sz_top + BALL_RADIUS_FT))
}


#' Row-wise derived columns
#'
#' Depends only on the pitch itself, never on which other pitches are in the
#' frame, so this runs once over every pitcher when app_data.rds is built rather
#' than per query. Idempotent, so re-running it is harmless.
#'
#' Savant reports break in feet from the catcher's view. The sign flip on pfx_x
#' is what makes arm side read positive, per CLAUDE.md.
add_pitch_features <- function(df) {
  df <- df |>
    mutate(
      hb = -pfx_x * 12,
      ivb = pfx_z * 12,
      in_zone = in_zone_flag(plate_x, plate_z, sz_bot, sz_top)
    )

  # VAA, the vertical angle the ball crosses the front of the plate at, in
  # degrees and always negative. Savant ships no such column and has no name for
  # it: all 120 columns of the season store were checked on 2026-08-31. It is
  # derived from the trajectory primitives, which live in the season store and
  # NOT in app_data, so this takes the pitch_team pattern below rather than
  # in_zone's. Deriving it here is exactly what lets the seven primitives stay
  # off the deploy artifact while the one number that reads them ships.
  #
  # Same formula as sc_add_trajectory() in 02_StuffPlus/scripts/stuffv4.R. Kept
  # in step by hand, which is tolerable only because it is four lines of physics
  # with no fitted constant in it. If either side gains a constant, this becomes
  # a shared file instead.
  #
  # yf is the front of the plate, 17/12 ft. A FIXED reference plane, so this is a
  # property of the pitch rather than of where it ended up. It still moves with
  # location through the trajectory, which is the known confound: the same
  # fastball crosses flatter at the top of the zone than at the knees.
  #
  # Deliberately NOT residualised on plate_z here. sc_fit_vaa_adj() in stuffv4.R
  # does that, per pitch type on a quadratic, and its output is already priced
  # into the Stuff+ column that sits two cells away in the traits table. The raw
  # angle is the scouting datapoint; the residual is the grade. Both are on the
  # page and they are answering different questions.
  if (all(c("vy0", "vz0", "ay", "az") %in% names(df))) {
    yf  <- 17 / 12
    tf  <- (-df$vy0 - sqrt(df$vy0^2 - 2 * df$ay * (50 - yf))) / df$ay
    df$vaa <- atan((df$vz0 + df$az * tf) / abs(df$vy0 + df$ay * tf)) * 180 / pi
  }

  # The pitching team, derived rather than looked up. Top of the inning means
  # the away team is batting, so the HOME team is on the mound. Resolves every
  # row of the 2026 store, 572,181 of them, across 30 teams.
  #
  # Conditional, and that is not laziness. The three source columns live in the
  # season store and NOT in app_data: shipping home_team, away_team and
  # inning_topbot to keep one derived column would be three columns to save one.
  # So this computes where the sources exist and leaves an already-derived
  # column alone everywhere else, which keeps add_pitch_features() idempotent on
  # app_data the way it already is for in_zone. in_zone gets away with
  # recomputing because ITS sources are in app_data; this one cannot.
  if (all(c("inning_topbot", "home_team", "away_team") %in% names(df))) {
    df$pitch_team <- ifelse(df$inning_topbot == "Top", df$home_team, df$away_team)
  }
  df
}


#' Remap or drop pitch codes this app cannot chart
#'
#' The single choke point. Every code that reaches a plotting or table function
#' has passed through here, so pitch_colors[[pt]] cannot fail.
#'
#' Runs BEFORE add_count() in shape_arsenal(), so a remapped code counts toward
#' its target for both pt_n and the usage ordering. Freddy Peralta's 39 CS join
#' his 292 CU as one 331-pitch curve rather than two rows.
#'
#' Anything neither in PITCH_CODE_RULES nor in pitch_colors is dropped, not
#' passed through. A code Savant introduces next season therefore degrades to a
#' visible note instead of crashing three tabs.
#'
#' The report rides as an attribute rather than a column, because it is a fact
#' about the frame and not about any row, and as an attribute rather than a
#' message() because this runs on every reactive invalidation.
reconcile_pitch_codes <- function(df) {
  known <- names(pitch_colors)
  pt    <- as.character(df$pitch_type)
  log   <- list()

  maps <- PITCH_CODE_RULES[PITCH_CODE_RULES$action == "map", ]
  for (i in seq_len(nrow(maps))) {
    hit <- pt == maps$code[i]
    if (any(hit)) {
      log[[length(log) + 1]] <- data.frame(code = maps$code[i], action = "remapped",
                                           target = maps$target[i], n = sum(hit),
                                           stringsAsFactors = FALSE)
      pt[hit] <- maps$target[i]
    }
  }

  bad <- !pt %in% known
  if (any(bad)) {
    for (code in sort(unique(pt[bad]))) {
      log[[length(log) + 1]] <- data.frame(code = code, action = "dropped",
                                           target = NA_character_, n = sum(pt == code),
                                           stringsAsFactors = FALSE)
    }
  }

  df$pitch_type <- pt
  df <- df[!bad, , drop = FALSE]

  # The guarantee, asserted rather than assumed. If this ever fires, a code got
  # past the rules and the fix belongs here, not in a downstream colour lookup.
  stopifnot("reconcile_pitch_codes let an unknown code through" =
              all(df$pitch_type %in% known))

  empty <- data.frame(code = character(), action = character(),
                      target = character(), n = integer(), stringsAsFactors = FALSE)
  attr(df, "pitch_codes") <- if (length(log)) do.call(rbind, log) else empty
  df
}


#' One-line summary of what reconcile_pitch_codes() did, or NULL
#'
#' NULL when nothing was touched, so callers can skip rendering entirely.
pitch_code_note <- function(x) {
  rep <- attr(x, "pitch_codes")
  if (is.null(rep) || !nrow(rep)) return(NULL)
  paste(ifelse(rep$action == "remapped",
               sprintf("%d %s remapped to %s", rep$n, rep$code, rep$target),
               sprintf("%d %s dropped", rep$n, rep$code)),
        collapse = ". ")
}


#' Per-pitcher, per-window shaping
#'
#' Everything here depends on which rows are present, so none of it can be
#' precomputed and stored.
#'
#' pt_n is the reason. It counts a pitch type within the frame it is given, so
#' it is a function of the selected date window, not a property of the pitch. A
#' stored season-wide pt_n would let a two-week view keep a pitch type that has
#' 200 pitches on the season and one in the window. The usage ordering has the
#' same dependence, which is why the existing pre-break and post-break reports
#' legitimately show different pitch orders.
#'
#' Rare types are dropped before any rate is computed, since a 3-pitch sample
#' produces rate columns that read as real. The returned factor ordering fixes
#' panel and legend order in every downstream plot, so it is load bearing rather
#' than cosmetic.
#'
#' Expects a frame already narrowed to one pitcher and one window.
shape_arsenal <- function(df) {
  df  <- df |> filter(!is.na(pitch_type), pitch_type != "")
  df  <- reconcile_pitch_codes(df)
  rep <- attr(df, "pitch_codes")

  # EVERY pitch type this app can chart is kept, however few pitches it has.
  #
  # There used to be a filter(pt_n >= MIN_PITCH_COUNT) here, dropping the ROWS
  # of any type under five pitches in the window. On a season it is invisible.
  # On one start it is a lie: Didier Fuentes on 2026-08-05 threw 13 four-seams,
  # 3 splitters and 3 sliders, and the app reported 100% four-seam, because the
  # six secondary pitches were gone before usage was computed. It also read
  # 0 batters faced, since all four of his PA-ending pitches happened to be
  # those splitters and sliders.
  #
  # Dropping rows was always the wrong instrument. theme.R says it outright:
  # MIN_PITCH_COUNT decided whether a pitch was SHOWN, while METRIC_SPEC's
  # per-metric floors decide whether a RATE is trustworthy, "and the two are
  # deliberately separate constants". Only the second concern is real. A
  # three-pitch slider has an honest count, an honest velocity and an honest
  # location; what it does not have is a meaningful whiff rate, and the
  # below_floor state already greys exactly those cells and prints their
  # denominator.
  #
  # So the count is always true and the rates police themselves.
  pl <- df |> add_count(pitch_type, name = "pt_n")
  ord <- pl |> count(pitch_type, sort = TRUE) |> pull(pitch_type)
  out <- pl |> mutate(pitch_type = factor(pitch_type, levels = ord))

  # dplyr drops attributes, so the report has to be re-attached after the
  # pipeline rather than surviving it.
  attr(out, "pitch_codes") <- rep
  out
}


#' Build the pitch-level frame for one pitcher
#'
#' The console and report entry point, unchanged in signature and output. It is
#' now the composition of the two halves above, so the app and the console run
#' the same code rather than two implementations that can drift.
#'
#' Composition order is deliberate. shape_arsenal() first, then features,
#' reproduces the original column order [..., pt_n, hb, ivb, in_zone]. The
#' reverse is numerically identical and silently reorders columns.
build_pitch_level <- function(df, mlb_id) {
  one <- df |> filter(pitcher == mlb_id)
  if (nrow(one) == 0) stop("No pitches found for id ", mlb_id)
  one |> shape_arsenal() |> add_pitch_features()
}


#' The pl_trim column contract
#'
#' The source script carried two different versions of this select, one with
#' release position and trajectory and one without. Downstream code was written
#' against whichever happened to be in the session, which is the re-paste bug
#' class the split exists to close. This vector is the single definition.
#'
#' This is the ANALYSIS contract and is deliberately wider than the deploy
#' artifact. Trajectory (vx0 through az, plus release_pos_y for the 50 ft
#' reference) is kept because VAA is a standing formula convention, and release
#' point because reports use it ad hoc. Both stay available from the console.
#'
#' They no longer ship. Phase 7 measured the cost: the ten columns below are
#' 27.0 MB of the 49.2 MB file and 42.4 MB of the 144.3 MB resident footprint,
#' and nothing in R/, scripts/ or app.R reads any of them. See APP_DATA_COLS.
PL_TRIM_COLS <- c(
  "pitcher", "player_name", "game_date", "pitch_type", "pt_n", "pitch_team",
  "stand", "p_throws", "balls", "strikes",
  "release_speed", "release_extension", "release_spin_rate", "pfx_x", "pfx_z", "arm_angle",
  "release_pos_x", "release_pos_z",
  "plate_x", "plate_z", "sz_bot", "sz_top", "hb", "ivb", "vaa", "in_zone",
  "type", "description", "events", "bb_type", "launch_speed",
  "estimated_woba_using_speedangle", "woba_denom",
  "vx0", "vy0", "vz0", "ax", "ay", "az", "release_pos_y"
)


#' The app_data.rds column set
#'
#' PL_TRIM_COLS minus pt_n and minus the ten columns nothing reads.
#'
#' pt_n cannot be stored at all: it is a function of the selected window rather
#' than of the pitch. See shape_arsenal(), which recomputes it per query.
#'
#' The other ten are readable, just unread. Measured 2026-08-20 by scanning every
#' .R in R/, scripts/ and app.R with this contract and PL_TRIM_COLS excluded:
#' zero references outside the contract itself. Dropping them takes app_data.rds
#' from 37 columns to 27, 49.2 MB to 22.2 MB on disk, 144.3 MB to 101.9 MB
#' resident.
#'
#' They are dropped from the DEPLOY artifact only. PL_TRIM_COLS still carries
#' them, so a console session keeps trajectory. Putting one back is a one-line
#' edit here plus a chain rerun.
#'
#' Two that look droppable and are not: arm_angle is read by plot_movement(),
#' and launch_speed backs HH% in the results panel.
#'
#' AMENDED 2026-08-31, when the traits table gave release point a reader. The
#' original note said these ten were "readable, just unread", and that was the
#' whole justification for dropping them, so the moment two of them are read the
#' justification expires for those two rather than being argued around.
#'
#' release_pos_x and release_pos_z came back. The seven trajectory columns did
#' NOT: add_pitch_features() now derives vaa from them and vaa is what ships, so
#' the artifact pays for one column instead of seven. That is the whole reason
#' the derivation lives in features.R rather than in tables.R.
APP_DATA_UNREAD <- c(
  # Trajectory. vaa is derived from these in add_pitch_features() and ships in
  # their place; nothing else reads them. Putting them back costs ~19 MB.
  "vx0", "vy0", "vz0", "ax", "ay", "az", "release_pos_y",
  # bb_type. CLAUDE.md documents its empty-string trap, but nothing reads it.
  "bb_type"
)

APP_DATA_COLS <- setdiff(PL_TRIM_COLS, c("pt_n", APP_DATA_UNREAD))


#' Trim a pitch-level frame to the contract
#'
#' all_of() rather than any_of() on purpose. A column missing from the upstream
#' pull should error here, while the pull is still the obvious suspect, rather
#' than surfacing as an empty chart panel several functions later.
trim_pitch_level <- function(pl) {
  pl |> select(all_of(PL_TRIM_COLS))
}
