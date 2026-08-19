# features.R
#
# Pitch-level construction. Turns the season-wide Statcast frame into the
# one-pitcher, trimmed frame every plot and table downstream expects.
#
# Requires theme.R for MIN_PITCH_COUNT.

library(dplyr)


#' Row-wise derived columns
#'
#' Depends only on the pitch itself, never on which other pitches are in the
#' frame, so this runs once over every pitcher when app_data.rds is built rather
#' than per query. Idempotent, so re-running it is harmless.
#'
#' Savant reports break in feet from the catcher's view. The sign flip on pfx_x
#' is what makes arm side read positive, per CLAUDE.md.
add_pitch_features <- function(df) {
  df |>
    mutate(
      hb = -pfx_x * 12,
      ivb = pfx_z * 12,
      in_zone = as.integer(plate_x >= -0.8291 & plate_x <= 0.8291 &
                             plate_z >= sz_bot & plate_z <= sz_top)
    )
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
  pl <- df |>
    filter(!is.na(pitch_type), pitch_type != "") |>
    add_count(pitch_type, name = "pt_n") |>
    filter(pt_n >= MIN_PITCH_COUNT)
  ord <- pl |> count(pitch_type, sort = TRUE) |> pull(pitch_type)
  pl |> mutate(pitch_type = factor(pitch_type, levels = ord))
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
#' This is the analysis contract and is deliberately wider than the deploy
#' artifact. Trajectory (vx0 through az, plus release_pos_y for the 50 ft
#' reference) is kept because VAA is a standing formula convention and six
#' doubles are cheap. If app_data.rds later exceeds the shinyapps.io ceiling it
#' gets trimmed in Phase 6 against a measured file size, not pre-emptively.
PL_TRIM_COLS <- c(
  "pitcher", "player_name", "game_date", "pitch_type", "pt_n",
  "stand", "p_throws", "balls", "strikes",
  "release_speed", "release_extension", "release_spin_rate", "pfx_x", "pfx_z", "arm_angle",
  "release_pos_x", "release_pos_z",
  "plate_x", "plate_z", "sz_bot", "sz_top", "hb", "ivb", "in_zone",
  "type", "description", "events", "bb_type", "launch_speed",
  "estimated_woba_using_speedangle", "woba_denom",
  "vx0", "vy0", "vz0", "ax", "ay", "az", "release_pos_y"
)


#' The app_data.rds column set
#'
#' PL_TRIM_COLS minus pt_n. pt_n is the one member of the contract that cannot
#' be stored, because it is a function of the selected window rather than of the
#' pitch. See shape_arsenal(), which recomputes it per query.
APP_DATA_COLS <- setdiff(PL_TRIM_COLS, "pt_n")


#' Trim a pitch-level frame to the contract
#'
#' all_of() rather than any_of() on purpose. A column missing from the upstream
#' pull should error here, while the pull is still the obvious suspect, rather
#' than surfacing as an empty chart panel several functions later.
trim_pitch_level <- function(pl) {
  pl |> select(all_of(PL_TRIM_COLS))
}
