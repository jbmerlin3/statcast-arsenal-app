# features.R
# Pitch-level columns: zone flag, movement, VAA, pitching team, pitch codes

library(dplyr)


# Zone edges include the ball radius on both axes (any part of the ball counts):
# half the 17 in plate (0.7083 ft) + 0.1208 ft
PLATE_HALF_FT  <- 0.7083
BALL_RADIUS_FT <- 0.1208
ZONE_HALF_FT   <- PLATE_HALF_FT + BALL_RADIUS_FT   # 0.8291


#' Strike zone membership (0/1), using the batter's own vertical zone.
#' Also used by scripts/update_data.R.
in_zone_flag <- function(plate_x, plate_z, sz_bot, sz_top) {
  as.integer(plate_x >= -ZONE_HALF_FT & plate_x <= ZONE_HALF_FT &
               plate_z >= (sz_bot - BALL_RADIUS_FT) &
               plate_z <= (sz_top + BALL_RADIUS_FT))
}


#' Row-wise derived columns: hb, ivb, in_zone, vaa, pitch_team. Idempotent.
#' HB is -pfx_x * 12 (arm side positive for a right-hander).
add_pitch_features <- function(df) {
  df <- df |>
    mutate(
      hb = -pfx_x * 12,
      ivb = pfx_z * 12,
      in_zone = in_zone_flag(plate_x, plate_z, sz_bot, sz_top)
    )

  # VAA at the front of the plate (17/12 ft), then location-adjusted.
  # Needs the trajectory columns, which only the season store has.
  if (all(c("vy0", "vz0", "ay", "az") %in% names(df))) {
    yf  <- 17 / 12
    tf  <- (-df$vy0 - sqrt(df$vy0^2 - 2 * df$ay * (50 - yf))) / df$ay
    df$vaa <- atan((df$vz0 + df$az * tf) / abs(df$vy0 + df$ay * tf)) * 180 / pi
    df$vaa <- adjust_vaa_for_location(df)
  }

  # Top of the inning means the home team is pitching (season store only)
  if (all(c("inning_topbot", "home_team", "away_team") %in% names(df))) {
    df$pitch_team <- ifelse(df$inning_topbot == "Top", df$home_team, df$away_team)
  }
  df
}


# Arm angles 30+ degrees off a pitcher's median with an unchanged release point
# are recording errors (a real slot change moves the release point too)
ARM_ANGLE_MAX_DEV     <- 30    # degrees from the pitcher's median
ARM_ANGLE_SAME_RELEASE <- 0.5  # ft from his median release point

#' Blank impossible arm angles, against each pitcher's median over the frame
#' passed in (the full season, in the chain). The pitch is kept.
clean_arm_angle <- function(df) {
  # Base R keeps a data.frame a data.frame
  med <- function(x) ave(x, df$pitcher, FUN = function(v) median(v, na.rm = TRUE))
  dev   <- abs(df$arm_angle - med(df$arm_angle))
  shift <- sqrt((df$release_pos_x - med(df$release_pos_x))^2 +
                (df$release_pos_z - med(df$release_pos_z))^2)
  bad <- !is.na(dev) & dev > ARM_ANGLE_MAX_DEV & !is.na(shift) & shift < ARM_ANGLE_SAME_RELEASE
  df$arm_angle[bad] <- NA_real_
  df
}


# Pitch types with fewer pitches keep raw VAA
VAA_ADJ_MIN_N <- 2000


#' Location-adjusted VAA: per pitch type, the residual from a quadratic in
#' plate_z plus the fit at that type's mean plate height (still in degrees).
adjust_vaa_for_location <- function(df) {
  out <- df$vaa
  if (!"plate_z" %in% names(df)) return(out)
  ok <- is.finite(df$vaa) & is.finite(df$plate_z)
  pt <- as.character(df$pitch_type)
  # Fit on reconciled codes (KC, CS -> CU; FO -> FS)
  maps <- PITCH_CODE_RULES[PITCH_CODE_RULES$action == "map", ]
  hit  <- match(pt, maps$code)
  pt[!is.na(hit)] <- maps$target[hit[!is.na(hit)]]
  for (p in unique(pt[ok])) {
    i <- which(ok & pt == p)
    if (length(i) < VAA_ADJ_MIN_N) next
    d   <- data.frame(vaa = df$vaa[i], plate_z = df$plate_z[i])
    fit <- stats::lm(vaa ~ poly(plate_z, 2), data = d)
    ref <- as.numeric(stats::predict(fit, newdata = data.frame(plate_z = mean(d$plate_z))))
    out[i] <- d$vaa - stats::predict(fit) + ref
  }
  out
}


#' Remap or drop pitch codes the app can't chart. The changes ride along as an
#' attribute for the note above the tabs.
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

  stopifnot("reconcile_pitch_codes let an unknown code through" =
              all(df$pitch_type %in% known))

  empty <- data.frame(code = character(), action = character(),
                      target = character(), n = integer(), stringsAsFactors = FALSE)
  attr(df, "pitch_codes") <- if (length(log)) do.call(rbind, log) else empty
  df
}


#' One-line summary of the pitch code changes, or NULL
pitch_code_note <- function(x) {
  rep <- attr(x, "pitch_codes")
  if (is.null(rep) || !nrow(rep)) return(NULL)
  paste(ifelse(rep$action == "remapped",
               sprintf("%d %s remapped to %s", rep$n, rep$code, rep$target),
               sprintf("%d %s dropped", rep$n, rep$code)),
        collapse = ". ")
}


#' One pitcher's window: reconcile codes, count each pitch type (pt_n) and order
#' by usage. pt_n depends on the window and is never stored.
shape_arsenal <- function(df) {
  df  <- df |> filter(!is.na(pitch_type), pitch_type != "")
  df  <- reconcile_pitch_codes(df)
  rep <- attr(df, "pitch_codes")

  # Every pitch type is kept; rate floors handle small samples
  pl <- df |> add_count(pitch_type, name = "pt_n")
  ord <- pl |> count(pitch_type, sort = TRUE) |> pull(pitch_type)
  out <- pl |> mutate(pitch_type = factor(pitch_type, levels = ord))

  # dplyr drops attributes
  attr(out, "pitch_codes") <- rep
  out
}


#' Pitch-level frame for one pitcher, for console and report use
build_pitch_level <- function(df, mlb_id) {
  one <- df |> filter(pitcher == mlb_id)
  if (nrow(one) == 0) stop("No pitches found for id ", mlb_id)
  one |> shape_arsenal() |> add_pitch_features()
}


# Analysis column set (wider than what ships; see APP_DATA_COLS)
PL_TRIM_COLS <- c(
  "pitcher", "player_name", "game_date", "pitch_type", "pt_n", "pitch_team",
  "stand", "p_throws", "balls", "strikes", "n_thruorder_pitcher",
  "release_speed", "release_extension", "release_spin_rate", "pfx_x", "pfx_z", "arm_angle",
  "release_pos_x", "release_pos_z",
  "plate_x", "plate_z", "sz_bot", "sz_top", "hb", "ivb", "vaa", "in_zone",
  "type", "description", "events", "bb_type", "launch_speed",
  "estimated_woba_using_speedangle", "woba_denom", "delta_pitcher_run_exp",
  "vx0", "vy0", "vz0", "ax", "ay", "az", "release_pos_y"
)


# Trajectory columns; only the derived vaa ships
APP_DATA_UNREAD <- c(
  "vx0", "vy0", "vz0", "ax", "ay", "az", "release_pos_y"
)

# Columns stored in app_data.rds
APP_DATA_COLS <- setdiff(PL_TRIM_COLS, c("pt_n", APP_DATA_UNREAD))

