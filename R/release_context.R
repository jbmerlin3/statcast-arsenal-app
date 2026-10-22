# release_context.R
# Release point for the Context tab: arm slot, release height against slot and
# listed height, release point rarity, and league percentiles in words.

#' Listed height per MLBAM id (pitcher, height_in), or NULL if the lookup is missing
load_pitcher_heights <- function(path = NULL, year = 2026) {
  if (is.null(path)) path <- file.path("lookups", sprintf("pitcher_heights_%d.csv", year))
  if (!file.exists(path)) {
    warning("No height lookup at ", path,
            ". Release-context residuals will be unavailable. ",
            "Build it with: Rscript scripts/build_pitcher_heights.R", call. = FALSE)
    return(NULL)
  }
  h <- utils::read.csv(path, stringsAsFactors = FALSE)
  need <- c("mlbam_id", "height_in")
  if (!all(need %in% names(h))) {
    warning("Height lookup is missing ", paste(setdiff(need, names(h)), collapse = ", "),
            call. = FALSE)
    return(NULL)
  }
  data.frame(pitcher = as.integer(h$mlbam_id), height_in = as.numeric(h$height_in),
             stringsAsFactors = FALSE) |>
    subset(is.finite(height_in) & !duplicated(pitcher))
}


#' One row per pitcher: mean release point, extension, arm angle and height.
#' `n_arm` counts non-missing arm angles (Savant fills them in late).
pitcher_release_profile <- function(df, heights = NULL, min_pitches = 100) {
  stopifnot(is.data.frame(df))
  need <- c("pitcher", "player_name", "p_throws", "release_pos_x",
            "release_pos_z", "release_extension", "arm_angle")
  missing <- setdiff(need, names(df))
  if (length(missing)) stop("pitcher_release_profile needs ", paste(missing, collapse = ", "),
                            call. = FALSE)

  out <- df |>
    dplyr::group_by(pitcher, player_name, p_throws) |>
    dplyr::summarise(
      n       = dplyr::n(),
      n_arm   = sum(!is.na(arm_angle)),
      rel_x   = mean(release_pos_x, na.rm = TRUE),
      rel_z   = mean(release_pos_z, na.rm = TRUE),
      ext     = mean(release_extension, na.rm = TRUE),
      arm     = mean(arm_angle, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::filter(n >= min_pitches)

  if (!is.null(heights)) out <- dplyr::left_join(out, heights, by = "pitcher")
  else out$height_in <- NA_real_

  # Positive toward third base, as in the traits table
  out$rel_side <- -out$rel_x
  out
}


#' Release height against slot and listed height, fit per hand as
#' rel_z ~ arm angle + height. Returns the residual, a band for
#' +/- `height_error_in` inches of listed-height error, and a flag for arm angles
#' with fewer than `support_min` same-hand pitchers within `support_window` degrees.
expected_release_height <- function(profile, height_error_in = 1,
                                    support_window = 5, support_min = 10) {
  usable <- is.finite(profile$rel_z) & is.finite(profile$arm) & is.finite(profile$height_in)
  profile$exp_rel_z  <- NA_real_
  profile$rel_z_resid <- NA_real_
  profile$resid_sd   <- NA_real_
  profile$ht_coef    <- NA_real_

  for (h in unique(profile$p_throws[usable])) {
    idx <- usable & profile$p_throws == h
    # Too few pitchers to fit
    if (sum(idx) < 30) next
    d  <- data.frame(rel_z = profile$rel_z[idx], arm = profile$arm[idx],
                     ht_ft = profile$height_in[idx] / 12)
    m  <- stats::lm(rel_z ~ arm + ht_ft, data = d)
    profile$exp_rel_z[idx]   <- stats::fitted(m)
    profile$rel_z_resid[idx] <- stats::residuals(m)
    profile$resid_sd[idx]    <- stats::sd(stats::residuals(m))
    profile$ht_coef[idx]     <- unname(stats::coef(m)[["ht_ft"]])
  }

  shift <- abs(profile$ht_coef) * (height_error_in / 12)
  profile$resid_lo <- profile$rel_z_resid - shift
  profile$resid_hi <- profile$rel_z_resid + shift

  # Same-hand pitchers near this arm angle, excluding self
  profile$arm_support <- NA_integer_
  fin <- is.finite(profile$arm)
  for (i in which(fin)) {
    profile$arm_support[i] <- sum(
      fin & profile$p_throws == profile$p_throws[i] &
      abs(profile$arm - profile$arm[i]) <= support_window) - 1L
  }
  profile$low_support     <- !is.na(profile$arm_support) & profile$arm_support < support_min
  profile$support_window  <- support_window
  profile$support_min     <- support_min
  profile
}


#' Same-hand pitchers releasing within `radius` SD in standardized side, height and
#' extension. Returns the count and the pool size, not a composite score.
release_rarity <- function(profile, radius = 0.75) {
  profile$rarity_n      <- NA_integer_
  profile$rarity_radius <- radius
  profile$rarity_pool   <- NA_integer_

  for (h in unique(profile$p_throws)) {
    idx <- profile$p_throws == h &
      is.finite(profile$rel_x) & is.finite(profile$rel_z) & is.finite(profile$ext)
    if (sum(idx) < 2) next
    z <- scale(cbind(profile$rel_x[idx], profile$rel_z[idx], profile$ext[idx]))
    dm <- as.matrix(stats::dist(z))
    # Exclude self
    diag(dm) <- Inf
    profile$rarity_n[idx]    <- as.integer(rowSums(dm <= radius))
    profile$rarity_pool[idx] <- as.integer(sum(idx))
  }
  profile
}

# Conventional slot names with fixed cuts
ARM_SLOT_CUTS <- c(-Inf, 0, 15, 30, 45, 60, Inf)
ARM_SLOT_LABELS <- c("Submarine", "Sidearm", "Low 3/4", "3/4", "High 3/4", "Over the top")

#' Arm angle as a slot name
arm_slot_label <- function(arm) {
  out <- as.character(cut(arm, breaks = ARM_SLOT_CUTS, labels = ARM_SLOT_LABELS,
                          right = FALSE))
  out[!is.finite(arm)] <- NA_character_
  out
}


#' Neighbor count as Rare / Uncommon / Typical
rarity_label <- function(n_neighbours) {
  ifelse(is.na(n_neighbours), NA_character_,
  ifelse(n_neighbours <= 2,  "Rare",
  ifelse(n_neighbours <= 10, "Uncommon", "Typical")))
}


# ---- League percentiles, as a sentence ---------------------------------------
# Reads the same league_ref and lg_pctile() as the Characteristics shading.

LEAGUE_PHRASE <- list(
  velo   = list(label = "Velocity",       high = "harder than",      low = "softer than",        digits = 1, unit = " mph"),
  ivb    = list(label = "Ride",           high = "more ride than",   low = "less ride than",     digits = 1, unit = " in"),
  hb     = list(label = "Run",            high = "more run than",    low = "less run than",      digits = 1, unit = " in"),
  vaa    = list(label = "Approach angle", high = "flatter than",     low = "steeper than",       digits = 2, unit = "°"),
  spin   = list(label = "Spin",           high = "more spin than",   low = "less spin than",     digits = 0, unit = " rpm"),
  ext    = list(label = "Extension",      high = "further out than", low = "shorter than",       digits = 2, unit = " ft"),
  rel_ht = list(label = "Release height", high = "higher than",      low = "lower than",         digits = 2, unit = " ft"),
  # Hand-neutral wording: positive is toward third base for both hands
  rel_side = list(label = "Release side",  high = "further toward third than", low = "further toward first than", digits = 2, unit = " ft"),
  zone_pct  = list(label = "In-zone rate", high = "in the zone more than", low = "in the zone less than", digits = 1, unit = "%"),
  whiff_pct = list(label = "Whiff rate",   high = "more whiffs than", low = "fewer whiffs than", digits = 1, unit = "%"),
  xwoba     = list(label = "xwOBA",        high = "more contact damage than", low = "less contact damage than", digits = 3, unit = "")
)


#' One pitch's traits as league percentiles and comparatives, at the same grain as
#' the Characteristics tab. `exact` is FALSE when a coarser league cut answered.
league_percentiles <- function(shape_row, ref, stand = "All",
                               count_bucket = "All Counts",
                               metrics = names(LEAGUE_PHRASE)) {
  if (is.null(shape_row) || !nrow(shape_row)) return(NULL)
  pt   <- as.character(shape_row$pitch_type[1])
  hand <- as.character(shape_row$p_throws[1])

  rows <- lapply(metrics, function(m) {
    if (!m %in% names(shape_row)) return(NULL)
    v <- shape_row[[m]][1]
    if (!is.finite(v)) return(NULL)
    # League HB is arm side positive: mirror lefties before the lookup
    lookup_v <- if (identical(m, "hb")) v * arm_side_sign(hand) else v
    hit <- lg_pctile(ref, lookup_v, m, pt, hand, stand, count_bucket)
    if (!is.finite(hit$pctile)) return(NULL)
    ph <- LEAGUE_PHRASE[[m]]
    # Phrase from the side he is on: "lower than 88%", not "higher than 12%"
    above <- hit$pctile >= 50
    list(metric = m, label = ph$label,
         value = v, digits = ph$digits, unit = ph$unit,
         pctile = hit$pctile,
         share = if (above) hit$pctile else 100 - hit$pctile,
         phrase = if (above) ph$high else ph$low,
         n_pitchers = hit$n_pitchers,
         exact = isTRUE(hit$exact), grain = hit$grain)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  do.call(rbind, lapply(rows, function(r) data.frame(r, stringsAsFactors = FALSE)))
}
