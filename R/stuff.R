# stuff.R
#
# Stuff+ grades from Jonathan's own model (stuff-plus repo, v2): how often a
# pitch's shape misses bats when swung at. 100 is the average pitch within its
# group (fastball, offspeed, hard breaker, soft breaker), 10 points one SD
# within the group. Validated on the held-out 2026 season before it was wired in.
#
# This file owns where a grade comes from. arsenal_table() takes the result as
# its stuff_all argument and knows nothing about the source. Keep it that way:
# no Stuff+ read inside arsenal_table(), no `source` argument on it.
#
# The stuff_all contract is two columns: pitch_type, stuff_plus.
#
# The model never runs in the app. Every 2026 pitch was scored once in the
# stuff-plus repo against a same-side and an opposite-side hitter, and the
# results ship as daily sums by pitcher, date, pitch type and batter side
# (lookups/stuff_v2_2026.csv). A grade for any window and side is a sum over
# rows. Live scoring belongs in the data build before the 2027 season.

library(readr)
library(dplyr)
library(tibble)


#' The scale group for a pitch type, as the app labels pitches (KC and CS are
#' already folded into CU, FO into FS)
STUFF_GROUP <- function(pitch_type) dplyr::case_when(
  pitch_type %in% c("FF", "SI", "FC")       ~ "fastball",
  pitch_type %in% c("CH", "FS")             ~ "offspeed",
  pitch_type == "SL"                        ~ "hard breaker",
  pitch_type %in% c("ST", "SV", "CU")       ~ "soft breaker")


#' Load the daily sums and the scale constants, once at startup
#'
#' NULL on any failure, so a missing file blanks the Stuff+ column rather than
#' taking the app down; every other column is still worth reading.
load_stuff_v2 <- function(daily = "lookups/stuff_v2_2026.csv",
                          scale = "lookups/stuff_v2_scale.csv") {
  tryCatch(list(
    daily = read_csv(daily, show_col_types = FALSE,
                     col_types = cols(pitcher = "i", game_date = "c", pitch_type = "c",
                                      stand = "c", n = "i", s_both = "d", s_side = "d")),
    scale = read_csv(scale, show_col_types = FALSE)
  ), error = function(e) {
    warning("Stuff+ grades not loaded: ", conditionMessage(e), call. = FALSE)
    NULL
  })
}


#' Stuff+ for one pitcher over a date window and batter side
#'
#' "All" scores every throw against both sides 50/50, so the grade does not
#' reward the hitters he happened to face. "R" or "L" uses the throws to that
#' side, scored as that side, so the grade matches the pitches the rest of the
#' table counts.
#'
#' Returns the two-column stuff_all contract, zero rows if the pitcher has none,
#' so arsenal_table() can left_join it either way.
stuff_for <- function(sv, mlb_id, dates, hand) {
  empty <- tibble(pitch_type = character(), stuff_plus = numeric())
  if (is.null(sv)) return(empty)
  d <- sv$daily |>
    filter(pitcher == mlb_id, game_date >= as.character(dates[1]),
           game_date <= as.character(dates[2]))
  if (hand != "All") d <- d |> filter(stand == hand)
  if (!nrow(d)) return(empty)
  col <- if (hand == "All") "s_both" else "s_side"
  d |>
    summarise(g = sum(.data[[col]]) / sum(n), .by = pitch_type) |>
    mutate(group = STUFF_GROUP(pitch_type)) |>
    inner_join(sv$scale, by = "group") |>
    transmute(pitch_type, stuff_plus = round(100 + 10 * (g - mu) / sigma))
}
