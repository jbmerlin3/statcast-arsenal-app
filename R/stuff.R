# stuff.R
# Stuff+ grades from a separate model (v2), shipped as daily sums per pitcher, date,
# pitch type and batter side. 100 = average within the pitch's group, 10 = one SD.
# arsenal_table() takes the grades as an argument; don't read them inside it.

library(readr)
library(dplyr)
library(tibble)


#' Scale group for a pitch type (after KC/CS -> CU and FO -> FS); NA for KN, which has no grade
stuff_group <- function(pitch_type) dplyr::case_when(
  pitch_type %in% c("FF", "SI", "FC")       ~ "fastball",
  pitch_type %in% c("CH", "FS")             ~ "offspeed",
  pitch_type == "SL"                        ~ "hard breaker",
  pitch_type %in% c("ST", "SV", "CU")       ~ "soft breaker")


#' Load the daily sums and scale constants, or NULL
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


#' Stuff+ for one pitcher, window and batter side. "All" weights both sides 50/50;
#' "R" or "L" uses throws to that side. Zero rows if none.
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
    mutate(group = stuff_group(pitch_type)) |>
    inner_join(sv$scale, by = "group") |>
    transmute(pitch_type, stuff_plus = round(100 + 10 * (g - mu) / sigma))
}
