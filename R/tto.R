# tto.R
# Times through the order for the Usage tab, from Savant's n_thruorder_pitcher
# (turns through the lineup per game).

library(dplyr)


# 3rd+ includes the rare 4th time
TTO_CHOICES <- c("Overall" = "All", "1st" = "1", "2nd" = "2", "3rd+" = "3")

TTO_TITLE <- c("1" = "1st time through", "2" = "2nd time through",
               "3" = "3rd time through or later")


#' Savant's count folded into the toggle's buckets
tto_bucket <- function(n) as.character(pmin(as.integer(n), 3L))


#' One view's rows; "All" returns the frame untouched. Factor levels are kept.
filter_tto <- function(df, tto) {
  if (identical(tto, "All")) return(df)
  filter(df, tto_bucket(n_thruorder_pitcher) == tto)
}


#' Pitches per view for the batter side on screen
tto_counts <- function(df, hand) {
  if (hand != "All") df <- filter(df, stand == hand)
  b <- tto_bucket(df$n_thruorder_pitcher)
  c(All = nrow(df), `1` = sum(b == "1"), `2` = sum(b == "2"), `3` = sum(b == "3"))
}


#' Show the toggle only if the window has a second or later time through
tto_visible <- function(counts) (counts[["2"]] + counts[["3"]]) > 0


#' The view applied: the pick if it has pitches, else "All"
tto_effective <- function(sel, counts) {
  if (!tto_visible(counts)) return("All")
  if (length(sel) != 1 || is.na(sel) || !sel %in% names(counts)) return("All")
  if (counts[[sel]] == 0) return("All")
  sel
}


#' Count table title suffix naming the view
tto_table_label <- function(hand, tto) {
  if (identical(tto, "All")) return(hand_label(hand))
  paste0(hand_label(hand), ", ", TTO_TITLE[[tto]])
}


#' The toggle, or NULL. Hand-built radio inputs so empty views can be disabled.
tto_toggle_ui <- function(counts, selected = "All") {
  if (!tto_visible(counts)) return(NULL)
  pill <- function(label, value) {
    n <- counts[[value]]
    shiny::tags$label(
      class = "tto-pill",
      shiny::tags$input(type = "radio", name = "tto", value = value,
                 `aria-label` = sprintf("%s, %s pitches", label, format(n, big.mark = ",")),
                 checked = if (identical(value, selected)) NA,
                 disabled = if (n == 0) NA),
      shiny::tags$span(label, shiny::tags$b(class = "tto-n", format(n, big.mark = ","))))
  }
  shiny::div(class = "tto-bar",
      shiny::div(class = "tto-lab", "Times through order"),
      shiny::div(id = "tto", class = "shiny-input-radiogroup tto-pills", role = "radiogroup",
          `aria-label` = "Times through the order",
          unname(Map(pill, names(TTO_CHOICES), TTO_CHOICES))))
}
