# tto.R
#
# Times through the order, for the Usage tab's toggle. Every function here is
# pure, so the rules that decide what the reader sees are testable without a
# running server. app.R only wires them together.
#
# The source is Savant's n_thruorder_pitcher, used as shipped. It counts turns
# through the LINEUP per pitcher per game, not plate appearances against a
# batter, and the difference is real: when an inning ends on the bases during a
# plate appearance, that batter leads off the next inning and Savant correctly
# keeps him on the same time through. Checked 2026-09-23 against a naive count
# of plate appearances per batter per game: 99.5% of 163,883 agree, and the 855
# that do not are that case plus plate appearances missing from the store
# because their pitches were untyped.
#
# Because it is per game and per batter, the awkward outings need no special
# case. A starter chased in the 1st has only first-time pitches that day; a
# reliever who goes bulk and sees a hitter twice has real second-time pitches.
# Nothing here asks whether a pitcher is a starter.

library(dplyr)


# "3rd+", not "3rd": a fourth time through happens (1,490 pitches in 2026, about
# 0.2%) and folding it in is what lets the three views add back up to Overall.
TTO_CHOICES <- c("Overall" = "All", "1st" = "1", "2nd" = "2", "3rd+" = "3")

TTO_TITLE <- c("1" = "1st time through", "2" = "2nd time through",
               "3" = "3rd time through or later")


#' Savant's count folded into the toggle's buckets, as character
tto_bucket <- function(n) as.character(pmin(as.integer(n), 3L))


#' The frame for one view. "All" returns it untouched
#'
#' filter() keeps factor levels, so every downstream plot and table still knows
#' the full arsenal for the window and can print a pitch he stopped throwing as
#' 0% rather than dropping its row.
filter_tto <- function(df, tto) {
  if (identical(tto, "All")) return(df)
  filter(df, tto_bucket(n_thruorder_pitcher) == tto)
}


#' Pitches per view, for the batter side on screen
#'
#' Follows the batter side because the count table does, and the count is what
#' tells the reader whether a view is worth reading: "3rd+ 14" says thin before
#' anyone looks at a percentage.
tto_counts <- function(df, hand) {
  if (hand != "All") df <- filter(df, stand == hand)
  b <- tto_bucket(df$n_thruorder_pitcher)
  c(All = nrow(df), `1` = sum(b == "1"), `2` = sum(b == "2"), `3` = sum(b == "3"))
}


#' Whether the toggle is worth drawing at all
#'
#' Only when something beyond the first time through exists. A one-inning
#' reliever, or a starter's short day, gets the tab exactly as it was, rather
#' than four buttons of which three do nothing.
tto_visible <- function(counts) (counts[["2"]] + counts[["3"]]) > 0


#' The view actually applied
#'
#' The reader's pick when it has pitches behind it, Overall otherwise. The chart
#' and the table read this, never input$tto directly: the input keeps its last
#' value after the toggle is hidden or its option disabled, and reading it raw
#' would cut a window to a time through it does not contain.
tto_effective <- function(sel, counts) {
  if (!tto_visible(counts)) return("All")
  if (length(sel) != 1 || is.na(sel) || !sel %in% names(counts)) return("All")
  if (counts[[sel]] == 0) return("All")
  sel
}


#' Title suffix for the count table, so a screenshot says which view it is
tto_table_label <- function(hand, tto) {
  if (identical(tto, "All")) return(hand_label(hand))
  paste0(hand_label(hand), ", ", TTO_TITLE[[tto]])
}


#' The toggle, or NULL when tto_visible() says no
#'
#' Hand-built radio markup rather than radioButtons(), because a view with no
#' pitches has to be DISABLED and radioButtons() cannot disable one choice.
#' The container carries shiny-input-radiogroup and the inputs share its id as
#' their name, which is the whole contract Shiny's radio binding reads, so
#' input$tto arrives exactly as it would from radioButtons().
tto_toggle_ui <- function(counts, selected = "All") {
  if (!tto_visible(counts)) return(NULL)
  pill <- function(label, value) {
    n <- counts[[value]]
    shiny::tags$label(
      class = "tto-pill",
      # Named for screen readers, which otherwise announce the bare value, "2".
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
