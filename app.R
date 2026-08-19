# app.R
#
# Phase 2, the minimal app. One tab, one reactive, one output.
#
# Thin by design. Every function it calls lives in R/, so the same code path
# serves the app and the console. Logic added here rather than in R/ becomes
# untestable outside a running server.

library(shiny)
library(dplyr)
library(ggplot2)


# ---- Global scope ------------------------------------------------------------
#
# Everything here runs ONCE when the process starts, not per session. app_data
# is roughly 142 MB in memory, so loading it inside server() would mean one copy
# per connected user and the free tier ceiling would not survive two of them.
# Loaded here, every session shares the one copy.
#
# R/ is NOT sourced here. Shiny auto-sources every .R file in R/ into the app's
# own environment before this file runs. Calling source() explicitly would put
# those functions in the global environment instead, since source() defaults to
# local = FALSE, which leaves the app reading from one environment and its
# helpers living in another. From the console, source them yourself as usual.

app_data     <- load_app_data()
player_index <- build_player_index(app_data)

# game_date is stored as character. Keep the bounds as Date for the input widget
# and convert back at comparison time, see pitcher_data() below.
DATE_MIN <- as.Date(min(app_data$game_date))
DATE_MAX <- as.Date(max(app_data$game_date))

DEFAULT_PITCHER <- 702070   # Cameron, so the page renders something on load


ui <- fluidPage(
  titlePanel("Pitcher Arsenal"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # choices = NULL because the list is filled server-side below. Passing 800
      # names here would ship them all to the browser on every page load.
      selectizeInput("pitcher", "Pitcher", choices = NULL),
      dateRangeInput("dates", "Date range",
                     start = DATE_MIN, end = DATE_MAX,
                     min   = DATE_MIN, max = DATE_MAX),
      helpText("Pitch types under ", MIN_PITCH_COUNT,
               " pitches in the selected window are dropped.")
    ),
    mainPanel(
      width = 9,
      plotOutput("movement", height = "620px")
    )
  )
)


server <- function(input, output, session) {

  # Server-side filtering. The browser holds only what it is showing and asks
  # for matches as the user types, so the 800-name index never crosses the wire
  # in full and never touches the pitch store.
  updateSelectizeInput(session, "pitcher",
                       choices  = player_choices(player_index),
                       selected = DEFAULT_PITCHER,
                       server   = TRUE)

  # The one reactive. Reading input$pitcher and input$dates here is what
  # subscribes to them: Shiny records the reads during evaluation, it does not
  # take a declared list. Lazy and cached, so it recomputes once per
  # invalidation no matter how many outputs ask for it. That is the whole reason
  # to have it as a reactive rather than filtering inside renderPlot, and it is
  # what keeps Phase 4's six outputs from running six identical filters over
  # 548k rows on every input change.
  pitcher_data <- reactive({
    req(input$pitcher, input$dates)

    # selectize returns a character string, and the pitcher column is integer.
    id <- as.integer(input$pitcher)

    # Compare character to character. Mixed character/Date comparison happens to
    # be correct for zero-padded ISO dates, but relying on that is a trap the
    # moment a date arrives in another format.
    from <- as.character(input$dates[1])
    to   <- as.character(input$dates[2])

    d <- app_data |>
      filter(pitcher == id, game_date >= from, game_date <= to) |>
      shape_arsenal()

    # An empty window is a normal thing for a user to select, not an error.
    # Without this the plot errors on an empty factor and the page shows a stack
    # trace instead of a sentence.
    validate(need(nrow(d) > 0,
                  "No pitches for this pitcher in the selected window."))
    d
  })

  output$movement <- renderPlot({
    plot_movement(pitcher_data())
  })
}


shinyApp(ui, server)
