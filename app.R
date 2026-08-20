# app.R
#
# Thin by design. Every function it calls lives in R/, so the same code path
# serves the app and the console. Logic added here rather than in R/ becomes
# untestable outside a running server.
#
# Tabs are inline rather than R/mod_*.R modules. At this size a module would be
# a function call with NS() ceremony around it: no tab owns an input of its own,
# and none is instantiated twice. See CLAUDE.md for the trigger to revisit.

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
HALVES       <- season_halves(app_data)

# game_date is stored as character. Keep the bounds as Date for the input widget
# and convert back at comparison time, see pitcher_data() below.
DATE_MIN <- HALVES$full[1]
DATE_MAX <- HALVES$full[2]

DEFAULT_PITCHER <- 702070   # Cameron, so the page renders something on load

# Which FanGraphs export backs the Stuff+ column. Resolved once, by the date
# window in the filename rather than by file mtime: the newest file by mtime in
# a working fg_stuff/ is typically a half-season export, and pairing half-season
# Stuff+ against a full-season table is wrong in a way the page would not show.
#
# A missing or unreadable directory degrades to a blank Stuff+ column rather
# than taking the app down, since every other column is still worth reading.
FG_EXPORT <- tryCatch(resolve_fg_export("fg_stuff"), error = function(e) {
  warning("No FanGraphs export resolved: ", conditionMessage(e), call. = FALSE)
  NULL
})

# The stuff_all contract, three columns, as load_fg_stuff() returns on no match.
# arsenal_table() takes this as an argument and never learns where it came from,
# which is the seam that lets the v4 model replace FanGraphs later without
# touching tables.R. See CLAUDE.md.
EMPTY_STUFF <- tibble::tibble(pitch_type = character(), stuff_plus = numeric(),
                              fg_exact = logical())

# 1H and 2H are omitted entirely when no break has happened yet, rather than
# rendering buttons that would set an invented boundary.
preset_buttons <- if (is.null(HALVES$first)) {
  actionButton("preset_all", "All", class = "btn-sm")
} else {
  tagList(actionButton("preset_all", "All", class = "btn-sm"),
          actionButton("preset_1h",  "1H",  class = "btn-sm"),
          actionButton("preset_2h",  "2H",  class = "btn-sm"))
}


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
      div(style = "margin-bottom:15px;", preset_buttons),
      radioButtons("hand", "Batter side",
                   choices  = c("All" = "All", "vs LHH" = "L", "vs RHH" = "R"),
                   selected = "All", inline = TRUE),
      helpText("Pitch types under ", MIN_PITCH_COUNT,
               " pitches in the selected window are dropped.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("Movement", plotOutput("movement", height = "620px")),
        tabPanel("Usage",
                 plotOutput("usage", height = "420px"),
                 # Said out loud because the batter side control is visible and
                 # this chart deliberately ignores it. Without the note the
                 # toggle looks broken on this tab.
                 helpText("The usage chart always shows both batter sides. ",
                          "The table below follows the Batter side selector."),
                 gt::gt_output("usage_table")),
        tabPanel("Characteristics", gt::gt_output("chars_table"))
      )
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

  # Presets write to the date input rather than acting as a second filter, so
  # the input always shows the window actually applied and the two can never
  # disagree.
  set_range <- function(r) updateDateRangeInput(session, "dates", start = r[1], end = r[2])
  observeEvent(input$preset_all, set_range(HALVES$full))
  if (!is.null(HALVES$first)) {
    observeEvent(input$preset_1h, set_range(HALVES$first))
    observeEvent(input$preset_2h, set_range(HALVES$second))
  }

  # The one data reactive. It is deliberately NOT filtered by batter side.
  # arsenal_table(), count_usage_tbl() and plot_heatmap() filter `stand`
  # internally and take hand as an argument, while plot_usage() reads `stand`
  # itself to draw both sides at once. Filtering here would double-filter the
  # first three and silently halve the usage chart.
  #
  # It also means outputs that never read input$hand do not re-render when the
  # toggle changes, because Shiny discovers dependencies by watching which
  # inputs are read. That falls out for free rather than needing any condition.
  pitcher_data <- reactive({
    req(input$pitcher, input$dates)
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
    validate(need(nrow(d) > 0,
                  "No pitches for this pitcher in the selected window."))
    d
  })

  output$movement <- renderPlot({
    plot_movement(pitcher_data())
  })

  output$usage <- renderPlot({
    plot_usage(pitcher_data())
  })

  output$usage_table <- gt::render_gt({
    count_usage_gt(count_usage_tbl(pitcher_data(), input$hand), input$hand)
  })

  # Stuff+ for the selected pitcher. The read is about 11 ms, so it runs per
  # invalidation rather than being cached; caching would add a staleness bug
  # for no perceptible gain.
  #
  # suppressMessages because load_fg_stuff() announces each match, which is
  # useful once in the console and console spam on every filter change. Its
  # warning on a missing pitcher is deliberately left audible.
  stuff_all <- reactive({
    req(input$pitcher)
    if (is.null(FG_EXPORT)) return(EMPTY_STUFF)
    suppressMessages(load_fg_stuff(as.integer(input$pitcher), FG_EXPORT$path))
  })

  output$chars_table <- gt::render_gt({
    arsenal_gt(arsenal_table(pitcher_data(), input$hand, stuff_all()),
               input$hand,
               fg_window = if (is.null(FG_EXPORT)) NULL else FG_EXPORT$label)
  })
}


shinyApp(ui, server)
