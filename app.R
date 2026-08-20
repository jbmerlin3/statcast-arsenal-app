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
# Same reasoning as app_data: loaded once, shared by every session. It is small
# next to app_data, but the app never rebuilds it either. scripts/update_data.R
# step 2 owns that, per the daily chain in CLAUDE.md.
league_ref   <- load_league_ref()
# Step 4 of the chain, and the only input the app tolerates missing: it depends
# on somebody else's uptime. NULL degrades the results panel's game-log row to
# an absence message rather than taking the app down.
game_logs    <- load_game_logs()
# One constant for the whole file, so every FIP on the page is on the same
# scale. Computed here rather than per render: it is a property of the log file.
FIP_CONST    <- if (is.null(game_logs)) NA_real_ else fip_constant(game_logs)
LOG_THROUGH  <- if (is.null(game_logs)) NULL else max(game_logs$game_date)
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
      # Sits above the tabs so a remap or drop is visible on whichever tab is
      # open, rather than only on the one that would have crashed.
      uiOutput("pitch_code_note"),
      # Above the tabs, like the pitch-code note: the results line describes the
      # whole selection, not one tab's view of it.
      shiny::tags$head(shiny::tags$style(shiny::HTML(RESULTS_PANEL_CSS))),
      uiOutput("results_panel"),
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
        tabPanel("Characteristics", gt::gt_output("chars_table")),
        tabPanel("Heat Maps",
                 # Taller than the other outputs: facet_grid lays out three
                 # situations by however many pitch types the window holds, and
                 # coord_fixed keeps each panel square.
                 plotOutput("heatmap", height = "700px"),
                 helpText("Three coarse count buckets, not the six in the usage ",
                          "table. A density estimate needs a larger per-panel ",
                          "sample than a usage percentage does. Panels under ",
                          KDE_MIN_N, " pitches show the raw locations ",
                          "as white dots instead of a smoothed surface."))
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

    raw <- app_data |> filter(pitcher == id, game_date >= from, game_date <= to)

    # An empty window is a normal thing for a user to select, not an error.
    validate(need(nrow(raw) > 0,
                  "No pitches for this pitcher in the selected window."))

    d <- shape_arsenal(raw)

    # Three different reasons the frame can come back empty, and saying the
    # wrong one is worse than saying nothing. A position player who threw only
    # eephuses has pitches; none of them are a type this app charts.
    validate(need(nrow(d) > 0, {
      note <- pitch_code_note(d)
      paste0(nrow(raw), " pitches in this window, but none chartable.",
             if (!is.null(note)) paste0(" ", note, ".") else "",
             if (is.null(note)) paste0(" No pitch type reached ", MIN_PITCH_COUNT,
                                       " pitches.") else "")
    }))
    d
  })

  output$pitch_code_note <- renderUI({
    note <- pitch_code_note(pitcher_data())
    if (is.null(note)) return(NULL)
    # paste0 rather than passing three arguments to div(), which inserts
    # whitespace between them and left a space before the full stop.
    div(style = "color:#666; font-size:12px; margin-bottom:6px;",
        paste0("Pitch codes: ", note, "."))
  })

  output$movement <- renderPlot({
    # league_ref was wired into the characteristics table in Phase 5 but not
    # here, so the reference marks existed and never reached the page.
    plot_movement(pitcher_data(), ref = league_ref)
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

  output$heatmap <- renderPlot({
    plot_heatmap(pitcher_data(), input$hand)
  })

  # Two sources, two rows, deliberately not merged. The Statcast half reads
  # input$hand and narrows with it; the game-log half does not read it at all,
  # so it cannot narrow, and its header says so. A game log has no platoon
  # split, so a vs-RHH ERA does not exist rather than being merely unavailable.
  output$results_panel <- renderUI({
    d  <- pitcher_data()
    sc <- results_statcast(d, input$hand)
    gl <- if (is.null(game_logs)) {
      list(have = FALSE, games = 0L, through = NA_character_)
    } else {
      results_gamelog(game_logs, input$pitcher, input$dates, FIP_CONST)
    }
    results_panel(sc, gl, input$dates, input$hand, LOG_THROUGH)
  })

  output$chars_table <- gt::render_gt({
    d   <- pitcher_data()
    tbl <- arsenal_table(d, input$hand, stuff_all())
    # resolve_table() reads league_ref and the table, never app_data, so this
    # stays cheap enough to run on every input change.
    ctx <- resolve_table(tbl, arsenal_denoms(d, input$hand), league_ref,
                         d$p_throws[1], input$hand)
    arsenal_gt(tbl, input$hand,
               fg_window = if (is.null(FG_EXPORT)) NULL else FG_EXPORT$label,
               ref = ctx)
  })
}


shinyApp(ui, server)
