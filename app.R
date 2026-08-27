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
# is 103 MB in memory, measured 2026-08-21 after the column trim, and the three
# startup objects put 191 MB on the R heap between them. Loading them inside
# server() would mean one copy per connected user, which the free tier ceiling
# would not survive twice. Loaded here, every session shares the one copy.
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
# Read off the data, not hardcoded, for the same reason player_index is: a list
# of 30 written out here would go stale on a relocation and would silently drop
# a team the store does have. Sorted so the dropdown is alphabetical.
TEAM_CODES   <- sort(unique(app_data$pitch_team))
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
  # The deploy bundles the rds files rather than fetching them at startup, so
  # the page has to say how current they are. Read off app_data itself and not
  # a build-time constant: a redeploy that ships stale data then shows the
  # stale date rather than the date of the deploy.
  titlePanel(div("Pitcher Arsenal",
                 span(paste("Data through", DATE_MAX),
                      style = "font-size:14px; color:#666; margin-left:12px;")),
             # Explicit, because titlePanel defaults windowTitle to the title
             # itself, and with a tag there that puts raw markup in the browser
             # tab. Verified on the rendered page, not assumed.
             windowTitle = "Pitcher Arsenal"),
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
      # Was "Pitch types under 5 pitches in the selected window are dropped",
      # which described behaviour that is gone. Every pitch type is charted now;
      # what varies is whether a RATE off it is trustworthy, which the table
      # already says per cell in grey with its denominator.
      helpText("Every pitch type is shown. Rates from small samples are greyed",
               " and carry their own denominator."),
      # Under the controls rather than over the tabs. The panel describes the
      # whole selection, which is what this column already is, and moving it
      # here fills the dead space below the inputs and lets the tabs start at
      # the top of the main area.
      shiny::tags$head(shiny::tags$style(shiny::HTML(RESULTS_PANEL_CSS))),
      shiny::tags$hr(style = "margin:14px 0 12px 0; border-color:#e0e0e0;"),
      uiOutput("results_panel")
    ),
    mainPanel(
      width = 9,
      # Sits above the tabs so a remap or drop is visible on whichever tab is
      # open, rather than only on the one that would have crashed. It stays here
      # and does not follow the results panel into the sidebar: it describes
      # what the CHARTS dropped, not how the pitcher performed.
      uiOutput("pitch_code_note"),
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
                          "as white dots instead of a smoothed surface.")),
        # Last, deliberately. The first four tabs are all views of the one
        # pitcher in the selector and read left to right as a report; this one
        # searches the league and writes BACK to that selector, so it sits after
        # them rather than interrupting them.
        tabPanel("Search",
                 # The one tab that owns its own inputs. The pitcher selector,
                 # the dates and the batter side stay global and still apply:
                 # the dates and the side narrow the population searched, and
                 # the pitcher selector is where a result LANDS.
                 div(style = "margin-top:10px;",
                   fluidRow(
                     column(2, selectInput("s_throws", "Pitcher hand",
                                           choices = c("RHP" = "R", "LHP" = "L"))),
                     column(2, selectInput("s_pitch", "Pitch type",
                                           choices = names(pitch_colors), selected = "FF")),
                     # Teams come from the data rather than a hardcoded list of
                     # 30, so a relocation or an expansion club needs no edit
                     # here and cannot silently go missing from the dropdown.
                     column(2, selectInput("s_team", "Team",
                                           choices = c("All teams" = "All", TEAM_CODES))),
                     column(3, numericInput("s_min", "Min pitches", value = 25,
                                            min = 1, step = 5)),
                     column(3, div(style = "margin-top:25px;",
                                   actionButton("s_reset", "Reset sliders", class = "btn-sm")))
                   ),
                   # Seeded from the data on every change of hand, pitch type,
                   # window or batter side, so each slider ends where that group
                   # actually ends. A typed threshold is how you ask for a shape
                   # nobody has: 95 mph with 18 IVB and 12 HB is zero righties.
                   uiOutput("search_sliders"),
                   uiOutput("search_count"),
                   gt::gt_output("search_table")
                 ))
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
             if (!is.null(note)) paste0(" ", note, ".")
             else " No pitch carried a chartable type code.")
    }))
    d
  })

  # The window before any pitch-type reconciliation or charting decisions.
  #
  # A plain reactive that recomputes the same filter, rather than a reactiveVal
  # written from inside pitcher_data(). Setting a reactiveVal from within a
  # reactive() is an anti-pattern and it failed silently here: the deployed app
  # kept reporting 0 batters faced for Fuentes on 2026-08-05 while the same call
  # returned 4 locally, because the value was never written and the panel was
  # reading NULL. Recomputing a filter over one pitcher is cheap; being wrong is
  # not.
  #
  # Deliberately does NOT depend on pitcher_data(). Batters faced must survive
  # even when nothing in the window is chartable and pitcher_data() validates
  # its way out.
  pitcher_window <- reactive({
    req(input$pitcher, input$dates)
    app_data |> filter(pitcher == as.integer(input$pitcher),
                       game_date >= as.character(input$dates[1]),
                       game_date <= as.character(input$dates[2]))
  })

  output$pitch_code_note <- renderUI({
    note <- pitch_code_note(pitcher_data())
    if (is.null(note)) return(NULL)
    # paste0 rather than passing three arguments to div(), which inserts
    # whitespace between them and left a space before the full stop.
    div(style = "color:#666; font-size:12px; margin-bottom:6px;",
        paste0("Pitch codes: ", note, "."))
  })

  # ---- Pitch trait search ----------------------------------------------------
  #
  # Three stages, cheapest last. search_pool() reads input$dates and input$hand
  # and nothing else, so dragging a slider never re-runs the 0.78 s aggregate
  # over all 818 pitchers. The sliders are seeded off that pool. The filter runs
  # on the 3,665-row result in 6 ms, which is what makes dragging feel live.
  #
  # Deliberately NOT built on pitcher_data(), which is one pitcher. Reusing it
  # would mean running it once per pitcher in the league.
  search_pool <- reactive({
    req(input$dates, input$s_team)
    from <- as.character(input$dates[1])
    to   <- as.character(input$dates[2])
    search_aggregate(filter(app_data, game_date >= from, game_date <= to),
                     input$hand, input$s_team)
  })

  search_bounds <- reactive({
    req(input$s_throws, input$s_pitch, input$s_min)
    search_ranges(search_pool(), input$s_throws, input$s_pitch, input$s_min)
  })

  # Rebuilt rather than updated, so switching pitch type resets the sliders onto
  # the new population instead of leaving a righty four-seam's range in place
  # while a lefty curveball is selected. The reset button re-triggers it.
  output$search_sliders <- renderUI({
    rg <- search_bounds()
    input$s_reset
    cols <- lapply(seq_len(nrow(rg)), function(i) {
      spec <- SEARCH_TRAITS[SEARCH_TRAITS$trait == rg$trait[i], ]
      # ticks = FALSE, deliberately. ionRangeSlider draws its grid at a
      # prettified interval and then ALWAYS labels the max, so whenever the
      # span is not a multiple of that interval the last tick and the max
      # overlap: RHP four-seams span 13.6 mph with ticks every 1.4, which
      # printed 100.6 and 101.6 on top of each other. Shiny's sliderInput
      # exposes no control over the grid count, so the grid goes.
      #
      # The range the grid was carrying moves into the label, where it reads
      # better anyway: it names the population rather than the axis.
      column(4, sliderInput(paste0("s_", rg$trait[i]),
                            paste0(spec$label, ": ", fmt_bound(rg$lo[i], spec$digits),
                                   " to ", fmt_bound(rg$hi[i], spec$digits)),
                            min = rg$lo[i], max = rg$hi[i], ticks = FALSE,
                            value = c(rg$lo[i], rg$hi[i]), step = spec$step))
    })
    tagList(fluidRow(cols[1:3]), fluidRow(cols[4:5]))
  })

  # Sort state, owned here rather than in the table, because a click has to know
  # what the last click did. Same column flips the direction, a new column starts
  # at descending: for every column in this table the interesting end is the top,
  # whether that is the hardest thrower or the most pitches.
  sort_state <- reactiveValues(col = "pitches", desc = TRUE)
  observeEvent(input$search_sort, {
    cl <- input$search_sort
    req(cl %in% c("player_name", "team", "pitches", SEARCH_TRAITS$trait,
                  "whiff_pct", "chase_pct", "xwoba"))
    if (identical(cl, sort_state$col)) {
      sort_state$desc <- !sort_state$desc
    } else {
      sort_state$col  <- cl
      sort_state$desc <- TRUE
    }
  })

  search_results <- reactive({
    rg <- search_bounds()
    bounds <- lapply(rg$trait, function(tr) input[[paste0("s_", tr)]])
    names(bounds) <- rg$trait
    # renderUI builds the sliders, so on the first pass they do not exist yet and
    # every bound is NULL. search_filter() reads a missing bound as no filter,
    # which is the right reading of a partial query in any case.
    search_filter(search_pool(), input$s_throws, input$s_pitch, bounds, input$s_min,
                  sort_by = sort_state$col, desc = sort_state$desc)
  })

  output$search_count <- renderUI({
    res <- search_results()
    # paste0 rather than separate arguments to div(), which inserts whitespace
    # between them. The same slip put a space before a full stop in the
    # pitch-code note once already.
    # A trait with no reading cannot satisfy a range, so those pitchers are
    # dropped even with every slider at full width. Named rather than swallowed,
    # since otherwise the count silently disagrees with the population.
    miss <- search_missing(search_pool(), input$s_throws, input$s_pitch, input$s_min)
    note <- if (nrow(miss)) paste0(
      " ", sum(miss$n), " excluded for no ",
      paste(SEARCH_TRAITS$label[match(miss$trait, SEARCH_TRAITS$trait)], collapse = " or "),
      " reading.") else ""

    div(style = "margin:6px 0 10px 0; color:#444;",
        strong(nrow(res)),
        paste0(" of ", search_bounds()$n[1], " ",
               if (input$s_throws == "R") "RHP" else "LHP", " ", input$s_pitch,
               " match, among those with ", input$s_min, "+ pitches in this window."),
        if (nzchar(note)) span(style = "color:#767676;", note))
  })

  output$search_table <- gt::render_gt({
    res <- search_results()
    # Two different empty results, and telling the reader to widen a slider when
    # nobody throws the pitch at all is the wrong advice. No LHP throws a KN with
    # 25+ pitches in a season, and the sliders are a 0-to-step stub in that case,
    # so widening them would achieve nothing.
    validate(need(search_bounds()$n[1] > 0, paste0(
      "No ", if (input$s_throws == "R") "RHP" else "LHP", " throws a ",
      input$s_pitch, " with ", input$s_min, "+ pitches in this window.")))
    validate(need(nrow(res) > 0,
                  "No pitcher matches. Widen a slider, or lower the minimum pitch count."))
    # ref = NULL: no percentile fill here. This table is already 50 rows of one
    # pitch type, so a fill on every cell reads as a wall rather than as
    # context, and the column you sorted on is the comparison you actually
    # asked for. It also drops the resolve pass, which was the expensive half.
    shown <- head(res, SEARCH_MAX_ROWS)
    search_gt(shown, input$s_pitch, input$s_throws, input$hand, ref = NULL,
              n_total = nrow(res), sort_by = sort_state$col, desc = sort_state$desc)
  })

  # The click. The payload is an id and the name is looked up here rather than
  # trusted from the page. Moving to Movement is deliberate: without it the click
  # updates a selector the reader cannot see, and the page looks broken.
  observeEvent(input$search_pick, {
    id <- suppressWarnings(as.integer(input$search_pick))
    req(!is.na(id), id %in% player_index$pitcher)
    updateSelectizeInput(session, "pitcher", choices = player_choices(player_index),
                         selected = id, server = TRUE)
    updateTabsetPanel(session, "tabs", selected = "Movement")
  })

  # ---- Plot sizing guard -----------------------------------------------------
  #
  # Shiny draws a plot as soon as the client reports that output's size, and on
  # a cold connection the first report can carry a zero width: the element is in
  # the DOM but the browser has not laid it out yet. The graphics device refuses
  # the size, and Shiny paints a red error box. On a free shinyapps instance that
  # box is what a first-time visitor looks at for the ten or so seconds the first
  # plot takes. The message differs by platform, "invalid 'width' argument" from
  # the PNG device on the server and "invalid quartz() device size" locally,
  # which is the same fault.
  #
  # This has to be a width function and not a req() at the top of the render
  # expression. renderPlot() OPENS THE DEVICE BEFORE it evaluates the expression,
  # so a guard inside the expression runs after the failure it is trying to
  # prevent. Tried that first; the error was unchanged, which is also why the
  # original traceback goes straight from output$movement to startPNG with no
  # app code in between.
  #
  # req() here suspends the output instead, and the suspension is temporary: the
  # client re-reports the size once layout settles, this function is reactive on
  # clientData, and the plot draws on the corrected width. That second report is
  # known to arrive rather than hoped for, observed on the live app before any
  # guard existed, where a failed render was followed by a successful one with no
  # user action.
  sized_width <- function(id) function() {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    req(!is.null(w), is.finite(w), w > 0)
    w
  }

  output$movement <- renderPlot({
    # league_ref was wired into the characteristics table in Phase 5 but not
    # here, so the reference marks existed and never reached the page.
    plot_movement(pitcher_data(), ref = league_ref)
  }, width = sized_width("movement"))

  output$usage <- renderPlot({
    plot_usage(pitcher_data())
  }, width = sized_width("usage"))

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
  }, width = sized_width("heatmap"))

  # Two sources, two rows, deliberately not merged. The Statcast half reads
  # input$hand and narrows with it; the game-log half does not read it at all,
  # so it cannot narrow, and its header says so. A game log has no platoon
  # split, so a vs-RHH ERA does not exist rather than being merely unavailable.
  # The league over the SAME window and batter side the panel is showing, which
  # is what makes IP gradeable at all: it is a counting stat, and the league
  # median IP over two weeks is 5.7 against 29.7 across the season. Keyed on
  # dates and side only, so it survives a change of pitcher and a tab switch and
  # costs 0.40 s when the window actually moves.
  results_ref <- reactive({
    req(input$dates)
    results_league(app_data, game_logs, input$dates, input$hand, FIP_CONST)
  })

  output$results_panel <- renderUI({
    # The RAW window, not pitcher_data(). Batters faced is a property of the
    # outing, not of which pitch types survived charting. Fed the charted frame,
    # this read 0 TBF for a reliever whose every PA-ending pitch was a type the
    # display had dropped, and blanked every rate computed over it. Even with
    # that floor gone, reconcile_pitch_codes() still drops genuinely unchartable
    # codes, and a plate appearance must not disappear because it ended on one.
    d  <- pitcher_data()
    sc <- results_statcast(pitcher_window(), input$hand)
    gl <- if (is.null(game_logs)) {
      list(have = FALSE, games = 0L, through = NA_character_)
    } else {
      results_gamelog(game_logs, input$pitcher, input$dates, FIP_CONST)
    }
    results_panel(sc, gl, input$dates, input$hand, LOG_THROUGH,
                  ctx = results_context(sc, gl, results_ref()))
  })

  # League context, on white rows. What made this table a patchwork was carrying
  # two colour systems at once: a pitch-colour wash saying WHICH pitch and a
  # percentile fill saying HOW GOOD. The wash is gone, the pitch code keeps the
  # colour, and the fill is the only thing that varies across the row.
  #
  # The reference is `league_ref`, which is season-wide and precomputed, rather
  # than the window-matched reference the results panel builds. Deliberate: the
  # panel had to match the window because IP is a counting stat, while every
  # cell here is a rate or a shape whose league value barely moves between a
  # two-week and a season measurement. league_ref also costs nothing at runtime
  # and carries the fallback ladder, which a per-window rebuild would lose.
  #
  # resolve_table() reads league_ref and the table, never app_data, so this
  # stays cheap enough to run on every input change.
  output$chars_table <- gt::render_gt({
    d   <- pitcher_data()
    tbl <- arsenal_table(d, input$hand, stuff_all())
    ctx <- resolve_table(tbl, arsenal_denoms(d, input$hand), league_ref,
                         d$p_throws[1], input$hand)
    arsenal_gt(tbl, input$hand,
               fg_window = if (is.null(FG_EXPORT)) NULL else FG_EXPORT$label,
               ref = ctx)
  })
}


shinyApp(ui, server)
