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

# Listed height per MLBAM id, for the Context tab's release model. A tracked
# CSV rather than chain-built data, for the reason in
# scripts/build_pitcher_heights.R. NULL degrades the Context tab's residual
# columns to blank rather than taking the app down; every other tab is
# unaffected, and the cohort engine still works on release height and extension.
PITCHER_HEIGHTS <- load_pitcher_heights()

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
  # ---- One visual language for every tab ----------------------------------
  #
  # Normalised 2026-09-21. Each tab had grown its own spacing, its own grey,
  # and its own way of writing a note under a chart; Context had been restyled
  # and the rest had not, so moving between tabs felt like moving between apps.
  # Every tab now opens with the same section header and writes notes the same
  # way, and those rules live HERE, once, rather than in each tab.
  #
  # Page-level only, deliberately. The gt tables keep their own internal
  # styling, because tests/step3_null_identical.R pins their rendered HTML byte
  # for byte and a stylesheet that reached inside them would move every
  # baseline for a cosmetic reason.
  tags$head(tags$style(HTML(paste0(
    "body{color:#1a1a1a;}",
    ".nav-tabs{margin-bottom:4px;}",
    ".nav-tabs>li>a{font-size:14px;padding:9px 16px;}",
    ".nav-tabs>li.active>a{font-weight:700;}",
    ".sec,.ctx-sec{margin-top:26px;}",
    ".sec-h,.ctx-h{font-size:18px;font-weight:800;color:#111;margin:0 0 12px 0;",
    "padding-left:10px;border-left:4px solid #1f3a5f;line-height:1.1;}",
    ".sec-sub{font-size:11px;color:#888;text-transform:uppercase;letter-spacing:.5px;",
    "margin:-6px 0 10px 14px;}",
    ".sec-note,.ctx-note{font-size:11.5px;color:#888;margin:8px 0 0 0;max-width:900px;",
    "line-height:1.45;}",
    ".app-codes{font-size:11.5px;color:#999;margin:2px 0 6px 0;}"
  )))),
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
      div(class = "app-codes", uiOutput("pitch_code_note")),
      tabsetPanel(
        id = "tabs",
        # Every tab opens with the same section header (.sec-h, defined once at
        # the top of the page) and writes notes the same way (.sec-note).
        tabPanel("Movement",
                 div(class = "sec",
                     div(class = "sec-h", "Movement profile"),
                     div(class = "sec-sub", "Induced vertical vs horizontal break, catcher's view"),
                     plotOutput("movement", height = "620px"))),
        tabPanel("Usage",
                 div(class = "sec",
                     div(class = "sec-h", "Pitch usage"),
                     plotOutput("usage", height = "420px"),
                     # Said out loud because the batter side control is visible
                     # and this chart deliberately ignores it. Without the note
                     # the toggle looks broken on this tab.
                     div(class = "sec-note",
                         "The chart always shows both batter sides. The table below follows the Batter side selector.")),
                 div(class = "sec",
                     div(class = "sec-h", "Usage by count"),
                     gt::gt_output("usage_table"))),
        # Two tables, traits above results, from one arsenal_table() pass. The
        # order is deliberate: what the pitch IS reads before what it DID, so a
        # reader who stops after the first table has still learned the arsenal.
        tabPanel("Characteristics",
                 div(class = "sec",
                     div(class = "sec-h", "Pitch traits"),
                     gt::gt_output("traits_table")),
                 div(class = "sec",
                     div(class = "sec-h", "Pitch results"),
                     gt::gt_output("results_table"))),
        tabPanel("Heat Maps",
                 div(class = "sec",
                     div(class = "sec-h", "Location by count"),
                     # Taller than the other outputs: facet_grid lays out three
                     # situations by however many pitch types the window holds,
                     # and coord_fixed keeps each panel square.
                     plotOutput("heatmap", height = "700px"),
                     div(class = "sec-note", paste0(
                       "Three coarse count buckets, not the six in the usage table. A density ",
                       "estimate needs a larger per-panel sample than a usage percentage does. ",
                       "Panels under ", KDE_MIN_N, " pitches show the raw locations as white ",
                       "dots instead of a smoothed surface.")))),
        # ---- Context: one pitcher, read the way a coach reads him ----------
        #
        # Laid out 2026-09-21 around two questions: is he funky (the percentile
        # chart, left, and the release profile, right) and what do his pitches
        # do (full width below). An attack plan sat on the right until later
        # that day and was cut by request: it was too simple, and how to attack
        # a pitcher depends on the hitter, which this page does not know. A location strip was tried and removed the same
        # day: the Heat Maps tab already draws every pitch's location, and this
        # tab duplicating it is the clutter it was rebuilt to get rid of. Everything that feeds the
        # analyst's view, including every control that only affects it, lives
        # in Details.
        tabPanel("Context",
                 tags$style(HTML(paste0(
                   ".ctx-wrap{max-width:1380px;margin-top:12px;}",
                   ".ctx-top{display:flex;align-items:flex-end;gap:18px;margin-bottom:6px;}",
                   ".ctx-top .form-group{margin-bottom:0;}",
                   ".ctx-card{background:#fff;border:1px solid #ececec;border-radius:6px;",
                   "padding:16px 18px;}",
                   # stat row
                   ".ctx-stats{display:flex;flex-wrap:wrap;gap:6px 30px;margin:2px 0 14px 0;}",
                   ".ctx-stat{min-width:96px;}",
                   ".ctx-stat-v{font-size:21px;font-weight:800;line-height:1.2;color:#111;}",
                   ".ctx-stat-l{font-size:10px;color:#888;text-transform:uppercase;",
                   "letter-spacing:.5px;margin-top:2px;}",
                   ".ctx-sub{color:#999;font-weight:400;font-size:13px;}",
                   ".ctx-flag{flex-basis:100%;margin-top:6px;padding:7px 10px;",
                   "background:#FAF6EC;border-left:3px solid #C9A227;font-size:12px;color:#5a4a1e;}",
                   # percentile chart
                   ".ctx-pk{font-size:10.5px;color:#888;text-transform:uppercase;",
                   "letter-spacing:.5px;margin:4px 0 8px 0;}",
                   ".ctx-prow{display:flex;align-items:center;gap:14px;padding:6px 0;}",
                   ".ctx-pl{width:118px;font-size:11px;color:#555;text-transform:uppercase;",
                   "letter-spacing:.4px;}",
                   ".ctx-pv{width:78px;font-size:14px;font-weight:700;text-align:right;color:#111;}",
                   ".ctx-pt-wrap{flex:1;position:relative;height:26px;}",
                   ".ctx-ptrack{position:absolute;left:0;right:0;top:12px;height:3px;",
                   "background:#e9e9e9;border-radius:2px;}",
                   ".ctx-pfill{position:absolute;left:0;top:12px;height:3px;border-radius:2px;}",
                   ".ctx-pdot{position:absolute;top:0;width:26px;height:26px;border-radius:50%;",
                   "transform:translateX(-50%);color:#fff;font-size:11px;font-weight:800;",
                   "display:flex;align-items:center;justify-content:center;",
                   "box-shadow:0 1px 2px rgba(0,0,0,.18);}",
                   ".ctx-pend{display:flex;justify-content:space-between;font-size:9.5px;",
                   "color:#aaa;text-transform:uppercase;letter-spacing:.4px;margin-top:-2px;}",
                   # arsenal
                   ".ctx-arow{display:grid;grid-template-columns:44px 58px 1fr 1fr;",
                   "align-items:center;gap:0 26px;padding:8px 0;border-bottom:1px solid #f3f3f3;}",
                   ".ctx-ahead{border-bottom:1px solid #ddd;padding:0 0 6px 0;}",
                   ".ctx-ahead div{font-size:10px;color:#888;font-weight:700;",
                   "text-transform:uppercase;letter-spacing:.5px;}",
                   ".ctx-pt{font-weight:800;font-size:14px;}",
                   ".ctx-use{display:flex;align-items:center;gap:6px;font-size:12px;color:#666;}",
                   ".ctx-usebar{height:6px;background:#c9ced6;border-radius:3px;}",
                   ".ctx-cell{display:flex;align-items:center;gap:10px;}",
                   ".ctx-bar-val{width:66px;font-size:13px;font-weight:700;text-align:right;}",
                   ".ctx-bar{flex:1;max-width:300px;margin:0 14px;}",
                   # profile, stacked in the side column
                   ".ctx-side .ctx-stats{display:grid;grid-template-columns:1fr 1fr;gap:16px 24px;}",
                   ".ctx-side .ctx-flag{grid-column:1 / -1;}",
                   ".ctx-line{color:#444;font-size:13px;margin:0 0 4px 0;}",
                   # details
                   ".ctx-details{margin-top:34px;border-top:1px solid #ddd;padding-top:14px;}",
                   ".ctx-details summary{cursor:pointer;font-weight:700;font-size:14px;color:#555;}",
                   ".ctx-dctl{background:#f7f7f7;border-radius:6px;padding:12px 16px 2px 16px;",
                   "margin:14px 0 16px 0;}"))),

                 div(class = "ctx-wrap",
                   # The only control on the visible page. Pitcher, dates and
                   # batter side are global; the cohort controls below it only
                   # ever changed the Details section, so that is where they
                   # went. A control that moves nothing visible teaches the
                   # reader the page is broken.
                   div(class = "ctx-top",
                       selectInput("ctx_pitch", "Pitch type",
                                   choices = names(pitch_colors), selected = "FF",
                                   width = "150px")),

                   fluidRow(
                     column(7,
                       div(class = "ctx-sec",
                           div(class = "ctx-h", "Percentile rankings"),
                           uiOutput("ctx_league_block"))),
                     column(5,
                       div(class = "ctx-sec ctx-side",
                           div(class = "ctx-h", "Profile"),
                           uiOutput("ctx_release_card")))
                   ),

                   div(class = "ctx-sec",
                       div(class = "ctx-h", "Arsenal"),
                       uiOutput("ctx_arsenal")),

                   tags$details(class = "ctx-details",
                     tags$summary("Details: peer groups, baselines, comparables"),
                     div(class = "ctx-dctl",
                       fluidRow(
                         column(4, radioButtons("ctx_mode", "Peer group",
                                                choices = c("25 nearest" = "fixed_k",
                                                            "Fixed window" = "window"),
                                                selected = "fixed_k", inline = TRUE)),
                         column(3, checkboxInput("ctx_adjust", "Control for velo", value = TRUE))
                       ),
                       conditionalPanel(
                         condition = "input.ctx_mode == 'window'",
                         fluidRow(
                           column(4, sliderInput("ctx_tol_z", "Release-height window (± ft)",
                                                 min = 0.05, max = 0.50, value = 0.15, step = 0.01)),
                           column(4, sliderInput("ctx_tol_arm", "Arm-angle window (± deg)",
                                                 min = 1, max = 20, value = 5, step = 0.5))
                         ))),
                     div(style = "max-width:940px;",
                         plotOutput("ctx_space", height = "300px")),
                     uiOutput("ctx_line_release"),
                     br(),
                     gt::gt_output("ctx_baselines"),
                     br(),
                     gt::gt_output("ctx_comparables"))
                 )),
        tabPanel("Search",
                 # The one tab that owns its own inputs. The pitcher selector,
                 # the dates and the batter side stay global and still apply:
                 # the dates and the side narrow the population searched, and
                 # the pitcher selector is where a result LANDS.
                 div(class = "sec",
                   div(class = "sec-h", "Find pitchers by shape"),
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
      # Label is the trait NAME only, as of 2026-09-08. It used to carry the
      # population range too, "Velocity (mph): 84.3 to 101.5", which was where
      # the range went when ticks = FALSE removed the grid. But ionRangeSlider
      # prints both handle values in bubbles directly below, and at full extent
      # those bubbles ARE the range, so the label was restating the two numbers
      # sitting an inch under it.
      column(4, sliderInput(paste0("s_", rg$trait[i]), spec$label,
                            min = rg$lo[i], max = rg$hi[i], ticks = FALSE,
                            value = c(rg$lo[i], rg$hi[i]), step = spec$step))
    })
    # Chunked from the actual slider count rather than hardcoded. This read
    # fluidRow(cols[1:3]), fluidRow(cols[4:5]) while SEARCH_TRAITS held exactly
    # five traits, so adding a sixth rendered five sliders and silently dropped
    # the new one: no error, no gap, just a control that was never drawn.
    rows <- split(cols, ceiling(seq_along(cols) / 3))
    # do.call, not tagList(!!!...). tagList collects with list(...) and does not
    # process rlang's splice operator, so !!! reaches it as a literal call and
    # errors at render.
    do.call(tagList, unname(lapply(rows, function(r) fluidRow(r))))
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
    plot_movement(pitcher_data())
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
  # One computation, two projections. arsenal_table() and arsenal_denoms() both
  # group over the window, so calling them once per table would double that work
  # on every input change for two tables that are by construction consistent.
  # This reactive is also the only thing keeping them consistent: split the
  # computation and a future edit to one table's filter silently desynchronises
  # the pitch counts between them.
  chars_parts <- reactive({
    d   <- pitcher_data()
    tbl <- arsenal_table(d, input$hand, stuff_all())
    list(tbl = tbl, denoms = arsenal_denoms(d, input$hand),
         p_throws = d$p_throws[1])
  })

  # resolve_table() is called separately per table rather than once and split,
  # because it narrows ARSENAL_METRIC_COLS by the columns present in the frame
  # it is handed. Resolving the wide table and slicing the cells afterwards
  # would work today and would break the first time a column name appears in one
  # table and not the other.
  output$traits_table <- gt::render_gt({
    p   <- chars_parts()
    tbl <- traits_tbl(p$tbl)
    traits_gt(tbl, input$hand,
              fg_window = if (is.null(FG_EXPORT)) NULL else FG_EXPORT$label,
              ref = resolve_table(tbl, p$denoms, league_ref, p$p_throws, input$hand))
  })

  output$results_table <- gt::render_gt({
    p   <- chars_parts()
    tbl <- results_tbl(p$tbl)
    results_gt(tbl, input$hand,
               ref = resolve_table(tbl, p$denoms, league_ref, p$p_throws, input$hand))
  })

  # ---- Context tab ---------------------------------------------------------
  #
  # One reactive builds the league-wide profile and one builds the shape table,
  # and every block on the tab projects from those two. Split them and a future
  # edit to one filter silently desynchronises the explorer from the cohorts,
  # which is the same failure chars_parts() exists to prevent.
  #
  # Both are scoped by the GLOBAL date range, so the explorer answers "unusual
  # over the window I am looking at" rather than always over the season. That
  # also means arm angle thins out on a short recent window, which is why
  # n_arm rides along and the note above the table reports it.
  ctx_window <- reactive({
    req(input$dates)
    from <- as.character(input$dates[1]); to <- as.character(input$dates[2])
    d <- app_data |> filter(game_date >= from, game_date <= to)
    validate(need(nrow(d) > 0, "No pitches in the selected window."))
    d
  })

  # The single source of truth for handedness on this tab. Read from the
  # pitcher's own rows rather than from ctx_profile(), so it still resolves when
  # he is below the Context minimum and the profile has dropped him.
  ctx_hand <- reactive({
    req(input$pitcher)
    h <- app_data$p_throws[app_data$pitcher == as.integer(input$pitcher)]
    validate(need(length(h) > 0, "No pitches for this pitcher."))
    as.character(h[1])
  })



  # Hardcoded, not inputs. 100 matches the Search tab's own minimum so the two
  # surfaces agree on who counts as a real pitcher, and 0.75 SD is the radius
  # the rarity counts were calibrated against.
  CTX_MIN_PITCHES <- 100
  CTX_RARITY_RADIUS <- 0.75

  ctx_profile <- reactive({
    pitcher_release_profile(ctx_window(), PITCHER_HEIGHTS,
                            min_pitches = CTX_MIN_PITCHES) |>
      expected_release_height() |>
      release_rarity(radius = CTX_RARITY_RADIUS)
  })

  # Scoped to the global batter-side selector, because the outcome rates it now
  # carries describe a split. Release point is NOT split this way: it is a
  # property of the pitcher, so ctx_profile() takes the whole window.
  ctx_shape <- reactive(pitch_shape(ctx_window(), hand = input$hand))

  ctx_mode <- reactive(input$ctx_mode %||% "fixed_k")

  # The sliders do not exist in fixed-K mode, so their inputs are NULL and the
  # defaults have to hold. %||% alone is not enough: a conditionalPanel that has
  # been shown once leaves the input behind at its last value, so this also has
  # to tolerate a stale number rather than assume NULL.
  ctx_tol <- reactive({
    t <- COHORT_TOLERANCES
    z <- suppressWarnings(as.numeric(input$ctx_tol_z))
    a <- suppressWarnings(as.numeric(input$ctx_tol_arm))
    if (length(z) == 1 && is.finite(z)) t$release_height <- z
    if (length(a) == 1 && is.finite(a)) t$arm_angle      <- a
    t
  })

  # A row of figures, no sentence. The verdict line above it ("releases the ball
  # about where a 6-3 pitcher from a high 3/4 slot normally does") and the
  # rounding caveat under it were both removed on 2026-09-18: the figures say
  # the same thing in a third of the space, and a caveat nobody can act on is
  # furniture on a page a coach reads in twenty seconds. The rounding is still
  # real and still documented in expected_release_height().
  output$ctx_release_card <- renderUI({
    p <- ctx_profile(); id <- as.integer(input$pitcher)
    r <- p[p$pitcher == id, ]
    if (!nrow(r)) return(div(class = "ctx-line", style = "color:#777;",
      "Widen the date range to bring this pitcher into the pool."))
    if (!is.finite(r$rel_z_resid[1])) return(div(class = "ctx-line", style = "color:#777;",
      "No release comparison: missing a listed height or an arm angle in this window."))

    slot <- arm_slot_label(r$arm[1])
    ht   <- sprintf("%d-%d", r$height_in[1] %/% 12, r$height_in[1] %% 12)
    gap  <- 12 * r$rel_z_resid[1]
    band <- 12 * (r$resid_hi[1] - r$rel_z_resid[1])

    box <- function(v, lab, sub = NULL) div(
      class = "ctx-stat",
      div(class = "ctx-stat-v", HTML(v)),
      div(class = "ctx-stat-l", lab))

    # EXPECTED and VS EXPECTED were two tiles until 2026-09-21 and neither said
    # expected by WHAT, once the explanatory caveat was cut. One tile, labelled
    # with its own basis. The expected height itself is still in the profile
    # for anyone who opens Details.
    div(class = "ctx-stats",
      box(ht, "listed height"),
      box(sprintf("%s <span class='ctx-sub'>%.0f\u00b0</span>", slot, r$arm[1]), "arm slot"),
      box(sprintf("%.2f <span class='ctx-sub'>ft</span>", r$rel_z[1]), "releases at"),
      box(if (abs(gap) <= band) "as predicted" else
            sprintf("%+.1f <span class='ctx-sub'>in</span>", gap), "vs slot + height"),
      box(sprintf("%s <span class='ctx-sub'>%d of %d</span>",
                  rarity_label(r$rarity_n[1]), r$rarity_n[1], r$rarity_pool[1]), "release point"),
      if (isTRUE(r$low_support[1])) div(
        class = "ctx-flag",
        sprintf("Only %d other %s throw from within %g\u00b0 of this slot, so there is no reliable normal for it.",
                r$arm_support[1],
                if (identical(r$p_throws[1], "R")) "right-handers" else "left-handers",
                r$support_window[1])) else NULL)
  })

  # The league percentile, said out loud. Reads the SAME league_ref the
  # Characteristics tab shades from, through the same lg_pctile(), so a sentence
  # here and a fill there can never disagree. What is new is only that it is a
  # sentence: "lower than 84% of LHP four-seams" is the unit a scouting report
  # repeats, and no surface in this app produced one.
  #
  # Ordered by distance from the 50th rather than by a fixed list, so the
  # unusual traits lead. A writeup opens with the thing that stands out, not
  # with velocity because velocity is first in the schema.
  # One percentile track: a faint fill to the percentile and a numbered dot on
  # it. Shared by the percentile chart and the Arsenal so the two read as one
  # scale. Near the 50th the app's ramp is nearly white, so white text on it
  # vanished: an extension at the 31st and a velocity at the 38th rendered as
  # blank circles. Light fills get dark text and a rim. faded marks a sample
  # under the floor.
  pctile_bubble <- function(q, faded = FALSE) {
    col <- pctile_fill(q, "high")
    rgb <- grDevices::col2rgb(col)[, 1]
    light <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) > 170
    dot_style <- sprintf("left:%.0f%%;background:%s;%s%s", q, col,
                         if (light) "color:#333;border:1px solid #bbb;" else "",
                         if (faded) "opacity:.45;" else "")
    div(class = "ctx-pt-wrap",
        div(class = "ctx-ptrack"),
        div(class = "ctx-pfill", style = sprintf("width:%.0f%%;background:%s;opacity:.35;", q, col)),
        div(class = "ctx-pdot", style = dot_style, sprintf("%.0f", q)))
  }

  output$ctx_league_block <- renderUI({
    id  <- as.integer(input$pitcher)
    shp <- ctx_shape()
    r   <- shp[shp$pitcher == id & as.character(shp$pitch_type) == input$ctx_pitch, , drop = FALSE]
    if (!nrow(r)) return(div(class = "ctx-note",
      sprintf("He does not throw enough %s in this window to rank.", input$ctx_pitch)))
    lp <- league_percentiles(r, league_ref, stand = input$hand,
                             metrics = c("velo","ivb","hb","vaa","spin","ext","rel_ht"))
    if (is.null(lp)) return(NULL)
    lp <- lp[order(-abs(lp$pctile - 50)), , drop = FALSE]
    hand_word <- if (identical(r$p_throws[1], "R")) "RHP" else "LHP"

    # ---- A percentile chart, not seven sentences ----
    #
    # The block was seven lines ending "...of LHP FFs", which is the problem
    # Savant's percentile-ranking chart was designed to solve and a format every
    # coach already reads. The dot sits at the RAW percentile, so right is
    # always "more of this trait" and the two end labels say what more means.
    #
    # Colour only where one end is better for the pitcher, per
    # METRIC_SPEC$context_better: velocity and extension. Ride, run, approach
    # angle, spin and release height are shapes, not grades, and a red dot on a
    # sinker's low ride would assert something the data cannot support. Those
    # render slate, and the note says why.
    ends <- function(m) {
      ph <- LEAGUE_PHRASE[[m]]
      c(sub(" than$", "", ph$low), sub(" than$", "", ph$high))
    }
    row <- function(i) {
      m   <- lp$metric[i]
      raw <- lp$pctile[i]
      # Red above the league, blue below, on every trait. Slate was tried for
      # the shape traits (ride, run, approach angle, spin, release height) on the
      # grounds that more of them is not better, and dropped 2026-09-21 by
      # request: the chart reads as one scale, and "red means more" is already
      # what the Characteristics tab's IVB and HB shading means.
      e   <- ends(m)
      val <- sprintf(paste0("%.", lp$digits[i], "f%s"), lp$value[i], lp$unit[i])
      div(class = "ctx-prow",
          div(class = "ctx-pl", lp$label[i],
              if (!lp$exact[i]) tags$span(style = "color:#aaa;", title = "coarser league cut", " \u2020")),
          div(class = "ctx-pv", val),
          div(style = "flex:1;",
              pctile_bubble(raw),
              # The high end only. The low end is its opposite and the reader
              # supplies it; printing both doubled the text under every track.
              div(class = "ctx-pend", style = "justify-content:flex-end;", tags$span(e[2]))))
    }
    tagList(
      div(class = "ctx-pk", sprintf("Percentile vs every %s %s", hand_word, input$ctx_pitch)),
      lapply(seq_len(nrow(lp)), row),
      div(class = "ctx-note", "Red is above the league, blue below."))
  })

  # His whole arsenal over the window and batter side on screen, not just the
  # selected pitch type. The pitch-type selector still drives the percentile
  # block above, which is a per-pitch question; these two blocks are about the
  # pitcher.
  ctx_arsenal_rows <- reactive({
    shp <- ctx_shape()
    shp[shp$pitcher == as.integer(input$pitcher), , drop = FALSE]
  })

  # NOT a table. The Characteristics tab already prints every one of these
  # numbers in a grid, and repeating a grid here made the page read as two
  # tables stacked. Two results per pitch, drawn as the same percentile bubbles
  # as the chart above (bars until 2026-09-21, changed by request so the page
  # reads as one scale).
  #
  # ---- Dot POSITION is "better for the pitcher", on every row ----
  #
  # The first version filled each bar to the raw league percentile. For whiff%
  # that is right, since more is better. For xwOBA it is backwards: a .370
  # four-seam sits at the 75th percentile of contact damage, so it drew a LONG
  # bar, and a long bar reads as "good" before anyone looks at the colour. The
  # colour said bad and the length said good, which is why the panel looked
  # inverted even though every colour on it was correct.
  #
  # So each bar now fills to the PITCHER percentile: the raw percentile for a
  # high-is-good metric, 100 minus it for a low-is-good one. Longer and redder
  # both mean better for him on every row, and the number beside the bar is that
  # same pitcher percentile. It is the convention Savant's own percentile
  # rankings use, for the same reason.
  #
  # The direction comes from METRIC_SPEC$context_better, not from this code.
  pitcher_pct <- function(p, metric) {
    if (!is.finite(p)) return(NA_real_)
    if (identical(context_better(metric), "low")) 100 - p else p
  }

  output$ctx_arsenal <- renderUI({
    d <- ctx_arsenal_rows()
    validate(need(nrow(d) > 0, "No pitch types clear the minimum in this window."))
    d <- d[order(-d$pitches), , drop = FALSE]
    tot <- sum(d$pitches, na.rm = TRUE)

    raw_pct <- function(i, m) {
      lp <- league_percentiles(d[i, , drop = FALSE], league_ref, stand = input$hand, metrics = m)
      if (is.null(lp) || !nrow(lp)) NA_real_ else lp$pctile[1]
    }
    fl <- function(m) METRIC_SPEC$floor[METRIC_SPEC$metric == m]

    bar <- function(q, val, thin) {
      if (!is.finite(q)) return(div(class = "ctx-cell",
        div(class = "ctx-bar-val", style = "color:#bbb;", val)))
      div(class = "ctx-cell",
          div(class = "ctx-bar-val", style = if (thin) "color:#999;font-style:italic;" else "", val),
          div(class = "ctx-bar", pctile_bubble(q, faded = thin)))
    }

    head <- div(class = "ctx-arow ctx-ahead",
                div("Pitch"), div("Usage"),
                div(style = "text-align:center;", "Whiff%"),
                div(style = "text-align:center;", "xwOBA"))

    rows <- lapply(seq_len(nrow(d)), function(i) {
      pt  <- as.character(d$pitch_type[i])
      col <- if (pt %in% names(pitch_text_colors)) pitch_text_colors[[pt]] else "#333"
      tw  <- is.finite(d$swings[i]) && d$swings[i] < fl("whiff_pct")
      tx  <- is.finite(d$pa[i])     && d$pa[i]     < fl("xwoba")
      wv  <- if (is.finite(d$whiff_pct[i]))
               paste0(sprintf("%.1f", d$whiff_pct[i]), if (tw) sprintf(" (%g)", d$swings[i]) else "") else "\u2014"
      xv  <- if (is.finite(d$xwoba[i]))
               paste0(sub("^0", "", sprintf("%.3f", d$xwoba[i])), if (tx) sprintf(" (%g)", d$pa[i]) else "") else "\u2014"
      u <- 100 * d$pitches[i] / tot
      div(class = "ctx-arow",
          div(class = "ctx-pt", style = sprintf("color:%s;", col), pt),
          # Usage drawn, not just printed, so the row's weight is visible: a 45%
          # four-seam and a 3% changeup should not look like equals.
          div(class = "ctx-use",
              div(class = "ctx-usebar", style = sprintf("width:%.0fpx;", max(2, u * 0.5))),
              sprintf("%.0f%%", u)),
          bar(pitcher_pct(raw_pct(i, "whiff_pct"), "whiff_pct"), wv, tw),
          bar(pitcher_pct(raw_pct(i, "xwoba"), "xwoba"), xv, tx))
    })

    tagList(
      div(class = "ctx-card", head, rows),
      div(class = "ctx-note", sprintf(
        "Percentile vs every %s throwing that pitch, scored so further right and redder is better for him. (n) = sample under the floor.",
        if (identical(ctx_hand(), "R")) "RHP" else "LHP")))
  })

  ctx_cohort <- reactive({
    build_cohort(ctx_shape(), ctx_profile(), as.integer(input$pitcher),
                 input$ctx_pitch,
                 match_on = c("release_height", "arm_angle"),
                 tolerances = ctx_tol(), mode = ctx_mode(), k = COHORT_K)
  })

  output$ctx_baselines <- gt::render_gt({
    id <- as.integer(input$pitcher)
    prof <- ctx_profile()
    validate(need(id %in% prof$pitcher,
                  "Selected pitcher is below the minimum pitch count in this window."))
    # Both metrics, always. The metric dropdown made the reader choose before
    # he had seen anything, and velo and IVB both fit.
    fb <- nested_baselines(ctx_shape(), prof, id, input$ctx_pitch,
                           metrics = c("velo", "ivb", "whiff_pct", "chase_pct", "xwoba"),
                           control_for = if (isTRUE(input$ctx_adjust)) "velo" else NULL,
                           tolerances = ctx_tol(), mode = ctx_mode(), k = COHORT_K)
    validate(need(!is.null(fb), "No cohort: this pitcher does not throw that pitch type enough in this window."))
    # mode and k already ride on fb from nested_baselines(); member_ids do too,
    # and baselines_gt() reads them for the overlap line.
    baselines_gt(fb, input$ctx_pitch,
                 if (identical(ctx_hand(), "R")) "RHP" else "LHP")
  })

  # Scoped to the SELECTED pitcher's hand, not the explorer's hand filter. The
  # explorer above browses either hand; this block is the drill-down on whoever
  # is in the pitcher selector, and drawing the righty cloud under a lefty
  # target puts the highlighted point outside its own population. It renders
  # cleanly and says something false, which is the worst failure a chart here
  # can have.
  output$ctx_space <- renderPlot({
    id   <- as.integer(input$pitcher)
    prof <- ctx_profile() |> filter(p_throws == ctx_hand())
    validate(need(nrow(prof) > 2, "Not enough pitchers to draw release space."))
    plot_release_space(prof, target_id = id,
                       cohort = ctx_cohort_rel(), pitch_type = input$ctx_pitch)
  })

  output$ctx_comparables <- gt::render_gt({
    co <- ctx_cohort()
    validate(need(!is.null(co) && nrow(co$members) > 0, paste0(
      "No comparables inside these windows. Widen the release-height or ",
      "arm-angle window above; the app will not widen them for you.")))
    comparables_gt(co)
  })
}


shinyApp(ui, server)
