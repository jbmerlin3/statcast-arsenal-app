# app.R
# Pitcher Arsenal: UI and server. Logic lives in R/, which Shiny sources automatically.

library(shiny)
library(dplyr)
library(ggplot2)


# ---- Data, loaded once at startup --------------------------------------------

# Shared by every session to stay under the free tier's memory limit
app_data     <- load_app_data()
league_ref   <- load_league_ref()
# Optional: without game logs the results panel shows only the Statcast row
game_logs    <- load_game_logs()
FIP_CONST    <- if (is.null(game_logs)) NA_real_ else fip_constant(game_logs)
LOG_THROUGH  <- if (is.null(game_logs)) NULL else max(game_logs$game_date)
player_index <- build_player_index(app_data, exclude = load_position_players())
TEAM_CODES   <- sort(unique(app_data$pitch_team))
HALVES       <- season_halves(app_data)
# Precomputed Search and Context league tables (NULL: compute live)
LEAGUE_POOL  <- load_league_pool(app_data)

# game_date is stored as character; the date inputs use Date
DATE_MIN <- HALVES$full[1]
DATE_MAX <- HALVES$full[2]

DEFAULT_PITCHER <- 702070  # Noah Cameron

STUFF_V2 <- load_stuff_v2()

PITCHER_HEIGHTS <- load_pitcher_heights()

EMPTY_STUFF <- tibble::tibble(pitch_type = character(), stuff_plus = numeric())


# Context tab note: pitches each trait needs, read from METRIC_SPEC$shape_floor
FLOOR_LABELS <- c(rel_ht = "release height", rel_side = "release side", ext = "extension",
                  velo = "velocity", spin = "spin", vaa = "approach angle", ivb = "ride",
                  hb = "run")
floor_n    <- vapply(names(FLOOR_LABELS), shape_floor, numeric(1))
floor_text <- paste(sprintf("%s %d", FLOOR_LABELS[order(floor_n)], sort(floor_n)), collapse = ", ")

# 1H and 2H appear once the season has a break
preset_buttons <- if (is.null(HALVES$first)) {
  actionButton("preset_all", "All", class = "btn-sm")
} else {
  tagList(actionButton("preset_all", "All", class = "btn-sm"),
          actionButton("preset_1h",  "1H",  class = "btn-sm"),
          actionButton("preset_2h",  "2H",  class = "btn-sm"))
}


# ---- UI ----------------------------------------------------------------------

ui <- fluidPage(
  # Spinner on recalculating outputs; stale results fade
  shiny::useBusyIndicators(spinners = TRUE, pulse = TRUE),
  tags$style(HTML(paste0(
    ".recalculating{opacity:.35 !important;transition:opacity .15s ease-in .25s;}",
    ".shiny-busy-pulse{--shiny-pulse-color:#1f3a5f;}"))),
  # Page styles (gt tables style themselves)
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
    # Usage tab layout
    ".usage-row{display:flex;flex-wrap:wrap;gap:0 28px;align-items:flex-start;}",
    ".usage-chart{flex:1 1 340px;min-width:0;}",
    ".usage-count{flex:0 1 auto;max-width:100%;overflow-x:auto;}",
    ".usage-count .gt_table{margin-left:0 !important;}",
    ".app-codes{font-size:11.5px;color:#999;margin:2px 0 6px 0;}",
    # Times-through pills
    ".tto-bar{display:flex;align-items:center;gap:14px;margin:14px 0 -8px 0;}",
    ".tto-lab{font-size:10.5px;color:#888;text-transform:uppercase;letter-spacing:.5px;}",
    ".tto-pills{display:flex;gap:6px;margin:0;}",
    ".tto-pill{margin:0;font-weight:400;cursor:pointer;}",
    ".tto-pill input{position:absolute;opacity:0;pointer-events:none;}",
    ".tto-pill span{display:inline-block;padding:4px 13px;border:1px solid #cfd5dd;",
    "border-radius:14px;font-size:12.5px;color:#1f3a5f;background:#fff;}",
    ".tto-pill .tto-n{font-weight:400;color:#8a94a3;margin-left:6px;font-size:11px;}",
    ".tto-pill input:checked+span{background:#1f3a5f;border-color:#1f3a5f;color:#fff;}",
    ".tto-pill input:checked+span .tto-n{color:#c8d3e3;}",
    ".tto-pill input:focus-visible+span{outline:2px solid #1f3a5f;outline-offset:2px;}",
    ".tto-pill input:disabled+span{opacity:.4;cursor:not-allowed;}",
    # Hover tooltips
    ".trend-tip{position:absolute;z-index:10;background:#fff;border:1px solid #cfd5dd;",
    "border-radius:6px;padding:7px 10px;font-size:12.5px;line-height:1.45;",
    "box-shadow:0 2px 8px rgba(0,0,0,.12);pointer-events:none;white-space:nowrap;}",
    ".trend-tip b{color:#1f3a5f;}",
    ".trend-changes{margin:14px 0 4px 0;padding:9px 14px;background:#f5f8fb;border-left:3px solid #1f3a5f;border-radius:3px;}",
    ".trend-changes ul{margin:4px 0 0 0;padding-left:18px;font-size:13.5px;line-height:1.6;}",
    ".trend-none{font-size:13px;color:#666;margin-top:3px;}",
    "#trends img, #mix_outings img{cursor:crosshair;}"
  )))),
  # Date read from the data, so a stale deploy shows a stale date
  titlePanel(div("Pitcher Arsenal",
                 span(paste("Data through", DATE_MAX),
                      style = "font-size:14px; color:#666; margin-left:12px;")),
             windowTitle = "Pitcher Arsenal"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("pitcher", "Pitcher", choices = NULL),
      dateRangeInput("dates", "Date range",
                     start = DATE_MIN, end = DATE_MAX,
                     min   = DATE_MIN, max = DATE_MAX),
      div(style = "margin-bottom:15px;", preset_buttons),
      radioButtons("hand", "Batter side",
                   choices  = c("All" = "All", "vs LHH" = "L", "vs RHH" = "R"),
                   selected = "All", inline = TRUE),
      helpText("Every pitch type is in the tables. Rates from small samples carry",
               " their own denominator."),
      shiny::tags$head(shiny::tags$style(shiny::HTML(RESULTS_PANEL_CSS))),
      shiny::tags$hr(style = "margin:14px 0 12px 0; border-color:#e0e0e0;"),
      uiOutput("results_panel")
    ),
    mainPanel(
      width = 9,
      div(class = "app-codes", uiOutput("pitch_code_note")),
      tabsetPanel(
        id = "tabs",
        tabPanel("Movement",
                 div(class = "sec",
                     div(class = "sec-h", "Movement profile"),
                     div(class = "sec-sub", "Induced vertical vs horizontal break, pitcher's view. About 100 pitches shown, in proportion to usage"),
                     plotOutput("movement", height = "620px"))),
        tabPanel("Usage",
                 uiOutput("tto_toggle"),
                 div(class = "usage-row",
                     div(class = "sec usage-chart",
                         div(class = "sec-h", "Pitch usage"),
                         plotOutput("usage", height = "330px"),
                         div(class = "sec-note",
                             "Always both batter sides. Usage by count and the mix by outing follow the Batter side selector.")),
                     div(class = "sec usage-count",
                         div(class = "sec-h", "Usage by count"),
                         gt::gt_output("usage_table"))),
                 div(class = "sec",
                     div(class = "sec-h", "Pitch mix by outing"),
                     div(class = "sec-sub", "One bar per outing, as wide as his pitch count. Hover for the pitch's velo and break."),
                     div(style = "position:relative;",
                         plotOutput("mix_outings", height = "300px",
                                    hover = hoverOpts("mix_hover", delay = 60,
                                                      delayType = "debounce")),
                         uiOutput("mix_tip")))),
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
                     plotOutput("heatmap", height = "700px"),
                     div(class = "sec-note", paste0(
                       "Two count buckets, before and with two strikes, not the six in the usage table: ",
                       "a density estimate needs more pitches per panel than a usage percentage does. ",
                       "Panels under ", KDE_MIN_N, " pitches show the raw locations as white ",
                       "dots instead of a smoothed surface.")))),
        tabPanel("Trends",
                 uiOutput("trend_changes_ui"),
                 uiOutput("trend_pitch_ui"),
                 div(class = "sec",
                     div(class = "sec-h", "Game by game"),
                     div(class = "sec-sub", paste0("One point per game. Dashed line: average over the date range. ",
                                                   "Grey band: his normal game-to-game range. ",
                                                   "Blue: the last ", TREND_LAST_N, " games.")),
                     div(style = "position:relative;",
                         plotOutput("trends", height = "780px",
                                    hover = hoverOpts("trends_hover", delay = 60,
                                                      delayType = "debounce")),
                         uiOutput("trends_tip")),
                     div(class = "sec-note", paste0(
                       "Velocity, spin and movement are the selected pitch against both batter sides; a game ",
                       "needs ", TREND_MIN_N, " of it to plot. Usage has a dot for every outing, sized by its ",
                       "pitches, and follows the Batter side selector; its line pools the last ",
                       TREND_USAGE_ROLL, " outings, more until it holds ", TREND_USAGE_POOL, " pitches.")))),
        tabPanel("Context",
                 tags$style(HTML(paste0(
                   ".ctx-wrap{max-width:1380px;margin-top:12px;}",
                   ".ctx-top{display:flex;align-items:flex-end;gap:18px;margin-bottom:6px;}",
                   ".ctx-top .form-group{margin-bottom:0;}",
                   ".ctx-card{background:#fff;border:1px solid #ececec;border-radius:6px;",
                   "padding:16px 18px;}",
                   ".ctx-stats{display:flex;flex-wrap:wrap;gap:6px 30px;margin:2px 0 14px 0;}",
                   ".ctx-stat{min-width:96px;}",
                   ".ctx-stat-v{font-size:21px;font-weight:800;line-height:1.2;color:#111;}",
                   ".ctx-stat-l{font-size:10px;color:#888;text-transform:uppercase;",
                   "letter-spacing:.5px;margin-top:2px;}",
                   ".ctx-sub{color:#999;font-weight:400;font-size:13px;}",
                   ".ctx-flag{flex-basis:100%;margin-top:6px;padding:7px 10px;",
                   "background:#FAF6EC;border-left:3px solid #C9A227;font-size:12px;color:#5a4a1e;}",
                   ".ctx-pk{font-size:10.5px;color:#888;text-transform:uppercase;",
                   "letter-spacing:.5px;margin:4px 0 8px 0;}",
                   ".ctx-prow{display:flex;align-items:center;gap:14px;padding:6px 0;}",
                   ".ctx-pl{width:118px;font-size:11px;color:#555;text-transform:uppercase;",
                   "letter-spacing:.4px;}",
                   ".ctx-pv{width:78px;font-size:14px;font-weight:700;text-align:right;color:#111;}",
                   ".ctx-pt-wrap{flex:1;position:relative;height:26px;margin:0 13px;}",
                   ".ctx-ptrack{position:absolute;left:0;right:0;top:12px;height:3px;",
                   "background:#e9e9e9;border-radius:2px;}",
                   ".ctx-pfill{position:absolute;left:0;top:12px;height:3px;border-radius:2px;}",
                   ".ctx-pdot{position:absolute;top:0;width:26px;height:26px;border-radius:50%;",
                   "transform:translateX(-50%);color:#fff;font-size:11px;font-weight:800;",
                   "display:flex;align-items:center;justify-content:center;",
                   "box-shadow:0 1px 2px rgba(0,0,0,.18);}",
                   ".ctx-pend{display:flex;justify-content:space-between;font-size:9.5px;",
                   "color:#aaa;text-transform:uppercase;letter-spacing:.4px;margin-top:-2px;}",
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
                   ".ctx-bar{flex:1;max-width:300px;}",
                   ".ctx-side .ctx-stats{display:grid;grid-template-columns:1fr 1fr;gap:16px 24px;}",
                   ".ctx-side .ctx-flag{grid-column:1 / -1;}",
                   ".ctx-line{color:#444;font-size:13px;margin:0 0 4px 0;}"))),

                 div(class = "ctx-wrap",
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
                       uiOutput("ctx_arsenal"))
                 )),
        tabPanel("Search",
                 div(class = "sec",
                   div(class = "sec-h", "Find pitchers by shape"),
                   fluidRow(
                     column(2, selectInput("s_throws", "Pitcher hand",
                                           choices = c("RHP" = "R", "LHP" = "L"))),
                     column(2, selectInput("s_pitch", "Pitch type",
                                           choices = names(pitch_colors), selected = "FF")),
                     column(2, selectInput("s_team", "Team",
                                           choices = c("All teams" = "All", TEAM_CODES))),
                     column(3, numericInput("s_min", "Min pitches", value = 25,
                                            min = 1, step = 5)),
                     column(3, div(style = "margin-top:25px;",
                                   actionButton("s_reset", "Reset sliders", class = "btn-sm")))
                   ),
                   uiOutput("search_sliders"),
                   uiOutput("search_count"),
                   gt::gt_output("search_table")
                 ))
      )
    )
  )
)


# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # ---- Pitcher and date window -----------------------------------------------
  updateSelectizeInput(session, "pitcher",
                       choices  = player_choices(player_index),
                       selected = DEFAULT_PITCHER,
                       server   = TRUE)

  set_range <- function(r) updateDateRangeInput(session, "dates", start = r[1], end = r[2])
  observeEvent(input$preset_all, set_range(HALVES$full))
  if (!is.null(HALVES$first)) {
    observeEvent(input$preset_1h, set_range(HALVES$first))
    observeEvent(input$preset_2h, set_range(HALVES$second))
  }

  # The selected pitcher and dates. Not filtered by batter side: the tables and plots
  # filter `stand` themselves. Raw, so batters faced survives when no pitch is chartable.
  pitcher_window <- reactive({
    req(input$pitcher, input$dates)
    app_data |> filter(pitcher == as.integer(input$pitcher),
                       game_date >= as.character(input$dates[1]),
                       game_date <= as.character(input$dates[2]))
  })

  # The same window with pitch codes reconciled and pitch types counted
  pitcher_data <- reactive({
    raw <- pitcher_window()
    validate(need(nrow(raw) > 0,
                  "No pitches for this pitcher in the selected window."))

    d <- shape_arsenal(raw)

    validate(need(nrow(d) > 0, {
      note <- pitch_code_note(d)
      paste0(nrow(raw), " pitches in this window, but none chartable.",
             if (!is.null(note)) paste0(" ", note, ".")
             else " No pitch carried a chartable type code.")
    }))
    d
  })

  chart_hide <- reactive(chart_hidden_types(pitcher_data()))

  output$pitch_code_note <- renderUI({
    d <- pitcher_data()
    codes <- pitch_code_note(d)
    parts <- c(if (!is.null(codes)) paste0("Pitch codes: ", codes),
               chart_hidden_note(d, chart_hide()))
    if (!length(parts)) return(NULL)
    div(style = "color:#666; font-size:12px; margin-bottom:6px;",
        paste0(paste(parts, collapse = ". "), "."))
  })

  # ---- Search tab ------------------------------------------------------------
  # The league pool depends only on dates and side; sliders just filter it
  search_pool <- reactive({
    req(input$dates, input$s_team)
    from <- as.character(input$dates[1])
    to   <- as.character(input$dates[2])
    hit <- if (identical(input$s_team, "All")) pool_search(LEAGUE_POOL, from, to, input$hand)
    if (!is.null(hit)) return(hit)
    search_aggregate(app_data, input$hand, input$s_team, from = from, to = to)
  })

  search_bounds <- reactive({
    req(input$s_throws, input$s_pitch, input$s_min)
    search_ranges(search_pool(), input$s_throws, input$s_pitch, input$s_min)
  })

  output$search_sliders <- renderUI({
    rg <- search_bounds()
    input$s_reset
    cols <- lapply(seq_len(nrow(rg)), function(i) {
      spec <- SEARCH_TRAITS[SEARCH_TRAITS$trait == rg$trait[i], ]
      column(4, sliderInput(paste0("s_", rg$trait[i]), spec$label,
                            min = rg$lo[i], max = rg$hi[i], ticks = FALSE,
                            value = c(rg$lo[i], rg$hi[i]), step = spec$step))
    })
    rows <- split(cols, ceiling(seq_along(cols) / 3))
    do.call(tagList, unname(lapply(rows, function(r) fluidRow(r))))
  })

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
    search_filter(search_pool(), input$s_throws, input$s_pitch, bounds, input$s_min,
                  sort_by = sort_state$col, desc = sort_state$desc)
  })

  output$search_count <- renderUI({
    res <- search_results()
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
    validate(need(search_bounds()$n[1] > 0, paste0(
      "No ", if (input$s_throws == "R") "RHP" else "LHP", " throws a ",
      input$s_pitch, " with ", input$s_min, "+ pitches in this window.")))
    validate(need(nrow(res) > 0,
                  "No pitcher matches. Widen a slider, or lower the minimum pitch count."))
    shown <- head(res, SEARCH_MAX_ROWS)
    search_gt(shown, input$s_pitch, input$s_throws, input$hand,
              n_total = nrow(res), sort_by = sort_state$col, desc = sort_state$desc)
  })

  # A clicked name loads that pitcher and switches to Movement
  observeEvent(input$search_pick, {
    id <- suppressWarnings(as.integer(input$search_pick))
    req(!is.na(id), id %in% player_index$pitcher)
    updateSelectizeInput(session, "pitcher", choices = player_choices(player_index),
                         selected = id, server = TRUE)
    updateTabsetPanel(session, "tabs", selected = "Movement")
  })

  # ---- Movement tab ----------------------------------------------------------
  # Used by every plot: on a cold load the browser can report a width of 0, so wait
  sized_width <- function(id) function() {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    req(!is.null(w), is.finite(w), w > 0)
    w
  }

  output$movement <- renderPlot({
    plot_movement(pitcher_data(), hide = chart_hide())
  }, width = sized_width("movement"))

  # ---- Trends tab ------------------------------------------------------------
  trend_choices <- reactive(trend_pitch_choices(pitcher_data(), chart_hide()))
  trend_pt <- reactive({
    ch <- names(trend_choices())
    req(length(ch) > 0)
    if (isTRUE(input$trend_pitch %in% ch)) input$trend_pitch else ch[1]
  })

  output$trend_pitch_ui <- renderUI({
    ch  <- trend_choices()
    sel <- isolate(trend_pt())
    pill <- function(code) tags$label(class = "tto-pill",
      tags$input(type = "radio", name = "trend_pitch", value = code,
                 checked = if (identical(code, sel)) NA),
      tags$span(code, tags$b(class = "tto-n", format(ch[[code]], big.mark = ","))))
    div(class = "tto-bar",
        div(class = "tto-lab", "Pitch"),
        div(id = "trend_pitch", class = "shiny-input-radiogroup tto-pills", role = "radiogroup",
            `aria-label` = "Pitch type", unname(lapply(names(ch), pill))))
  })

  trend_tp <- reactive(trend_panels(pitcher_data(), trend_pt(), input$hand))

  output$trends <- renderPlot(plot_trends(trend_tp()), width = sized_width("trends"))
  mix_p <- reactive(plot_mix_outings(filter_tto(pitcher_data(), tto()), input$hand, chart_hide()))
  output$mix_outings <- renderPlot({
    p <- mix_p()
    req(p)
    p
  }, width = sized_width("mix_outings"))

  output$mix_tip <- renderUI({
    cl <- input$mix_hover
    p <- mix_p()
    if (is.null(cl) || is.null(p) || is.null(cl$x) || is.null(cl$y)) return(NULL)
    md <- p$data
    hit <- md[cl$x >= md$x0 & cl$x < md$x1 & cl$y >= md$y0 & cl$y < md$y1, ]
    if (nrow(hit) == 0) return(NULL)
    lines <- mix_tip_text(hit[1, ], input$hand, tto())
    x <- round(cl$coords_css$x); y <- round(cl$coords_css$y)
    w <- session$clientData$output_mix_outings_width
    h <- session$clientData$output_mix_outings_height
    flip_x <- isTRUE(x > 0.6 * w)
    flip_y <- isTRUE(y > 0.6 * h)
    div(class = "trend-tip",
        style = sprintf("left:%dpx;top:%dpx;transform:translate(%s,%s);",
                        x + if (flip_x) -12 else 12, y + if (flip_y) -10 else 10,
                        if (flip_x) "-100%" else "0", if (flip_y) "-100%" else "0"),
        tags$b(lines[1]), lapply(lines[-1], div))
  })

  output$trend_changes_ui <- renderUI({
    lines <- trend_change_text(trend_changes(pitcher_data(), input$hand, chart_hide()), input$hand)
    div(class = "trend-changes",
        div(class = "tto-lab", paste0("Last ", TREND_LAST_N, " games vs his average")),
        if (length(lines) == 0) div(class = "trend-none", "Nothing outside his normal game-to-game range.")
        else tags$ul(lapply(lines, tags$li)))
  })

  output$trends_tip <- renderUI({
    cl <- input$trends_hover
    tp <- trend_tp()
    if (is.null(cl)) return(NULL)
    hit <- nearPoints(tp$pts, cl, xvar = "game_date", yvar = "v", panelvar1 = "panel",
                      threshold = 12, maxpoints = 1)
    if (nrow(hit) == 0) return(NULL)
    lines <- trend_tip_text(hit[1, ], tp)
    x <- round(cl$coords_css$x); y <- round(cl$coords_css$y)
    w <- session$clientData$output_trends_width
    h <- session$clientData$output_trends_height
    flip_x <- isTRUE(x > 0.6 * w)
    flip_y <- isTRUE(y > 0.8 * h)
    div(class = "trend-tip",
        style = sprintf("left:%dpx;top:%dpx;transform:translate(%s,%s);",
                        x + if (flip_x) -12 else 12, y + if (flip_y) -10 else 10,
                        if (flip_x) "-100%" else "0", if (flip_y) "-100%" else "0"),
        tags$b(lines[1]), lapply(lines[-1], div))
  })

  # ---- Usage tab -------------------------------------------------------------
  # Outputs read tto(), not input$tto, which keeps a stale pick when the toggle hides
  tto_n <- reactive(tto_counts(pitcher_data(), input$hand))
  tto   <- reactive(tto_effective(input$tto, tto_n()))

  output$tto_toggle <- renderUI(tto_toggle_ui(tto_n(), isolate(tto())))

  output$usage <- renderPlot({
    d <- filter_tto(pitcher_data(), tto())
    validate(need(nrow(d) > 0, "No pitches in this view."))
    plot_usage(d, hide = chart_hide())
  }, width = sized_width("usage"))

  output$usage_table <- gt::render_gt({
    count_usage_gt(count_usage_tbl(pitcher_data(), input$hand, tto()), input$hand,
                   label = tto_table_label(input$hand, tto()), heading = NULL)
  })

  # ---- Results panel ---------------------------------------------------------
  # League over the same window and side, since IP is a counting stat
  results_ref <- reactive({
    req(input$dates)
    results_league(app_data, game_logs, input$dates, input$hand, FIP_CONST)
  })

  output$results_panel <- renderUI({
    sc <- results_statcast(pitcher_window(), input$hand)
    gl <- if (is.null(game_logs)) {
      list(have = FALSE, games = 0L, through = NA_character_)
    } else {
      results_gamelog(game_logs, input$pitcher, input$dates, FIP_CONST)
    }
    results_panel(sc, gl, input$dates, input$hand, LOG_THROUGH,
                  ctx = results_context(sc, gl, results_ref()))
  })

  # ---- Characteristics tab ---------------------------------------------------
  stuff_all <- reactive({
    req(input$pitcher, input$dates)
    if (is.null(STUFF_V2)) return(EMPTY_STUFF)
    stuff_for(STUFF_V2, as.integer(input$pitcher), input$dates, input$hand)
  })

  # One pass feeds both tables so their pitch counts match
  chars_parts <- reactive({
    d   <- pitcher_data()
    tbl <- arsenal_table(d, input$hand, stuff_all())
    list(tbl = tbl, denoms = arsenal_denoms(d, input$hand),
         p_throws = d$p_throws[1])
  })

  output$traits_table <- gt::render_gt({
    p   <- chars_parts()
    tbl <- traits_tbl(p$tbl)
    traits_gt(tbl, input$hand,
              ref = resolve_table(tbl, p$denoms, league_ref, p$p_throws, input$hand))
  })

  output$results_table <- gt::render_gt({
    p   <- chars_parts()
    tbl <- results_tbl(p$tbl)
    results_gt(tbl, input$hand,
               ref = resolve_table(tbl, p$denoms, league_ref, p$p_throws, input$hand))
  })

  # ---- Heat Maps tab ---------------------------------------------------------
  output$heatmap <- renderPlot({
    plot_heatmap(pitcher_data(), input$hand, hide = chart_hide())
  }, width = sized_width("heatmap"))

  # ---- Context tab -----------------------------------------------------------
  ctx_range <- reactive({
    req(input$dates)
    from <- as.character(input$dates[1]); to <- as.character(input$dates[2])
    validate(need(any(app_data$game_date >= from & app_data$game_date <= to),
                  "No pitches in the selected window."))
    c(from, to)
  })

  ctx_hand <- reactive({
    req(input$pitcher)
    h <- app_data$p_throws[app_data$pitcher == as.integer(input$pitcher)]
    validate(need(length(h) > 0, "No pitches for this pitcher."))
    as.character(h[1])
  })

  # Rarity radius is in standard deviations
  CTX_MIN_PITCHES <- 100
  CTX_RARITY_RADIUS <- 0.75

  ctx_profile <- reactive({
    r <- ctx_range()
    prof <- pool_release(LEAGUE_POOL, r[1], r[2])
    prof <- if (is.null(prof)) {
      pitcher_release_profile(window_rows(app_data, r[1], r[2], RELEASE_PROFILE_COLS),
                              PITCHER_HEIGHTS, min_pitches = CTX_MIN_PITCHES)
    } else {
      prof[prof$n >= CTX_MIN_PITCHES, , drop = FALSE]
    }
    prof |>
      expected_release_height() |>
      release_rarity(radius = CTX_RARITY_RADIUS)
  })

  ctx_shape <- reactive({
    r <- ctx_range()
    agg <- pool_search(LEAGUE_POOL, r[1], r[2], input$hand)
    if (is.null(agg)) agg <- search_aggregate(app_data, input$hand, from = r[1], to = r[2])
    pitch_shape_finish(agg, min_pitches = 1)
  })

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

    box <- function(v, lab) div(
      class = "ctx-stat",
      div(class = "ctx-stat-v", HTML(v)),
      div(class = "ctx-stat-l", lab))

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

  pctile_bubble <- function(q, faded = FALSE) {
    col <- pctile_fill(q, "high")
    rgb_val <- grDevices::col2rgb(col)[, 1]
    light <- (0.299 * rgb_val[1] + 0.587 * rgb_val[2] + 0.114 * rgb_val[3]) > 170
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
      sprintf("No %s in this window and batter side.", input$ctx_pitch)))
    n_pitches <- r$pitches[1]
    lp <- league_percentiles(r, league_ref, stand = input$hand,
                             metrics = c("velo","ivb","hb","vaa","spin",
                                         "ext","rel_ht","rel_side"))
    if (is.null(lp)) return(NULL)
    # Most unusual traits first, release traits kept together
    REL_GROUP <- c("ext", "rel_ht", "rel_side")
    dev <- abs(lp$pctile - 50)
    in_rel <- lp$metric %in% REL_GROUP
    if (any(in_rel)) dev[in_rel] <- max(dev[in_rel])
    lp <- lp[order(-dev, ifelse(in_rel, match(lp$metric, REL_GROUP), 0L)), , drop = FALSE]
    hand_word <- if (identical(r$p_throws[1], "R")) "RHP" else "LHP"

    ends <- function(m) {
      ph <- LEAGUE_PHRASE[[m]]
      c(sub(" than$", "", ph$low), sub(" than$", "", ph$high))
    }
    row <- function(i) {
      m   <- lp$metric[i]
      raw <- lp$pctile[i]
      e   <- ends(m)
      val <- sprintf(paste0("%.", lp$digits[i], "f%s"), lp$value[i], lp$unit[i])
      # Faded when the sample is under the trait's stability floor
      fl   <- shape_floor(m)
      thin <- is.finite(fl) && n_pitches < fl
      div(class = "ctx-prow",
          div(class = "ctx-pl", lp$label[i]),
          div(class = "ctx-pv", style = if (thin) "color:#999;font-style:italic;" else "", val),
          div(style = "flex:1;",
              pctile_bubble(raw, faded = thin),
              div(class = "ctx-pend", style = "justify-content:flex-end;", tags$span(e[2]))))
    }
    tagList(
      div(class = "ctx-pk", sprintf("Percentile vs every %s %s", hand_word, input$ctx_pitch),
          tags$span(style = "text-transform:none;color:#aaa;",
                    sprintf(" \u00b7 %d pitches", n_pitches))),
      lapply(seq_len(nrow(lp)), row),
      div(class = "ctx-note", paste0(
        "Red is above the league, blue below. A faded trait has too few pitches here ",
        "to place precisely. Pitches needed: ", floor_text, ".")))
  })

  ctx_arsenal_rows <- reactive({
    shp <- ctx_shape()
    shp[shp$pitcher == as.integer(input$pitcher), , drop = FALSE]
  })

  observe({
    d  <- ctx_arsenal_rows()
    ch <- as.character(d$pitch_type[order(-d$pitches)])
    if (!length(ch)) return()
    cur <- isolate(input$ctx_pitch)
    updateSelectInput(session, "ctx_pitch", choices = ch,
                      selected = if (isTRUE(cur %in% ch)) cur else ch[1])
  })

  # Further right is better for the pitcher, so low-is-good metrics are flipped
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

    header_row <- div(class = "ctx-arow ctx-ahead",
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
          div(class = "ctx-use",
              div(class = "ctx-usebar", style = sprintf("width:%.0fpx;", max(2, u * 0.5))),
              sprintf("%.0f%%", u)),
          bar(pitcher_pct(raw_pct(i, "whiff_pct"), "whiff_pct"), wv, tw),
          bar(pitcher_pct(raw_pct(i, "xwoba"), "xwoba"), xv, tx))
    })

    tagList(
      div(class = "ctx-card", header_row, rows),
      div(class = "ctx-note", sprintf(
        "Percentile vs every %s throwing that pitch, scored so further right and redder is better for him. (n) = sample under the floor.",
        if (identical(ctx_hand(), "R")) "RHP" else "LHP")))
  })
}


shinyApp(ui, server)
