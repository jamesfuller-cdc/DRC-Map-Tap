source("R/data.R")
source("R/game.R")

library(shiny)
library(leaflet)
library(sf)

game_data <- load_game_data()

ui <- fluidPage(
  tags$head(
    tags$title("DRC Daily Map"),
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
    tags$script(src = "app.js?v=2")
  ),
  div(
    class = "game-shell",
    div(
      class = "game-header",
      div(
        class = "header-row",
        h1("DRC Daily Map"),
        tags$details(
          class = "help-details header-help",
          tags$summary("How to play"),
          div(
            class = "help-popover",
            tags$p("Each day, the game selects one DRC province, and you receive five questions about that province. Question 1 asks you to locate the province; Questions 2 and 3 ask you to locate cities or towns within it; Questions 4 and 5 ask you to locate health zones within it."),
            tags$p("For each question, click the map to place your marker, then select Submit guess. You can pan and zoom before submitting. The target is revealed after your guess is locked."),
            tags$p("Each question is worth up to 100 points, for a maximum of 500. The closer your guess, the higher your score; guesses inside polygon targets and exact city guesses receive full credit."),
            tags$p("The daily puzzle refreshes at 12:01 AM Eastern.")
          )
        )
      ),
      div(class = "date-line", textOutput("puzzle_date", inline = TRUE))
    ),
    div(
      class = "quiz-card",
      div(
        class = "quiz-main-row",
        div(class = "question-badge", textOutput("question_label", inline = TRUE)),
        div(class = "target-prompt", uiOutput("target_prompt", inline = TRUE)),
        uiOutput("question_action", inline = TRUE)
      )
    ),
    uiOutput("question_result"),
    uiOutput("final_result"),
    leafletOutput("map", height = "65vh"),
    div(class = "attribution", textOutput("imagery_attribution", inline = TRUE)),
    tags$footer(
      class = "site-footer",
      tags$a(
        href = "https://github.com/jamesfuller-cdc/DRC-Map-Tap",
        target = "_blank",
        rel = "noopener noreferrer",
        "View the code on GitHub"
      ),
      tags$span(class = "footer-separator", "·"),
      tags$a(
        href = "https://github.com/jamesfuller-cdc/DRC-Map-Tap/issues",
        target = "_blank",
        rel = "noopener noreferrer",
        "Report a bug or request a feature"
      )
    )
  )
)

server <- function(input, output, session) {
  initial_date <- daily_puzzle_date()
  puzzle_date <- reactiveVal(initial_date)
  puzzle <- reactiveVal(make_daily_puzzle(game_data, initial_date))
  state <- reactiveValues(question = 1L, guesses = vector("list", 5), guess = NULL, submitted = FALSE, finished = FALSE)

  observe({
    invalidateLater(60000, session)
    current_date <- daily_puzzle_date()
    if (!identical(current_date, puzzle_date())) {
      puzzle_date(current_date)
      puzzle(make_daily_puzzle(game_data, current_date))
      state$question <- 1L
      state$guesses <- vector("list", 5)
      state$guess <- NULL
      state$submitted <- FALSE
      state$finished <- FALSE
    }
  })

  output$puzzle_date <- renderText(sprintf("Puzzle for %s", format(puzzle_date(), "%B %d, %Y")))
  output$imagery_attribution <- renderText(imagery_attribution())

  output$question_label <- renderText({
    if (state$finished) "Complete" else sprintf("Question %d of 5", state$question)
  })
  output$target_prompt <- renderUI({
    if (state$finished) return(HTML("Daily puzzle complete"))
    target <- puzzle()[[state$question]]
    HTML(sprintf("%s <strong>%s</strong>", target_prompt_prefix(target), htmltools::htmlEscape(target_label_text(target))))
  })

  output$question_action <- renderUI({
    if (state$finished) return(NULL)
    if (state$submitted && state$question == 5L) return(actionButton("finish_game", "See total score", class = "secondary-button"))
    if (state$submitted) return(actionButton("next_question", "Next question", class = "secondary-button"))
    actionButton("submit_guess", "Submit guess", class = "primary-button", disabled = TRUE)
  })

  output$map <- renderLeaflet({
    tile_url <- Sys.getenv("DRC_MAP_TILES_URL", unset = "")
    if (!nzchar(tile_url) && !dir.exists("www/tiles")) {
      tile_url <- "https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless-2024_3857/default/g/{z}/{y}/{x}.jpg"
    }
    target <- puzzle()[[state$question]]
    province_hint <- NULL
    if (target$type %in% c("city", "health_zone")) {
      province_hint <- game_data$provinces[game_data$provinces$name == target$province_name, ]
      if (!nrow(province_hint)) province_hint <- NULL
    }
    view_bbox <- if (!is.null(province_hint)) st_bbox(province_hint) else st_bbox(game_data$drc_border)
    m <- leaflet(options = leafletOptions(zoomControl = TRUE, doubleClickZoom = FALSE)) |>
      addMapPane("healthZonePane", zIndex = 650) |>
      fitBounds(lng1 = unname(view_bbox[["xmin"]]), lat1 = unname(view_bbox[["ymin"]]),
                lng2 = unname(view_bbox[["xmax"]]), lat2 = unname(view_bbox[["ymax"]]))
    if (nzchar(tile_url)) {
      m <- m |> addTiles(urlTemplate = tile_url, options = tileOptions(opacity = 1, noWrap = TRUE), group = "Satellite imagery")
    } else if (dir.exists("www/tiles")) {
      m <- m |> addTiles(urlTemplate = "tiles/{z}/{x}/{y}.png", options = tileOptions(opacity = 1, noWrap = TRUE), group = "Satellite imagery")
    }
    m <- m |> addPolygons(data = game_data$drc_border, color = map_colors$drc_border, weight = 3,
                          fill = FALSE, group = "DRC border")
    if (!is.null(province_hint)) {
      m <- m |> addPolygons(data = province_hint, color = map_colors$hint_boundary, weight = 3,
                            fill = FALSE,
                            group = "hint province")
    }
    if (target$type == "health_zone" && !is.null(province_hint)) {
      province_key <- normalize_place_key(target$province_name)
      zone_hints <- game_data$health_zones_display_by_province[[province_key]]
      if (is.null(zone_hints)) {
        display_zones <- game_data$health_zones
        province_matches <- normalize_place_key(display_zones$province_name) == province_key
        zone_hints <- display_zones[province_matches, ]
      }
      if (!is.null(zone_hints) && nrow(zone_hints)) {
        m <- m |> addPolygons(data = zone_hints, color = map_colors$hint_boundary, weight = 1.5,
                              opacity = 1, fill = FALSE,
                              options = pathOptions(pane = "healthZonePane"),
                              group = "health zone hints")
      }
    }
    m
  })

  observeEvent(input$map_click, {
    req(!state$submitted, !state$finished)
    state$guess <- input$map_click
    leafletProxy("map") |>
      clearGroup("current guess") |>
      addCircleMarkers(lng = input$map_click$lng, lat = input$map_click$lat,
                       radius = 8, color = map_colors$player_guess, fillColor = map_colors$player_guess, fillOpacity = 1,
                       weight = 3, group = "current guess")
    updateActionButton(session, "submit_guess", disabled = FALSE)
  })

  observeEvent(input$submit_guess, {
    req(state$guess, !state$submitted, !state$finished)
    target <- puzzle()[[state$question]]
    result <- score_guess(state$guess$lng, state$guess$lat, target)
    state$guesses[[state$question]] <- result
    state$submitted <- TRUE
    updateActionButton(session, "submit_guess", disabled = TRUE)
    view_geometry <- if (target$type == "province") {
      game_data$drc_border
    } else {
      province <- game_data$provinces[game_data$provinces$name == target$province_name, , drop = FALSE]
      if (nrow(province)) province else game_data$drc_border
    }
    view_bbox <- st_bbox(view_geometry)
    leafletProxy("map") |>
      clearGroup("revealed target") |>
      reveal_target(target) |>
      fitBounds(lng1 = unname(view_bbox[["xmin"]]), lat1 = unname(view_bbox[["ymin"]]),
                lng2 = unname(view_bbox[["xmax"]]), lat2 = unname(view_bbox[["ymax"]]))
  })

  observeEvent(input$next_question, {
    req(state$submitted, state$question < 5)
    state$question <- state$question + 1L
    state$guess <- NULL
    state$submitted <- FALSE
    updateActionButton(session, "submit_guess", disabled = TRUE)
    # The question change rebuilds the map and adds the new question's hint layers.
    # Do not clear the health-zone group here: doing so can race the reactive
    # map render and remove the newly added health-zone boundaries.
    leafletProxy("map") |> clearGroup("current guess") |> clearGroup("revealed target")
  })

  observeEvent(input$finish_game, {
    req(state$question == 5L, state$submitted, !state$finished)
    state$finished <- TRUE
  })

  output$question_result <- renderUI({
    req(state$submitted, !state$finished)
    result <- state$guesses[[state$question]]
    div(class = "result-card", h2(sprintf("%d points", result$score)),
        p(sprintf("You were %s km away.", format(round(result$distance_km, 1), nsmall = 1))))
  })

  output$final_result <- renderUI({
    req(state$finished)
    scores <- vapply(state$guesses, function(x) x$score, numeric(1))
    share_text <- make_share_text(state$guesses, puzzle_date())
    div(class = "result-card final-card", h2(class = "final-score-title", sprintf("Your Score: %d / 500", sum(scores))),
        div(class = "share-box",
            actionButton("share", "Copy Score to Clipboard", class = "secondary-button",
                         `data-share-text` = share_text),
            span(id = "copy-status", class = "copy-status")),
        tags$table(class = "score-table",
          tags$thead(tags$tr(tags$th("Guess #"), tags$th("What you needed to find"), tags$th("Distance (km)"), tags$th("Score"))),
          tags$tbody(lapply(seq_along(scores), function(i) {
            tags$tr(tags$td(sprintf("%d", i)),
                    tags$td(target_label_text(puzzle()[[i]])),
                    tags$td(format(round(state$guesses[[i]]$distance_km, 1), nsmall = 1)),
                    tags$td(sprintf("%d", scores[[i]])))
          }))))
  })
}

shinyApp(ui, server)
