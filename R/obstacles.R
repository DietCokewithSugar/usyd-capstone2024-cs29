library(shiny)

obstaclesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        div(
          style = "text-align: center;",
          actionButton(ns("add_obstacle"), "Add Obstacle")
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(
          style = "text-align: center;",
          uiOutput(ns("obstacle_tiles"))
        )
      )
    )
  )
}

obstaclesServer <- function(id, userInput) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # observeEvent(userInput$wall_height(), {
    #   wall_height <- userInput$wall_height()
    # })
    # observeEvent(userInput$wall_width(), {
    #   wall_width <- userInput$wall_width()
    # })
    # print(userInput)
    wall_height <- userInput$wall_height()
    wall_width <- userInput$wall_width()

    obstacles <- reactiveVal(list())
    observeEvent(input$add_obstacle, {
      showModal(
        modalDialog(
          title = "Add New Obstacle",
          textInput(ns("new_obstacle_name"), "Obstacle Name", width = "100%"),
          div(style = "color: red; font-size: 12px; margin-top: -10px;", "*Required"),

          fluidRow(
            column(
              6,
              numericInput(
                ns("new_obstacle_width"),
                "Obstacle Width",
                value = 100,
                min = 1
              ),
              div(style = "color: red; font-size: 12px; margin-top: -10px;", "*Required")
            ),
            column(
              6,
              numericInput(
                ns("new_obstacle_height"),
                "Obstacle Height",
                value = 100,
                min = 1
              ),
              div(style = "color: red; font-size: 12px; margin-top: -10px;", "*Required")
            )
          ),

          fluidRow(
            column(
              6,
              numericInput(
                ns("top"),
                "Distance from Top",
                value = 50,
                min = 1
              ),
              div(style = "color: red; font-size: 12px; margin-top: -10px;", "*Required")
            ),
            column(
              6,
              numericInput(
                ns("left"),
                "Distance from Left",
                value = 50,
                min = 1
              ),
              div(style = "color: red; font-size: 12px; margin-top: -10px;", "*Required")
            )
          ),

          footer = tagList(div(
            style = "width: 100%; text-align: center;",
            div(modalButton("Cancel"), style = "display: inline-block; width: 48%; margin-right: 2%;"),
            div(actionButton(
              ns("confirm_add_obstacle"), "Add Obstacle"
            ), style = "display: inline-block; width: 48%;")
          ))
        )
      )
    })
    observeEvent(input$confirm_add_obstacle, {
      req(
        input$new_obstacle_name,
        input$new_obstacle_width,
        input$new_obstacle_height,
        input$top,
        input$left
      )

      wall_height <- userInput$wall_height()
      wall_width <- userInput$wall_width()

      obstacle_width <- input$new_obstacle_width
      obstacle_height <- input$new_obstacle_height
      top <- input$top
      left <- input$left

      obstacle_right_edge <- left + obstacle_width
      obstacle_bottom_edge <- top + obstacle_height

      if (left < 0 || top < 0 ||
          obstacle_right_edge > wall_width ||
          obstacle_bottom_edge > wall_height) {

        showModal(modalDialog(
          title = "Invalid Obstacle Position",
          "The obstacle is outside the wall boundaries. Please adjust its dimensions or position.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))

      } else {
        current_obstacles <- obstacles()
        new_obstacle <- list(
          name = input$new_obstacle_name,
          width = obstacle_width,
          height = obstacle_height,
          top = top,
          left = left,
          id = paste0("obstacle_", length(current_obstacles) + 1)
        )
        obstacles(append(current_obstacles, list(new_obstacle)))
        removeModal()
      }
    })

    observeEvent(input$delete_obstacle, {
      obs_id <- input$delete_obstacle
      current_obstacles <- obstacles()
      updated_obstacles <- Filter(function(x)
        x$id != obs_id, current_obstacles)
      obstacles(updated_obstacles)
    })

    output$obstacle_tiles <- renderUI({
      tiles <- lapply(obstacles(), function(obstacle) {
        name_length <- nchar(obstacle$name)
        width <- max(100, name_length * 10)
        height <- max(50, name_length * 5)
        div(
          style = "display: inline-flex; align-items: center; justify-content: space-between; border: 5px solid #ccc; padding: 10px; margin: 5px; flex-wrap: wrap;",
          span(h5(obstacle$name), style = "margin: 0; margin-right: 10px; flex: 1 1 auto;"),

          actionButton(
            ns("delete_obstacle"),
            "x",
            style = "background-color: red; color: white; border: none; padding: 5px 10px; cursor: pointer; flex-shrink: 0;",
            onclick = sprintf(
              "Shiny.onInputChange('%s', '%s');",
              ns("delete_obstacle"),
              obstacle$id
            )
          )


        )
      })
      do.call(tagList, tiles)
    })

    return(obstacles)
  })
}
