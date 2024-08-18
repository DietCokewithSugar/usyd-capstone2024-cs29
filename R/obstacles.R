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

obstaclesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    obstacles <- reactiveVal(list())
    
    observeEvent(input$add_obstacle, {
      showModal(modalDialog(
        title = "Add New Obstacle",
        textInput(ns("new_obstacle_name"), "Obstacle Name", width = "100%"),
        fluidRow(
          column(6, numericInput(ns("new_obstacle_width"), "Obstacle Width", value = 100, min = 1)),
          column(6, numericInput(ns("new_obstacle_height"), "Obstacle Height", value = 100, min = 1))
        ),
        fluidRow(
          column(6, numericInput(ns("top"), "Distance from Top", value = 50, min = 1)),
          column(6, numericInput(ns("left"), "Distance from Left", value = 50, min = 1))
        ),
        fluidRow(
          column(6, numericInput(ns("right"), "Distance from Right", value = 50, min = 1)),
          column(6, numericInput(ns("bottom"), "Distance from Bottom", value = 50, min = 1))
        ),
        footer = tagList(
          div(style = "width: 100%; text-align: center;",
              div(modalButton("Cancel"), style = "display: inline-block; width: 48%; margin-right: 2%;"),
              div(actionButton(ns("confirm_add_obstacle"), "Add Obstacle"), style = "display: inline-block; width: 48%;")
          )
        )
      ))
    })
    
    observeEvent(input$confirm_add_obstacle, {
      req(input$new_obstacle_name, input$new_obstacle_width, input$new_obstacle_height,
          input$top,
          input$left,
          input$right,
          input$bottom)
      
      current_obstacles <- obstacles()
      new_obstacle <- list(
        name = input$new_obstacle_name,
        width = input$new_obstacle_width,
        height = input$new_obstacle_height,
        top = input$top,
        left = input$left,
        right = input$right,
        bottom = input$bottom,
        id = paste0("obstacle_", length(current_obstacles) + 1)
      )
      obstacles(append(current_obstacles, list(new_obstacle)))
      removeModal()
    })
    
    observeEvent(input$delete_obstacle, {
      obs_id <- input$delete_obstacle
      current_obstacles <- obstacles()
      updated_obstacles <- Filter(function(x) x$id != obs_id, current_obstacles)
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
          actionButton(ns("delete_obstacle"), "x", 
                       style = "background-color: red; color: white; border: none; padding: 5px 10px; cursor: pointer; flex-shrink: 0;", 
                       onclick = sprintf("Shiny.onInputChange('%s', '%s');", ns("delete_obstacle"), obstacle$id))
        )
      })
      do.call(tagList, tiles)
    })
    
    return(obstacles)
    
    
  })
}