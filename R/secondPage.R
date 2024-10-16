library(shiny)

secondPage_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        style = "display: flex; justify-content: center; align-items: flex-start; height: 85vh; margin-top: 15vh;",
        div(
          style = "background-color: white; padding: 20px; border-radius: 10px; width: 50%;",
          
          # Existing inputs for wall dimensions, tile size, etc.
          numericInput(ns("wall_height"), "Wall Height:", value = 100, min = 100, max = 3000, width = '100%'),
          numericInput(ns("wall_width"), "Wall Width:", value = 100, min = 100, max = 1200, width = '100%'),
          numericInput(ns("wall_offset"), "Tile Offset:", value = 0, min = 0, max = 500, width = '100%'),
          
          selectInput(
            ns("tile_size"),
            "Tile Size:",
            choices = c("50x50", "75x75", "100x100", "150x150", "200x200", "300x300", "400x400", "450x450", "600x600", "900x900"),
            selected = "50x50",
            width = '100%'
          ),
          selectInput(
            ns("wall_grout"),
            "Tile Grout:",
            choices = c(2, 3, 4, 5, 6, 7, 8, 9, 10),
            selected = 2,
            width = '100%'
          ),
          
          div(
            style = "margin-top: 20px;",
            actionButton(ns("submit_button"), "Next", style = "width: 100%; background-color: #add8e6; color: black; margin-bottom: 10px;"),
            actionButton(ns("back"), "Back", style = "width: 100%; background-color: #ffa500; color: white; margin-top: 10px;")
          ),
          
          # Side-by-Side Obstacle Form
          div(
            style = "margin-top: 20px;",
            h4("Add Obstacle"),
            fluidRow(
              column(
                width = 4,
                textInput(ns("new_obstacle_name"), "Obstacle Name", width = "100%")
              ),
              column(
                width = 4,
                numericInput(ns("new_obstacle_width"), "Obstacle Width", value = 100, min = 1)
              ),
              column(
                width = 4,
                numericInput(ns("new_obstacle_height"), "Obstacle Height", value = 100, min = 1)
              )
            ),
            fluidRow(
              column(
                width = 4,
                numericInput(ns("top"), "Distance from Top", value = 50, min = 1)
              ),
              column(
                width = 4,
                numericInput(ns("left"), "Distance from Left", value = 50, min = 1)
              ),
              column(
                width = 4,
                actionButton(ns("add_obstacle"), "Add Obstacle", style = "width: 100%; background-color: #4CAF50; color: white; margin-top: 25px;")
              )
            )
          ),
          
          # Display the obstacles
          div(
            uiOutput(ns("obstacle_tiles")),
            style = "margin-top: 20px;"
          )
        )
      )
    )
  )
}

secondPage_server <- function(id, switch_ui) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store obstacles
    obstacles <- reactiveVal(list())
    
    # Function to reset the form inputs after adding an obstacle
    reset_obstacle_form <- function() {
      updateTextInput(session, "new_obstacle_name", value = "")
      updateNumericInput(session, "new_obstacle_width", value = 100)
      updateNumericInput(session, "new_obstacle_height", value = 100)
      updateNumericInput(session, "top", value = 50)
      updateNumericInput(session, "left", value = 50)
    }
    
    # Event handler for adding a new obstacle
    observeEvent(input$add_obstacle, {
      req(
        input$new_obstacle_name,
        input$new_obstacle_width,
        input$new_obstacle_height,
        input$top,
        input$left
      )
      
      # Fetch wall dimensions
      wall_height <- input$wall_height
      wall_width <- input$wall_width
      
      # Get the obstacle dimensions and position
      obstacle_width <- input$new_obstacle_width
      obstacle_height <- input$new_obstacle_height
      top <- input$top
      left <- input$left
      
      # Validate the obstacle is within bounds
      obstacle_right_edge <- left + obstacle_width
      obstacle_bottom_edge <- top + obstacle_height
      
      if (left < 0 || top < 0 || obstacle_right_edge > wall_width || obstacle_bottom_edge > wall_height) {
        showNotification("Invalid obstacle position. Please adjust its dimensions or position.", type = "error")
      } else {
        # Add the obstacle to the reactive obstacles list
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
        
        # Reset the form inputs after adding the obstacle
        reset_obstacle_form()
      }
    })
    
    # Render the list of obstacles
    output$obstacle_tiles <- renderUI({
      tiles <- lapply(obstacles(), function(obstacle) {
        div(
          style = "display: inline-block; padding: 10px; border: 1px solid #ccc; margin: 5px;",
          h5(obstacle$name),
          p(paste("Width:", obstacle$width, "Height:", obstacle$height)),
          p(paste("Position: Top", obstacle$top, "Left", obstacle$left)),
          actionButton(ns(paste0("delete_", obstacle$id)), "Delete", style = "background-color: red; color: white;")
        )
      })
      do.call(tagList, tiles)
    })
    
    # Handle dynamic delete button clicks using reactive inputs
    observe({
      lapply(obstacles(), function(obstacle) {
        observeEvent(input[[paste0("delete_", obstacle$id)]], {
          current_obstacles <- obstacles()
          updated_obstacles <- Filter(function(x) x$id != obstacle$id, current_obstacles)
          obstacles(updated_obstacles)
        })
      })
    })
    
    # Handle 'Next' button click to pass data to the next page
    observeEvent(input$submit_button, {
      final_values <- list(
        wall_height = input$wall_height,
        wall_width = input$wall_width,
        wall_offset = input$wall_offset,
        wall_grout = input$wall_grout,
        tile_size = input$tile_size,
        obstacles = obstacles()  # Pass the obstacles to the next page
      )
      
      switch_ui("landingPage", final_values)
    })
  })
}

# secondPage_server <- function(id, switch_ui) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Reactive value to store obstacles
#     obstacles <- reactiveVal(list())
#     
#     # Event handler for adding a new obstacle
#     observeEvent(input$add_obstacle, {
#       req(
#         input$new_obstacle_name,
#         input$new_obstacle_width,
#         input$new_obstacle_height,
#         input$top,
#         input$left
#       )
#       
#       # Fetch wall dimensions
#       wall_height <- input$wall_height
#       wall_width <- input$wall_width
#       
#       # Get the obstacle dimensions and position
#       obstacle_width <- input$new_obstacle_width
#       obstacle_height <- input$new_obstacle_height
#       top <- input$top
#       left <- input$left
#       
#       # Validate the obstacle is within bounds
#       obstacle_right_edge <- left + obstacle_width
#       obstacle_bottom_edge <- top + obstacle_height
#       
#       if (left < 0 || top < 0 || obstacle_right_edge > wall_width || obstacle_bottom_edge > wall_height) {
#         showNotification("Invalid obstacle position. Please adjust its dimensions or position.", type = "error")
#       } else {
#         # Add the obstacle to the reactive obstacles list
#         current_obstacles <- obstacles()
#         new_obstacle <- list(
#           name = input$new_obstacle_name,
#           width = obstacle_width,
#           height = obstacle_height,
#           top = top,
#           left = left,
#           id = paste0("obstacle_", length(current_obstacles) + 1)
#         )
#         obstacles(append(current_obstacles, list(new_obstacle)))
#         
#         # Reset the form inputs after adding the obstacle
#         updateTextInput(session, "new_obstacle_name", value = "")
#         updateNumericInput(session, "new_obstacle_width", value = 100)
#         updateNumericInput(session, "new_obstacle_height", value = 100)
#         updateNumericInput(session, "top", value = 50)
#         updateNumericInput(session, "left", value = 50)
#       }
#     })
#     
#     # Render the list of obstacles
#     output$obstacle_tiles <- renderUI({
#       tiles <- lapply(obstacles(), function(obstacle) {
#         div(
#           style = "display: inline-block; padding: 10px; border: 1px solid #ccc; margin: 5px;",
#           h5(obstacle$name),
#           p(paste("Width:", obstacle$width, "Height:", obstacle$height)),
#           p(paste("Position: Top", obstacle$top, "Left", obstacle$left)),
#           actionButton(ns(paste0("delete_", obstacle$id)), "Delete", style = "background-color: red; color: white;")
#         )
#       })
#       do.call(tagList, tiles)
#     })
#     
#     # Handle obstacle deletion
#     observe({
#       lapply(obstacles(), function(obstacle) {
#         observeEvent(input[[paste0("delete_", obstacle$id)]], {
#           current_obstacles <- obstacles()
#           updated_obstacles <- Filter(function(x) x$id != obstacle$id, current_obstacles)
#           obstacles(updated_obstacles)
#         })
#       })
#     })
#     
#     # Handle 'Next' button click to pass data to the next page
#     observeEvent(input$submit_button, {
#       final_values <- list(
#         wall_height = input$wall_height,
#         wall_width = input$wall_width,
#         wall_offset = input$wall_offset,
#         wall_grout = input$wall_grout,
#         tile_size = input$tile_size,
#         obstacles = obstacles()  # Pass the obstacles to the next page
#       )
#       
#       switch_ui("landingPage", final_values)
#     })
#   })
# }
