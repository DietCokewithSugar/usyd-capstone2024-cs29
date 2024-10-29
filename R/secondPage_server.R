# R/secondPage_server.R

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
          id = paste0("obstacle_", as.integer(Sys.time()))  # Unique ID generation using the current timestamp
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
