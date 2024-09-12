library(shiny)

finalPage_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(
      HTML("
        .button-shadow {
          box-shadow: 0px 8px 15px rgba(0, 0, 0, 0.4);
          transition: all 0.3s ease;
        }
        .button-shadow:hover {
          box-shadow: 0px 15px 20px rgba(0, 0, 0, 0.5);
          transform: translateY(-3px);
        }
        .centered-container {
          display: flex;
          justify-content: center;
          align-items: center;
          height: calc(100vh - 120px); /* full height minus space for buttons */
        }
        .bottom-controls {
          position: fixed;
          bottom: 20px;
          width: 100%;
          display: flex;
          flex-direction: column;
          align-items: center;
        }
        .button-group {
          display: flex;
          justify-content: space-between;
          width: 320px;
        }
        .tile-count-container {
          display: flex;
          justify-content: center;
          margin-bottom: 10px;
        }
        .tile-count {
          margin-right: 20px;
          font-size: 16px;
          font-weight: bold;
        }
      ")
    )),
    
    fluidPage(
      # Center the tile plot container in the middle of the screen
      div(class = "centered-container", uiOutput(ns("dynamicWallPlot"))),
      
      # Bottom controls container that holds tile counts and buttons
      div(
        class = "bottom-controls",
        # Tile counts displayed above the arrow buttons
        div(
          class = "tile-count-container",
          div(class = "tile-count", textOutput(ns("fullTileCount"))),
          div(class = "tile-count", textOutput(ns("splitTileCount")))
        ),
        # Arrow buttons and controls at the bottom
        div(
          class = "button-group",
          actionButton(
            ns("left"),
            "",
            icon = icon("arrow-left"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("up"),
            "",
            icon = icon("arrow-up"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("down"),
            "",
            icon = icon("arrow-down"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("right"),
            "",
            class = "button-shadow",
            icon = icon("arrow-right"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("reset"),
            "",
            class = "button-shadow",
            icon = icon("refresh"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          )
        )
      )
    )
  )
}

# finalPage_server <- function(id, input_data) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Define reactive values to store the position of the tile grid and tile counts
#     values <- reactiveValues(
#       offset_x = 0,
#       offset_y = 0,
#       full_tiles = 0,
#       split_tiles = 0,
#       adjusted_wh = 0,
#       adjusted_ww = 0,
#       adjusted_th = 0,
#       adjusted_tw = 0,
#       scale_factor = 1
#     )
#     
#     # Calculate adjusted dimensions and scaling factor globally within the module
#     calculate_adjusted_dimensions <- function() {
#       # Extract wall dimensions and tile sizes
#       wh <- input_data$wall_height
#       ww <- input_data$wall_width
#       th <- switch(input_data$tile_size, Small = 30, Medium = 50, Large = 70)
#       tw <- switch(input_data$tile_size, Small = 30, Medium = 50, Large = 70)
#       
#       # Calculate scaling factor based on available screen size
#       max_height <- 0.9 * session$clientData$output_wallPlot_height
#       max_width <- 0.9 * session$clientData$output_wallPlot_width
#       scale_factor <- min(max_height / wh, max_width / ww, 1)
#       
#       # Adjusted dimensions and tile sizes based on scale
#       values$adjusted_wh <- wh * scale_factor
#       values$adjusted_ww <- ww * scale_factor
#       values$adjusted_th <- th * scale_factor
#       values$adjusted_tw <- tw * scale_factor
#       values$scale_factor <- scale_factor
#     }
#     
#     # Movement control logic (adjusts grid positions)
#     observeEvent(input$up, { 
#       values$offset_y <- values$offset_y + 10
#       recalculate_tiles()
#     })
#     observeEvent(input$down, { 
#       values$offset_y <- values$offset_y - 10
#       recalculate_tiles()
#     })
#     observeEvent(input$left, { 
#       values$offset_x <- values$offset_x - 10
#       recalculate_tiles()
#     })
#     observeEvent(input$right, { 
#       values$offset_x <- values$offset_x + 10
#       recalculate_tiles()
#     })
#     observeEvent(input$reset, {
#       values$offset_x <- 0
#       values$offset_y <- 0
#       values$full_tiles <- 0
#       values$split_tiles <- 0
#       recalculate_tiles()
#     })
#     
#     # Render tile plot in UI
#     output$dynamicWallPlot <- renderUI({
#       plotOutput(ns("wallPlot"), height = "60vh", width = "60vw")
#     })
#     
#     # Render the tile grid and recalculate the tile counts
#     output$wallPlot <- renderPlot({
#       calculate_adjusted_dimensions()  # Ensure the dimensions are calculated
#       draw_tiles_and_box()
#     })
#     
#     # Function to recalculate the tiles and tile counts when moving the red box
#     recalculate_tiles <- function() {
#       full_tiles <- 0
#       split_tiles <- 0
#       # Recalculate the number of full and split tiles
#       y_position <- values$offset_y
#       row_counter <- 1
#       while (y_position < values$adjusted_wh + 100) {
#         x_position <- values$offset_x + ifelse(row_counter %% 2 == 0, input_data$wall_offset * values$scale_factor, 0)
#         while (x_position < values$adjusted_ww + 100) {
#           # Check if the tile is fully inside the red box
#           is_full_tile <- (
#             x_position >= 0 && x_position + values$adjusted_tw <= values$adjusted_ww &&
#               y_position >= 0 && y_position + values$adjusted_th <= values$adjusted_wh
#           )
#           
#           # Check if the tile is touching the red line (intersecting with the edges of the red box)
#           is_touching_red_line <- (
#             (x_position >= 0 && x_position + values$adjusted_tw == values$adjusted_ww) ||  # Right edge
#               (x_position == 0) ||  # Left edge
#               (y_position == 0) ||  # Top edge
#               (y_position + values$adjusted_th == values$adjusted_wh)  # Bottom edge
#           )
#           
#           # Update tile counts
#           if (is_full_tile) {
#             full_tiles <- full_tiles + 1
#           } else if (is_touching_red_line) {
#             split_tiles <- split_tiles + 1
#           }
#           
#           x_position <- x_position + values$adjusted_tw + input_data$wall_grout * values$scale_factor
#         }
#         y_position <- y_position + values$adjusted_th + input_data$wall_grout * values$scale_factor
#         row_counter <- row_counter + 1
#       }
#       
#       # Update the full and split tile counts
#       values$full_tiles <- full_tiles
#       values$split_tiles <- split_tiles
#     }
#     
#     # Function to draw the tiles, red box, and obstacles
#     draw_tiles_and_box <- function() {
#       # Set up the plot window
#       plot.new()
#       plot.window(xlim = c(0, values$adjusted_ww), ylim = c(0, values$adjusted_wh), asp = values$adjusted_ww / values$adjusted_wh)
#       
#       # Draw the tiles across the wall area with offsets
#       y_position <- values$offset_y
#       row_counter <- 1
#       while (y_position < values$adjusted_wh + 100) {
#         x_position <- values$offset_x + ifelse(row_counter %% 2 == 0, input_data$wall_offset * values$scale_factor, 0)
#         while (x_position < values$adjusted_ww + 100) {
#           # Check if the tile is fully inside the red box
#           is_full_tile <- (
#             x_position >= 0 && x_position + values$adjusted_tw <= values$adjusted_ww &&
#               y_position >= 0 && y_position + values$adjusted_th <= values$adjusted_wh
#           )
#           
#           # Check if the tile is touching the red line (intersecting with the edges of the red box)
#           is_touching_red_line <- (
#             (x_position >= 0 && x_position + values$adjusted_tw == values$adjusted_ww) ||  # Right edge
#               (x_position == 0) ||  # Left edge
#               (y_position == 0) ||  # Top edge
#               (y_position + values$adjusted_th == values$adjusted_wh)  # Bottom edge
#           )
#           
#           # Set color based on whether the tile is full, touching the red line, or outside
#           tile_color <- if (is_full_tile) {
#             "lightblue"  # Full tile inside the red box
#           } else if (is_touching_red_line) {
#             "pink"  # Touching the red line
#           } else {
#             "lightgray"  # Outside and not touching the red line
#           }
#           
#           # Draw each tile with the appropriate color
#           polygon(
#             c(x_position, x_position, x_position + values$adjusted_tw, x_position + values$adjusted_tw),
#             c(y_position, y_position + values$adjusted_th, y_position + values$adjusted_th, y_position),
#             col = tile_color,
#             border = "black"
#           )
#           
#           x_position <- x_position + values$adjusted_tw + input_data$wall_grout * values$scale_factor
#         }
#         y_position <- y_position + values$adjusted_th + input_data$wall_grout * values$scale_factor
#         row_counter <- row_counter + 1
#       }
#       
#       # Draw the red wall boundary
#       rect(0, 0, values$adjusted_ww, values$adjusted_wh, border = "red", lwd = 3)
#       
#       # Draw the obstacles based on the provided dimensions (top, left, width, height)
#       for (obstacle in input_data$obstacles) {
#         # Scale obstacle dimensions based on the wall scale
#         obstacle_top <- obstacle$top * values$scale_factor
#         obstacle_left <- obstacle$left * values$scale_factor
#         obstacle_width <- obstacle$width * values$scale_factor
#         obstacle_height <- obstacle$height * values$scale_factor
#         
#         # Draw obstacle
#         rect(
#           obstacle_left,
#           values$adjusted_wh - obstacle_top,  # Flip y-axis for plotting
#           obstacle_left + obstacle_width,
#           values$adjusted_wh - obstacle_top - obstacle_height,
#           col = "orange", border = "black", lwd = 2
#         )
#       }
#     }
#     
#     # Render tile counts
#     output$fullTileCount <- renderText({
#       paste("Full Tiles:", values$full_tiles)
#     })
#     output$splitTileCount <- renderText({
#       paste("Split Tiles:", values$split_tiles)
#     })
#   })
# }

finalPage_server <- function(id, input_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define reactive values to store the position of the tile grid and tile counts
    values <- reactiveValues(
      offset_x = 0,
      offset_y = 0,
      full_tiles = 0,
      split_tiles = 0,
      adjusted_wh = 0,
      adjusted_ww = 0,
      adjusted_th = 0,
      adjusted_tw = 0,
      scale_factor = 1
    )

    # Calculate adjusted dimensions and scaling factor globally within the module
    calculate_adjusted_dimensions <- function() {
      # Extract wall dimensions and tile sizes
      wh <- input_data$wall_height
      ww <- input_data$wall_width
      th <- switch(input_data$tile_size, Small = 30, Medium = 50, Large = 70)
      tw <- switch(input_data$tile_size, Small = 30, Medium = 50, Large = 70)

      # Calculate scaling factor based on available screen size
      max_height <- 0.9 * session$clientData$output_wallPlot_height
      max_width <- 0.9 * session$clientData$output_wallPlot_width
      scale_factor <- min(max_height / wh, max_width / ww, 1)

      # Adjusted dimensions and tile sizes based on scale
      values$adjusted_wh <- wh * scale_factor
      values$adjusted_ww <- ww * scale_factor
      values$adjusted_th <- th * scale_factor
      values$adjusted_tw <- tw * scale_factor
      values$scale_factor <- scale_factor
    }

    # Movement control logic (adjusts grid positions)
    observeEvent(input$up, {
      values$offset_y <- values$offset_y + 10
      recalculate_tiles()
    })
    observeEvent(input$down, {
      values$offset_y <- values$offset_y - 10
      recalculate_tiles()
    })
    observeEvent(input$left, {
      values$offset_x <- values$offset_x - 10
      recalculate_tiles()
    })
    observeEvent(input$right, {
      values$offset_x <- values$offset_x + 10
      recalculate_tiles()
    })
    observeEvent(input$reset, {
      values$offset_x <- 0
      values$offset_y <- 0
      values$full_tiles <- 0
      values$split_tiles <- 0
      recalculate_tiles()
    })

    # Render tile plot in UI
    output$dynamicWallPlot <- renderUI({
      plotOutput(ns("wallPlot"), height = "60vh", width = "60vw")
    })

    # Render the tile grid and recalculate the tile counts
    output$wallPlot <- renderPlot({
      calculate_adjusted_dimensions()  # Ensure the dimensions are calculated
      draw_tiles_and_box()
    })

    # Function to recalculate the tiles and tile counts when moving the red box
    recalculate_tiles <- function() {
      full_tiles <- 0
      split_tiles <- 0
      # Recalculate the number of full and split tiles
      y_position <- values$offset_y
      row_counter <- 1
      while (y_position < values$adjusted_wh + 100) {
        x_position <- values$offset_x + ifelse(row_counter %% 2 == 0, input_data$wall_offset * values$scale_factor, 0)
        while (x_position < values$adjusted_ww + 100) {
          # Check if the tile is fully inside the red box
          is_full_tile <- (
            x_position >= 0 && x_position + values$adjusted_tw <= values$adjusted_ww &&
              y_position >= 0 && y_position + values$adjusted_th <= values$adjusted_wh
          )

          # Check if the tile is split (partially inside and outside the red box)
          is_split_tile <- (
            (x_position < 0 && x_position + values$adjusted_tw > 0) ||  # Left edge
              (x_position + values$adjusted_tw > values$adjusted_ww && x_position < values$adjusted_ww) ||  # Right edge
              (y_position < 0 && y_position + values$adjusted_th > 0) ||  # Bottom edge
              (y_position + values$adjusted_th > values$adjusted_wh && y_position < values$adjusted_wh)  # Top edge
          )

          # Update tile counts
          if (is_full_tile) {
            full_tiles <- full_tiles + 1
          } else if (is_split_tile) {
            split_tiles <- split_tiles + 1
          }

          x_position <- x_position + values$adjusted_tw + input_data$wall_grout * values$scale_factor
        }
        y_position <- y_position + values$adjusted_th + input_data$wall_grout * values$scale_factor
        row_counter <- row_counter + 1
      }

      # Update the full and split tile counts
      values$full_tiles <- full_tiles
      values$split_tiles <- split_tiles
    }

    # Function to draw the tiles, red box, and obstacles
    draw_tiles_and_box <- function() {
      # Set up the plot window
      plot.new()
      plot.window(xlim = c(0, values$adjusted_ww), ylim = c(0, values$adjusted_wh), asp = values$adjusted_ww / values$adjusted_wh)

      # Draw the tiles across the wall area with offsets
      y_position <- values$offset_y
      row_counter <- 1
      while (y_position < values$adjusted_wh + 100) {
        x_position <- values$offset_x + ifelse(row_counter %% 2 == 0, input_data$wall_offset * values$scale_factor, 0)
        while (x_position < values$adjusted_ww + 100) {
          # Draw each tile
          polygon(
            c(x_position, x_position, x_position + values$adjusted_tw, x_position + values$adjusted_tw),
            c(y_position, y_position + values$adjusted_th, y_position + values$adjusted_th, y_position),
            col = "lightblue",
            border = "black"
          )

          x_position <- x_position + values$adjusted_tw + input_data$wall_grout * values$scale_factor
        }
        y_position <- y_position + values$adjusted_th + input_data$wall_grout * values$scale_factor
        row_counter <- row_counter + 1
      }

      # Draw the red wall boundary
      rect(0, 0, values$adjusted_ww, values$adjusted_wh, border = "red", lwd = 3)

      # Draw the obstacles based on the provided dimensions (top, left, width, height)
      for (obstacle in input_data$obstacles) {
        # Scale obstacle dimensions based on the wall scale
        obstacle_top <- obstacle$top * values$scale_factor
        obstacle_left <- obstacle$left * values$scale_factor
        obstacle_width <- obstacle$width * values$scale_factor
        obstacle_height <- obstacle$height * values$scale_factor

        # Draw obstacle
        rect(
          obstacle_left,
          values$adjusted_wh - obstacle_top,  # Flip y-axis for plotting
          obstacle_left + obstacle_width,
          values$adjusted_wh - obstacle_top - obstacle_height,
          col = "orange", border = "black", lwd = 2
        )
      }
    }

    # Render tile counts
    output$fullTileCount <- renderText({
      paste("Full Tiles:", values$full_tiles)
    })
    output$splitTileCount <- renderText({
      paste("Split Tiles:", values$split_tiles)
    })
  })
}

# finalPage_server <- function(id, input_data) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
# 
#     # Define reactive values to store the position of the tile grid and tile counts
#     values <- reactiveValues(
#       offset_x = 0,
#       offset_y = 0,
#       full_tiles = 0,
#       split_tiles = 0
#     )
# 
#     # Movement control logic (adjusts grid positions)
#     observeEvent(input$up, { values$offset_y <- values$offset_y + 10 })
#     observeEvent(input$down, { values$offset_y <- values$offset_y - 10 })
#     observeEvent(input$left, { values$offset_x <- values$offset_x - 10 })
#     observeEvent(input$right, { values$offset_x <- values$offset_x + 10 })
#     observeEvent(input$reset, {
#       values$offset_x <- 0
#       values$offset_y <- 0
#       values$full_tiles <- 0
#       values$split_tiles <- 0
#     })
# 
#     # Dynamic rendering of the wall plot based on the passed input_data
#     output$dynamicWallPlot <- renderUI({
#       plotOutput(ns("wallPlot"), height = "60vh", width = "60vw")
#     })
# 
#     output$wallPlot <- renderPlot({
#       # Extract wall dimensions and tile sizes
#       wh <- input_data$wall_height
#       ww <- input_data$wall_width
#       th <- switch(input_data$tile_size, Small = 30, Medium = 50, Large = 70)
#       tw <- switch(input_data$tile_size, Small = 30, Medium = 50, Large = 70)
#       ts <- input_data$wall_grout
#       off <- input_data$wall_offset
#       obstacles <- input_data$obstacles  # Get obstacles
# 
#       # Calculate scaling factor based on available screen size
#       max_height <- 0.9 * session$clientData$output_wallPlot_height
#       max_width <- 0.9 * session$clientData$output_wallPlot_width
#       scale_factor <- min(max_height / wh, max_width / ww, 1)
# 
#       # Adjusted dimensions and tile sizes based on scale
#       adjusted_wh <- wh * scale_factor
#       adjusted_ww <- ww * scale_factor
#       adjusted_th <- th * scale_factor
#       adjusted_tw <- tw * scale_factor
# 
#       # Set up the plot window
#       plot.new()
#       plot.window(xlim = c(0, adjusted_ww), ylim = c(0, adjusted_wh), asp = adjusted_ww / adjusted_wh)
# 
#       # Reset tile counts
#       full_tiles <- 0
#       split_tiles <- 0
# 
#       # Logic for drawing the tiles across the wall area with offsets
#       y_position <- values$offset_y
#       row_counter <- 1
#       while (y_position < adjusted_wh + 100) {
#         x_position <- values$offset_x + ifelse(row_counter %% 2 == 0, off * scale_factor, 0)
#         while (x_position < adjusted_ww + 100) {
#           # Draw each tile
#           polygon(
#             c(x_position, x_position, x_position + adjusted_tw, x_position + adjusted_tw),
#             c(y_position, y_position + adjusted_th, y_position + adjusted_th, y_position),
#             col = "lightblue",
#             border = "black"
#           )
# 
#           # Count full and split tiles
#           if (x_position + adjusted_tw <= adjusted_ww && y_position + adjusted_th <= adjusted_wh) {
#             full_tiles <- full_tiles + 1
#           } else {
#             split_tiles <- split_tiles + 1
#           }
# 
#           x_position <- x_position + adjusted_tw + ts * scale_factor
#         }
#         y_position <- y_position + adjusted_th + ts * scale_factor
#         row_counter <- row_counter + 1
#       }
# 
#       # Update tile counts
#       values$full_tiles <- full_tiles
#       values$split_tiles <- split_tiles
# 
#       # Draw the red wall boundary
#       rect(0, 0, adjusted_ww, adjusted_wh, border = "red", lwd = 3)
# 
#       # Draw the obstacles based on the provided dimensions (top, left, width, height)
#       for (obstacle in obstacles) {
#         # Scale obstacle dimensions based on the wall scale
#         obstacle_top <- obstacle$top * scale_factor
#         obstacle_left <- obstacle$left * scale_factor
#         obstacle_width <- obstacle$width * scale_factor
#         obstacle_height <- obstacle$height * scale_factor
# 
#         # Draw obstacle
#         rect(
#           obstacle_left,
#           adjusted_wh - obstacle_top,  # Flip y-axis for plotting
#           obstacle_left + obstacle_width,
#           adjusted_wh - obstacle_top - obstacle_height,
#           col = "orange", border = "black", lwd = 2
#         )
#       }
#     })
# 
#     # Render tile counts
#     output$fullTileCount <- renderText({ paste("Full Tiles:", values$full_tiles) })
#     output$splitTileCount <- renderText({ paste("Split Tiles:", values$split_tiles) })
#   })
# }