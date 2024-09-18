library(shiny)
library(gridExtra)
library(grid)
library(gridExtra)
library(grid)
library(gridBase)
library(writexl)
library(openxlsx)

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
          width: 400px;
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
            ns("fast_left"),
            "",
            icon = icon("fast-backward"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("left"),
            "",
            icon = icon("arrow-left"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("up"),
            "",
            icon = icon("arrow-up"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("down"),
            "",
            icon = icon("arrow-down"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("right"),
            "",
            class = "button-shadow",
            icon = icon("arrow-right"),
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("fast_right"),
            "",
            icon = icon("fast-forward"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("reset"),
            "",
            class = "button-shadow",
            icon = icon("refresh"),
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          downloadButton(
            ns("download_plot"),
            "",
            class = "button-shadow",
            icon = icon("download"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          )
        )
      )
    )
  )
}

finalPage_server <- function(id, input_data,switch_ui) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print("final")
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
      th <- switch(input_data$tile_size, "30x30" = 30, "50x50" = 50, "70x70" = 70)
      tw <- switch(input_data$tile_size, "30x30" = 30, "50x50" = 50, "70x70" = 70)

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
      values$offset_y <- values$offset_y + 1
      recalculate_tiles()
    })
    observeEvent(input$down, {
      values$offset_y <- values$offset_y - 1
      recalculate_tiles()
    })
    observeEvent(input$left, {
      values$offset_x <- values$offset_x - 1
      recalculate_tiles()
    })
    observeEvent(input$right, {
      values$offset_x <- values$offset_x + 1
      recalculate_tiles()
    })
    observeEvent(input$fast_right, {
      values$offset_x <- values$offset_x + values$adjusted_th/2
      recalculate_tiles()
    })
    observeEvent(input$fast_left, {
      values$offset_x <- values$offset_x - values$adjusted_th/2
      recalculate_tiles()
    })
    observeEvent(input$reset, {
      values$offset_x <- 0
      values$offset_y <- 0
      values$full_tiles <- 0
      values$split_tiles <- 0
      recalculate_tiles()
      # isolate({
      #   switch_ui("landingPage", NULL)
      # })
    })

    # Render tile plot in UI
    output$dynamicWallPlot <- renderUI({
      plotOutput(ns("wallPlot"), height = "60vh", width = "60vw")
    })

    # Render the tile grid and recalculate the tile counts
    output$wallPlot <- renderPlot({
      calculate_adjusted_dimensions()  # Ensure the dimensions are calculated
      draw_tiles_and_box()
      recalculate_tiles()
    })

    recalculate_tiles <- function() {
      full_tiles <- 0
      split_tiles <- 0

      y_position <- values$offset_y
      row_counter <- 1
      while (y_position < values$adjusted_wh + 100) {
        x_position <- values$offset_x + ifelse(row_counter %% 2 == 0, input_data$wall_offset * values$scale_factor, 0)
        while (x_position < values$adjusted_ww + 100) {

          # Define the boundaries of the current tile
          tile_left <- x_position
          tile_right <- x_position + values$adjusted_tw
          tile_bottom <- y_position
          tile_top <- y_position + values$adjusted_th

          # Define the boundaries of the red box (wall)
          wall_left <- 0
          wall_right <- values$adjusted_ww
          wall_bottom <- 0
          wall_top <- values$adjusted_wh

          # Check if the tile is fully within the red box
          is_full_tile <- (
            tile_left >= wall_left && tile_right <= wall_right &&
              tile_bottom >= wall_bottom && tile_top <= wall_top
          )

          # Check if the tile is split (partially inside and partially outside)
          is_split_tile <- !is_full_tile && (
            (tile_left < wall_right && tile_right > wall_left) &&  # Horizontal overlap
              (tile_bottom < wall_top && tile_top > wall_bottom)     # Vertical overlap
          )

          # Update tile counts based on the conditions
          if (is_full_tile) {
            full_tiles <- full_tiles + 1
          } else if (is_split_tile) {
            split_tiles <- split_tiles + 1
          }

          # Move to the next tile horizontally
          x_position <- x_position + values$adjusted_tw + input_data$wall_grout * values$scale_factor
        }

        # Move to the next row vertically
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

      # Add tile counts to the plot
      # text(x = values$adjusted_ww - 10, y = values$adjusted_wh + 10,
      #      labels = paste("Full Tiles:", values$full_tiles),
      #      adj = 1, col = "black", font = 2, cex = 1.2)
      # text(x = values$adjusted_ww - 10, y = values$adjusted_wh,
      #      labels = paste("Split Tiles:", values$split_tiles),
      #      adj = 1, col = "black", font = 2, cex = 1.2)
    }

    output$download_plot <- downloadHandler(
      filename = function() {
        paste("wall-plot", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file, width = 800, height = 1200)  # Set dimensions for the image to include both tables

        # Create a function to draw the wall plot (red box, tiles, obstacles)
        wall_plot <- function() {
          # Ensure the base R plot is captured within grid layout
          par(new = TRUE, fig = gridFIG())
          draw_tiles_and_box()  # Call your existing function to draw the wall plot
        }

        # Create a table with wall and tile data
        wall_tile_details <- data.frame(
          Parameter = c("Wall Height", "Wall Width", "Tile Height", "Tile Width", "Full Tiles", "Split Tiles"),
          Value = c(
            input_data$wall_height,
            input_data$wall_width,
            switch(input_data$tile_size, "30x30" = 30, "50x50" = 50, "70x70" = 70),  # Tile height
            switch(input_data$tile_size, "30x30" = 30, "50x50" = 50, "70x70" = 70),  # Tile width
            values$full_tiles,
            values$split_tiles
          )
        )

        # Check if there are any obstacles before creating the obstacle table
        if (length(input_data$obstacles) > 0) {
          # Create a table with obstacle details
          obstacle_details <- data.frame(
            Parameter = paste0("Obstacle ", seq_along(input_data$obstacles)),
            Value = sapply(input_data$obstacles, function(ob)
              paste0("Top: ", ob$top, ", Left: ", ob$left, ", Width: ", ob$width, ", Height: ", ob$height))
          )

          # Remove column names (headers) for the tables
          colnames(wall_tile_details) <- NULL
          colnames(obstacle_details) <- NULL

          # Convert the tables to grobs (graphical objects) without headers
          wall_tile_table_grob <- tableGrob(wall_tile_details, rows = NULL)  # Wall and tile table
          obstacle_table_grob <- tableGrob(obstacle_details, rows = NULL)    # Obstacle table
        } else {
          # If there are no obstacles, create a message instead of an obstacle table
          obstacle_table_grob <- textGrob("No obstacles", gp = gpar(fontsize = 12, col = "red"))

          # Remove column names (headers) for the wall tile table
          colnames(wall_tile_details) <- NULL
          wall_tile_table_grob <- tableGrob(wall_tile_details, rows = NULL)  # Wall and tile table
        }

        # Save the image as PNG
        png(file, width = 800, height = 1200)  # Increase height to include plot and both tables

        # Split the image space into three parts: one for the plot, one for the wall/tile table, and one for the obstacle table
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(3, 1)))  # 3 rows, 1 column layout

        # Row 1: Base R plot (tiles, red box, obstacles)
        pushViewport(viewport(layout.pos.row = 1))
        wall_plot()  # Plot the wall tiles, red box, and obstacles
        popViewport()

        # Row 2: Wall and tile table
        pushViewport(viewport(layout.pos.row = 2))
        grid.draw(wall_tile_table_grob)  # Draw the wall and tile table below the plot
        popViewport()

        # Row 3: Obstacle table or "No obstacles" message
        pushViewport(viewport(layout.pos.row = 3))
        grid.draw(obstacle_table_grob)  # Draw the obstacle table or the message
        popViewport()

        dev.off()
      }
    )

    # Render tile counts
    output$fullTileCount <- renderText({
      paste("Full Tiles:", values$full_tiles)
    })
    output$splitTileCount <- renderText({
      paste("Split Tiles:", values$split_tiles)
    })
  })
}
