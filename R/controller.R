source("R/pattern.R")
source("R/designUI.R")

library(shiny)
library(gridExtra)
library(grid)
library(gridBase)
library(writexl)
library(openxlsx)
library(shinyWidgets)
library(colourpicker)

userInput_server <- function(id, herringbone_sv) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$wall_height_num, {
      numeric_error <- validate_numeric_input(input$wall_height_num, min_value = 100, max_value = 900)
      output$numeric_error_wall_height <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
          return("")
        }
      })
    })
    
    observeEvent(input$wall_width_num, {
      numeric_error <- validate_numeric_input(input$wall_width_num, min_value = 100, max_value = 1200)
      output$numeric_error_wall_width <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$tile_height_num, {
      numeric_error <- validate_numeric_input(input$tile_height_num, min_value = 10, max_value = 1000)
      output$numeric_error_tile_height <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$tile_width_num, {
      numeric_error <- validate_numeric_input(input$tile_width_num, min_value = 10, max_value = 150)
      output$numeric_error_tile_width <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$tile_spacing_num, {
      numeric_error <- validate_numeric_input(input$tile_spacing_num, min_value = 0, max_value = 100)
      output$numeric_error_tile_spacing <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$tile_ratio_num, {
      numeric_error <- validate_numeric_input(input$tile_ratio_num, min_value = 2, max_value = 20)
      output$numeric_error_tile_ratio <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$offset_num, {
      numeric_error <- validate_numeric_input(input$offset_num, min_value = 0, max_value = 500)
      output$numeric_error_offset <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    wall_height <- debounce(reactive({
      if (is.null(validate_numeric_input(input$wall_height_num, min_value = 100, max_value = 900))) {
        input$wall_height_num
      } else {
        300
      }
    }), millis = 500)
    
    wall_width <- debounce(reactive({
      if (is.null(validate_numeric_input(input$wall_width_num, min_value = 100, max_value = 1200))) {
        input$wall_width_num
      } else {
        500
      }
    }), millis = 500)
    
    tile_height <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_height_num, min_value = 10, max_value = 1000))) {
        input$tile_height_num
      } else {
        50
      }
    }), millis = 500)
    
    tile_width <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_width_num, min_value = 10, max_value = 150))) {
        input$tile_width_num
      } else {
        50
      }
    }), millis = 500)
    
    tile_spacing <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_spacing_num, min_value = 0, max_value = 100))) {
        input$tile_spacing_num
      } else {
        5
      }
    }), millis = 500)
    
    tile_ratio <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_ratio_num, min_value = 2, max_value = 20))) {
        input$tile_ratio_num
      } else {
        2
      }
    }), millis = 500)
    
    offset <- debounce(reactive({
      if (is.null(validate_numeric_input(input$offset_num, min_value = 0, max_value = 500))) {
        input$offset_num
      } else {
        25
      }
    }), millis = 500)
    
    tile_color <- debounce(reactive(
      input$tile_color
    ), millis = 500)
    
    tile_color_2 <- debounce(reactive(
      input$tile_color_2
    ), millis = 500)
    
    pattern_dropdown <- reactive({
      input$pattern_dropdown
    })
    
    texture_option <- debounce(reactive(
      input$texture_option
    ), millis = 500)
    
    # 2 way sync
    updating <- reactiveValues(flag = TRUE)
    
    observe({
      
      if(input$pattern_dropdown == "Herringbone") {
        
        observeEvent(input$tile_height_num, {
          if(input$pattern_dropdown == "Herringbone") {
            if(updating$flag){
              updating$flag <- FALSE
              cat("change tile_height \n")
              herringbone_sv$tile_height <- input$tile_height_num
              herringbone_sv$input_type <- "tile_height"
              later(function() {
                updating$flag <- TRUE
              }, 0.5)
            }
          }
        })
        
        observeEvent(input$tile_width_num, {
          if(input$pattern_dropdown == "Herringbone") {
            if(updating$flag){
              updating$flag <- FALSE
              cat("change tile_width \n")
              herringbone_sv$tile_width <- input$tile_width_num
              herringbone_sv$input_type <- "tile_width"
              later(function() {
                updating$flag <- TRUE
              }, 0.5)
            }
          }
        })
        
        observeEvent(input$tile_ratio_num, {
          if(input$pattern_dropdown == "Herringbone") {
            if(updating$flag){
              updating$flag <- FALSE
              cat("change tile_ratio \n")
              herringbone_sv$tile_ratio <- input$tile_ratio_num
              herringbone_sv$input_type <- "tile_ratio"
              later(function() {
                updating$flag <- TRUE
              }, 0.5)
            }
          }
        })
        
        observeEvent(input$tile_spacing_num, {
          if(input$pattern_dropdown == "Herringbone") {
            if(updating$flag){
              updating$flag <- FALSE
              cat("change tile_spacing \n")
              later(function() {
                updating$flag <- TRUE
              }, 0.5)
            }
          }
        })
        
      }
    })
    
    return(
      list(
        wall_height = wall_height,
        wall_width = wall_width,
        tile_height = tile_height,
        tile_width = tile_width,
        tile_spacing = tile_spacing,
        offset = offset,
        tile_ratio = tile_ratio,
        tile_color = tile_color,
        tile_color_2 = tile_color_2,
        pattern_dropdown = pattern_dropdown,
        texture_option = texture_option,
        session = session
      )
    )
  })
  
}





landingPage_server <- function(id, selected_values, switch_ui) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Pre-fill inputs if selected_values are provided (from the second page)
    if (!is.null(selected_values)) {
      # If selected_values includes wall-related fields (from secondPage), do nothing
      if (!is.null(selected_values$wall_height)) {
        # wall values already provided, no need to update class, family, or layout
      } else {
        # This is from the landing page, update class, family, and layout
        updateSelectInput(session, "class_option", selected = selected_values$class_option)
        updateSelectInput(session, "family_option", selected = selected_values$family_option)
        updateSelectInput(session, "layout_option", selected = selected_values$layout_option)
      }
    }
    
    # Define class-to-family mapping
    family_choices <- list(
      "One tile patterns" = c("Family A", "Family B", "Family C"),
      "Two tile patterns" = c("Family D", "Family E", "Family F"),
      "Three tile patterns" = c("Family G", "Family H", "Family I"),
      "Four (and more) tile patterns" = c("Family J", "Family K", "Family L")
    )
    
    layout_choices <- list(
      "Family A" = c("Stack", "Herringbone"),
      "Family B" = c("Basketweave", "Lattice"),
      "Family C" = c("Hexagon", "Diamond"),
      "Family D" = c("Square", "Rectangle"),
      "Family E" = c("Linear", "Diagonal"),
      "Family F" = c("Grid", "Chevron"),
      "Family G" = c("Flemish", "Windmill"),
      "Family H" = c("Versailles", "Cobblestone"),
      "Family I" = c("Mosaic", "Basketweave"),
      "Family J" = c("Herringbone", "Chevron"),
      "Family K" = c("Basketweave", "Stack"),
      "Family L" = c("Lattice", "Hexagon")
    )
    
    # Reset the inputs to their default values
    observeEvent(input$reset, {
      updateSelectInput(session, "class_option", selected = "One tile patterns")
      updateSelectInput(session, "family_option", selected = family_choices[["One tile patterns"]][1])
      updateSelectInput(session, "layout_option", selected = layout_choices[["Family A"]][1])
    })
    
    # Update family choices when class_option changes
    observeEvent(input$class_option, {
      selected_class <- input$class_option
      families <- family_choices[[selected_class]]
      
      # Update the family selectInput based on the selected class
      updateSelectInput(session, "family_option",
                        choices = families,
                        selected = families[1])
      
      # Update layout for the first family in the updated family list
      layouts <- layout_choices[[families[1]]]
      updateSelectInput(session, "layout_option",
                        choices = layouts,
                        selected = layouts[1])
    })
    
    # Update layout choices when family_option changes
    observeEvent(input$family_option, {
      selected_family <- input$family_option
      layouts <- layout_choices[[selected_family]]
      
      # Update the layout selectInput based on the selected family
      updateSelectInput(session, "layout_option",
                        choices = layouts,
                        selected = layouts[1])
    })
    
    # Handle the NEXT button click to move to final page
    observeEvent(input$next_button, {
      # Collect the values from the landing page
      landing_page_values <- list(
        class_option = input$class_option,
        family_option = input$family_option,
        layout_option = input$layout_option
      )
      
      # Combine the landingPage values with the secondPage values
      final_values <- if (!is.null(selected_values)) {
        c(landing_page_values, selected_values)  # Merge both lists
      } else {
        landing_page_values  # Only landing page values
      }
      
      # Switch to final page with all values
      switch_ui("finalPage", final_values)
    })
  })
}






validate_numeric_input <- function(input_value, min_value = -Inf, max_value = Inf, positive_only = FALSE) {
  if (is.na(as.numeric(input_value))) {
    return("Error: Input must be a numeric value.")
  }
  if (as.numeric(input_value) < min_value || as.numeric(input_value) > max_value) {
    return(paste0("Error: Input must be between ", min_value, " and ", max_value, "."))
  }
  if (positive_only && as.numeric(input_value) <= 0) {
    return("Error: Input must be a positive number.")
  }
  return(NULL)
}

validate_text_input <- function(input_value, min_length = 1, max_length = Inf) {
  input_value <- trimws(input_value)
  if (nchar(input_value) < min_length) {
    return(paste0("Error: Input must be at least ", min_length, " characters long."))
  }
  if (nchar(input_value) > max_length) {
    return(paste0("Error: Input must be no more than ", max_length, " characters long."))
  }
  return(NULL)
}








 obstaclesServer <- function(id, userInput) {
  moduleServer(id, function(input, output, session) {
     ns <- session$ns
     
     observeEvent(userInput$wall_height(), {
       wall_height <- userInput$wall_height()
     })
     
     observeEvent(userInput$wall_width(), {
       wall_width <- userInput$wall_width()
     })
     
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





finalPage_server <- function(id, input_data, switch_ui) {
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
      th <- switch(
        input_data$tile_size,
        "50x50" = 50,
        "75x75" = 75,
        "100x100" = 100,
        "150x150" = 150,
        "200x200" = 200,
        "300x300" = 300,
        "400x400" = 400,
        "450x450" = 450,
        "600x600" = 600,
        "900x900" = 900
      )
      
      tw <- switch(
        input_data$tile_size,
        "50x50" = 50,
        "75x75" = 75,
        "100x100" = 100,
        "150x150" = 150,
        "200x200" = 200,
        "300x300" = 300,
        "400x400" = 400,
        "450x450" = 450,
        "600x600" = 600,
        "900x900" = 900
      )
      
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
    observeEvent(input$fast_up, {
      values$offset_y <- values$offset_y + values$adjusted_th / 2
      recalculate_tiles()
    })
    observeEvent(input$fast_down, {
      values$offset_y <- values$offset_y - values$adjusted_th / 2
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
      values$offset_x <- values$offset_x + values$adjusted_th / 2
      recalculate_tiles()
    })
    observeEvent(input$fast_left, {
      values$offset_x <- values$offset_x - values$adjusted_th / 2
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
      recalculate_tiles()
    })
    
    recalculate_tiles <- function() {
      full_tiles <- 0
      split_tiles <- 0
      
      # Ensure wall_offset, wall_grout, and other input values are numeric
      wall_offset <- as.numeric(input_data$wall_offset)
      wall_grout <- as.numeric(input_data$wall_grout)
      adjusted_th <- as.numeric(values$adjusted_th)
      adjusted_tw <- as.numeric(values$adjusted_tw)
      adjusted_ww <- as.numeric(values$adjusted_ww)
      adjusted_wh <- as.numeric(values$adjusted_wh)
      offset_x <- as.numeric(values$offset_x)
      offset_y <- as.numeric(values$offset_y)
      scale_factor <- as.numeric(values$scale_factor)
      
      # Ensure none of the values are NA or NULL
      if (is.na(wall_offset) ||
          is.na(wall_grout) || is.na(adjusted_th) ||
          is.na(adjusted_tw) ||
          is.na(adjusted_ww) || is.na(adjusted_wh) ||
          is.na(offset_x) ||
          is.na(offset_y) || is.na(scale_factor)) {
        warning("One or more input values are missing or invalid.")
        return(NULL)  # Exit the function if there are invalid values
      }
      
      y_position <- offset_y
      row_counter <- 1
      
      # Loop through vertical positions
      while (y_position < adjusted_wh + 100) {
        # Adjust horizontal offset for every second row
        x_position <- offset_x + ifelse(row_counter %% 2 == 0, wall_offset * scale_factor, 0)
        
        # Loop through horizontal positions
        while (x_position < adjusted_ww + 100) {
          # Define the boundaries of the current tile
          tile_left <- x_position
          tile_right <- x_position + adjusted_tw
          tile_bottom <- y_position
          tile_top <- y_position + adjusted_th
          
          # Define the boundaries of the wall
          wall_left <- 0
          wall_right <- adjusted_ww
          wall_bottom <- 0
          wall_top <- adjusted_wh
          
          # Check if the tile is fully within the wall
          is_full_tile <- (
            tile_left >= wall_left && tile_right <= wall_right &&
              tile_bottom >= wall_bottom && tile_top <= wall_top
          )
          
          # Check if the tile is split (partially inside and partially outside)
          is_split_tile <- !is_full_tile && (
            (tile_left < wall_right &&
               tile_right > wall_left) &&  # Horizontal overlap
              (tile_bottom < wall_top &&
                 tile_top > wall_bottom)     # Vertical overlap
          )
          
          # Update tile counts
          if (is_full_tile) {
            full_tiles <- full_tiles + 1
          } else if (is_split_tile) {
            split_tiles <- split_tiles + 1
          }
          
          # Move to the next tile horizontally
          x_position <- x_position + adjusted_tw + wall_grout * scale_factor
        }
        
        # Move to the next row vertically
        y_position <- y_position + adjusted_th + wall_grout * scale_factor
        row_counter <- row_counter + 1
      }
      
      # Update the reactive values for full and split tiles
      values$full_tiles <- full_tiles
      values$split_tiles <- split_tiles
    }
    
    # Function to draw the tiles, red box, and obstacles
    
    values$tile_color <- "#ADD8E6"
    
    observeEvent(input$change_color, {
      values$tile_color <- input$tile_color
      draw_tiles_and_box()  # Redraw the tiles with the new color
    })
    
    
    draw_tiles_and_box <- function() {
      # Ensure input_data values are numeric
      wall_offset <- as.numeric(input_data$wall_offset)
      wall_grout <- as.numeric(input_data$wall_grout)
      
      # Set up the plot window
      plot.new()
      plot.window(
        xlim = c(0, values$adjusted_ww),
        ylim = c(0, values$adjusted_wh),
        asp = values$adjusted_ww / values$adjusted_wh
      )
      
      # Draw the tiles across the wall area with offsets
      y_position <- values$offset_y
      row_counter <- 1
      while (y_position < values$adjusted_wh + 100) {
        x_position <- values$offset_x + ifelse(row_counter %% 2 == 0,
                                               wall_offset * values$scale_factor,
                                               0)
        while (x_position < values$adjusted_ww + 100) {
          # Draw each tile
          polygon(
            c(
              x_position,
              x_position,
              x_position + values$adjusted_tw,
              x_position + values$adjusted_tw
            ),
            c(
              y_position,
              y_position + values$adjusted_th,
              y_position + values$adjusted_th,
              y_position
            ),
            col = values$tile_color,
            border = "black"
          )
          
          # Move to the next tile horizontally
          x_position <- x_position + values$adjusted_tw + wall_grout * values$scale_factor
        }
        # Move to the next row vertically
        y_position <- y_position + values$adjusted_th + wall_grout * values$scale_factor
        row_counter <- row_counter + 1
      }
      
      # Draw the red wall boundary
      rect(
        0,
        0,
        values$adjusted_ww,
        values$adjusted_wh,
        border = "red",
        lwd = 3
      )
      
      # Check if obstacles exist and are non-empty
      if (!is.null(input_data$obstacles) &&
          length(input_data$obstacles) > 0) {
        # Draw each obstacle based on its provided dimensions (top, left, width, height)
        for (obstacle in input_data$obstacles) {
          # Ensure obstacle dimensions are numeric
          obstacle_top <- as.numeric(obstacle$top) * values$scale_factor
          obstacle_left <- as.numeric(obstacle$left) * values$scale_factor
          obstacle_width <- as.numeric(obstacle$width) * values$scale_factor
          obstacle_height <- as.numeric(obstacle$height) * values$scale_factor
          
          # Draw the obstacle with correct dimensions and flipping on the y-axis
          rect(
            obstacle_left,
            values$adjusted_wh - obstacle_top,
            # Flip y-axis for plotting
            obstacle_left + obstacle_width,
            values$adjusted_wh - obstacle_top - obstacle_height,
            col = "orange",
            border = "black",
            lwd = 2
          )
        }
      }
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
          Parameter = c(
            "Wall Height",
            "Wall Width",
            "Tile Height",
            "Tile Width",
            "Full Tiles",
            "Split Tiles"
          ),
          Value = c(
            input_data$wall_height,
            input_data$wall_width,
            switch(
              input_data$tile_size,
              "30x30" = 30,
              "50x50" = 50,
              "70x70" = 70
            ),
            # Tile height
            switch(
              input_data$tile_size,
              "30x30" = 30,
              "50x50" = 50,
              "70x70" = 70
            ),
            # Tile width
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
              paste0(
                "Top: ",
                ob$top,
                ", Left: ",
                ob$left,
                ", Width: ",
                ob$width,
                ", Height: ",
                ob$height
              ))
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

