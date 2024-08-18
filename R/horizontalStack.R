horizontalStack_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        # uiOutput(ns("dynamicWallPlot")),  # Dynamically generate plotOutput
        plotOutput(ns("wallPlot"), height = "1000px"),
        verbatimTextOutput(ns("info"))
      )
    ),
    
    absolutePanel(
      top = "calc(50% - 75px)",
      left = "calc(50% - 75px)",
      width = 150,
      height = 150,
      div(
        style = "display: flex; justify-content: center; align-items: center; flex-direction: column; height: 100%;",
        div(
          style = "display: flex; justify-content: center; margin-bottom: 10px;",
          actionButton(
            ns("up"),
            "",
            icon = icon("arrow-up"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          )
        ),
        div(
          style = "display: flex; justify-content: space-between; width: 100%;",
          actionButton(
            ns("left"),
            "",
            icon = icon("arrow-left"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          ),
          actionButton(
            ns("right"),
            "",
            icon = icon("arrow-right"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          )
        ),
        div(
          style = "display: flex; justify-content: center; margin-top: 10px;",
          actionButton(
            ns("down"),
            "",
            icon = icon("arrow-down"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          )
        )
      )
    )
  )
}

horizontalStack_server <- function(id, wall_height, wall_width, tile_height, tile_width, tile_spacing, offset, obstacles) {
  moduleServer(id, function(input, output, session) {

    
    values <- reactiveValues(
      box_x = 0,
      box_y = 0
    )
    
    observeEvent(input$up, {
      values$box_y <- values$box_y + 1  # Move box up
    })
    
    observeEvent(input$down, {
      values$box_y <- values$box_y - 1  # Move box down
    })
    
    observeEvent(input$left, {
      values$box_x <- values$box_x - 1  # Move box left
    })
    
    observeEvent(input$right, {
      values$box_x <- values$box_x + 1  # Move box right
    })
    
    output$wallPlot <- renderPlot({
      
      wh <- wall_height()
      ww <- wall_width()
      th <- tile_height()
      tw <- tile_width()
      ts <- tile_spacing()
      off <- offset()

      box_x <- values$box_x
      box_y <- values$box_y

      plot.new()
      plot.window(xlim = c(0, ww), ylim = c(0, wh))
      
      draw_tile <- function(x, y) {
        polygon(
          c(x, x, x + tw, x + tw),
          c(y, y + th, y + th, y),
          col = "lightblue",
          border = "black"
        )
      }
      
      y_position <- -th  
      row_counter <- 1  
      while (y_position <= wh) {
        x_position <- ifelse(row_counter %% 2 == 0, -tw + off, -tw)
        while (x_position <= ww) {
          draw_tile(x_position, y_position)
          x_position <- x_position + tw + ts
        }
        y_position <- y_position + th + ts
        row_counter <- row_counter + 1
      }
      
      polygon(
        c(
          box_x,
          box_x,
          box_x + ww,
          box_x + ww
        ),
        c(
          box_y,
          box_y + wh,
          box_y + wh,
          box_y
        ),
        border = "red",
        lwd = 3,
        col = NA
      )
      
      for (obstacle in obstacles()) {
        obstacle_center_x <- (obstacle$left + obstacle$right) / 2
        obstacle_center_y <- (obstacle$top + obstacle$bottom) / 2
        
        obstacle_x <- box_x + obstacle_center_x - obstacle$width / 2
        obstacle_y <- box_y + obstacle_center_y - obstacle$height / 2
        
        rect(
          obstacle_x, 
          obstacle_y, 
          obstacle_x + obstacle$width, 
          obstacle_y + obstacle$height, 
          col = "darkgray", 
          border = "black"
        )
      }
      
      # Top (adjust with horizontal position)
      rect(box_x, box_y + wh, box_x + ww, wh + 50, col = rgb(1, 1, 1, 0.99), border = NA)  
      
      # Left
      rect(-50, box_y, box_x, box_y + wh, col = rgb(1, 1, 1, 0.99), border = NA) 
      
      # Right
      rect(box_x + ww, box_y, ww + 50, box_y + wh, col = rgb(1, 1, 1, 0.99), border = NA)   
      
      # Bottom (adjust with horizontal position)
      rect(box_x, -50, box_x + ww, box_y, col = rgb(1, 1, 1, 0.99), border = NA)  
      
      # Top-left corner
      rect(-50, box_y + wh, box_x, wh + 50, col = rgb(1, 1, 1, 0.99), border = NA)
      
      # Top-right corner
      rect(box_x + ww, box_y + wh, ww + 50, wh + 50, col = rgb(1, 1, 1, 0.99), border = NA)
      
      # Bottom-left corner
      rect(-50, -50, box_x, box_y, col = rgb(1, 1, 1, 0.99), border = NA)
      
      # Bottom-right corner
      rect(box_x + ww, -50, ww + 50, box_y, col = rgb(1, 1, 1, 0.99), border = NA)
      
    })
    
  })
}