basketweave_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      column(
        12,
        div(
          style = "display: flex; justify-content: center; align-items: center; height: 100%;",  # Center the content
          uiOutput(ns("dynamicWallPlot"))  # Dynamically generate plotOutput
        )
      )
    ),
    absolutePanel(
      bottom = "20px",  # Position the panel near the bottom
      left = "50%",
      width = "auto",
      height = "auto",
      style = "transform: translateX(-50%);",  # Center the panel horizontally
      div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 250px;",  # Adjust width as needed
        actionButton(
          ns("left"),
          "",
          icon = icon("arrow-left"),
          style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
        ),
        actionButton(
          ns("up"),
          "",
          icon = icon("arrow-up"),
          style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
        ),
        actionButton(
          ns("down"),
          "",
          icon = icon("arrow-down"),
          style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
        ),
        actionButton(
          ns("right"),
          "",
          icon = icon("arrow-right"),
          style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
        )
      )
    )
  )
}

basketweave_server <- function(id, wall_height, wall_width, tile_height, tile_width, tile_spacing, offset, obstacles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define ns inside the moduleServer function
    
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
    
    output$dynamicWallPlot <- renderUI({
      plotOutput(ns("wallPlot"), height = paste0(wall_height(), "px"), width = paste0(wall_width(), "px"))
    })
    
    output$wallPlot <- renderPlot({
      wh <- wall_height()
      ww <- wall_width()
      th <- tile_height()  # Assume height is shorter dimension for horizontal tiles
      ts <- tile_spacing()
      off <- offset()
      
      #尝试
      tw <- th*3 + ts*2
      
      box_x <- values$box_x
      box_y <- values$box_y
      
      plot.new()
      plot.window(xlim = c(0, ww), ylim = c(0, wh))

      base_x <- 100
      base_y <- 100

      # base point on top left
      draw_horizontal_tile <- function(x, y) {
        # 左下
        polygon(
          c(x, x + tw, x + tw, x),
          c(y, y, y + th, y + th),
          col = "lightblue",
          border = "black"
        )
      }

      draw_vertical_tile <- function(x, y) {
        # 左下
        polygon(
          c(x, x + th, x + th, x),
          c(y, y, y + tw, y + tw),
          col = "beige",
          border = "black"
        )
      }

      tg <-  th + ts

      draw_unit <- function(x, y) {
        # bottom-left
        draw_horizontal_tile(x,y)
        draw_horizontal_tile(x,y + tg)
        draw_horizontal_tile(x,y + tg*2)
        # bottom-right
        draw_vertical_tile(x + tw + ts,y)
        draw_vertical_tile(x + tw + ts + tg,y)
        draw_vertical_tile(x + tw + ts + tg*2,y)
        # top-left
        draw_vertical_tile(x,y + tw + ts)
        draw_vertical_tile(x + tg,y + tw + ts)
        draw_vertical_tile(x + tg*2,y + tw + ts)
        # top-right
        draw_horizontal_tile(x + tw + ts,y + tw + ts)
        draw_horizontal_tile(x + tw + ts,y + tw + ts + tg)
        draw_horizontal_tile(x + tw + ts,y + tw + ts + tg*2)
      }

      uhw <- tw*2 + ts


      y_position <- - uhw*2 - ts*2
      while (y_position <= wh + uhw*2 + ts*2) {
        x_position <- - uhw*2 - ts*2

        while (x_position <= ww + uhw*2 + ts*2) {
          draw_unit(x_position, y_position)
          x_position <- x_position + uhw + ts
        }

        y_position <- y_position + uhw + ts
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
      rect(box_x, box_y + wh, box_x + ww, wh + 100, col = rgb(1, 1, 1, 1), border = NA)
      
      # Left
      rect(-100, box_y, box_x, box_y + wh, col = rgb(1, 1, 1, 1), border = NA)
      
      # Right
      rect(box_x + ww, box_y, ww + 100, box_y + wh, col = rgb(1, 1, 1, 1), border = NA)
      
      # Bottom (adjust with horizontal position)
      rect(box_x, -100, box_x + ww, box_y, col = rgb(1, 1, 1, 1), border = NA)
      
      # Top-left corner
      rect(-100, box_y + wh, box_x, wh + 100, col = rgb(1, 1, 1, 1), border = NA)
      
      # Top-right corner
      rect(box_x + ww, box_y + wh, ww + 100, wh + 100, col = rgb(1, 1, 1, 1), border = NA)
      
      # Bottom-left corner
      rect(-100, -100, box_x, box_y, col = rgb(1, 1, 1, 1), border = NA)
      
      # Bottom-right corner
      rect(box_x + ww, -100, ww + 100, box_y, col = rgb(1, 1, 1, 1), border = NA)
      
    }, height = wall_height(), width = wall_width())
  })
}
