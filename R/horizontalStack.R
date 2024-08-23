source("R/obstacles.R")

horizontalStack_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(column(
    12,
    div(
      style = "display: flex; justify-content: center; align-items: center; flex-direction: column; height: 100%;",
      
      uiOutput(ns("dynamicWallPlot")),
      
      div(
        style = "margin-top: 20px; text-align: center; display: flex; justify-content: center;",
        div(style = "margin-right: 20px;", textOutput(ns("fullTilesCount"))),
        div(style = "margin-left: 20px;", textOutput(ns(
          "splitTilesCount"
        )))
      ),
      
      div(
        style = "margin-top: 20px; display: flex; justify-content: center; align-items: center;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; width: 320px;",
          actionButton(
            ns("left"),
            "",
            icon = icon("arrow-left"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          ),
          actionButton(ns("up"), "", icon = icon("arrow-up"), style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"),
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
          ),
          actionButton(
            ns("reset"),
            "",
            icon = icon("refresh"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          )
        )
      )
    )
  )))
}

horizontalStack_server <- function(id,
                                   wall_height,
                                   wall_width,
                                   tile_height,
                                   tile_width,
                                   tile_spacing,
                                   offset,
                                   obstacles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    values <- reactiveValues(
      box_x = 0,
      box_y = 0,
      full_tiles = 0,
      split_tiles = 0
    )
    
    observeEvent(input$up, {
      values$box_y <- values$box_y + 1  
    })
    
    observeEvent(input$down, {
      values$box_y <- values$box_y - 1  
    })
    
    observeEvent(input$left, {
      values$box_x <- values$box_x - 1  
    })
    
    observeEvent(input$right, {
      values$box_x <- values$box_x + 1  
    })
    
    observeEvent(input$reset, {
      values$box_x <- 0
      values$box_y <- 0
      values$full_tiles <- 0
      values$split_tiles <- 0
    })
    
    output$dynamicWallPlot <- renderUI({
      plotOutput(
        ns("wallPlot"),
        height = paste0(wall_height(), "px"),
        width = paste0(wall_width(), "px")
      )
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
      
      full_tiles <- 0
      split_tiles <- 0
      
      y_position <- -th
      row_counter <- 1
      while (y_position <= wh + 100) {
        x_position <- ifelse(row_counter %% 2 == 0, -tw + off, -tw)
        while (x_position <= ww + 100) {
          draw_tile(x_position, y_position)
          
          if (x_position >= box_x &&
              x_position + tw <= box_x + ww &&
              y_position >= box_y &&
              y_position + th <= box_y + wh) {
            full_tiles <- full_tiles + 1
          } else if ((x_position + tw > box_x &&
                      x_position < box_x + ww) &&
                     (y_position + th > box_y &&
                      y_position < box_y + wh)) {
            split_tiles <- split_tiles + 1
          }
          
          x_position <- x_position + tw + ts
        }
        y_position <- y_position + th + ts
        row_counter <- row_counter + 1
      }
      
      values$full_tiles <- full_tiles
      values$split_tiles <- split_tiles
      
      polygon(
        c(box_x, box_x, box_x + ww, box_x + ww),
        c(box_y, box_y + wh, box_y + wh, box_y),
        border = "red",
        lwd = 3,
        col = NA
      )
      
      for (obstacle in obstacles()) {
        obstacle_x <- obstacle$left + box_x
        obstacle_y <- obstacle$top + box_y

        top_left_x <- obstacle_x
        top_left_y <- wh - (obstacle_y + obstacle$height)

        rect(
          top_left_x,
          top_left_y,
          top_left_x + obstacle$width,
          top_left_y + obstacle$height,
          col = "darkgray",
          border = "black"
        )
      }
      
      rect(box_x,
           box_y + wh,
           box_x + ww,
           wh + 100,
           col = rgb(1, 1, 1, 1),
           border = NA)

      rect(-100,
           box_y,
           box_x,
           box_y + wh,
           col = rgb(1, 1, 1, 1),
           border = NA)

      rect(box_x + ww,
           box_y,
           ww + 100,
           box_y + wh,
           col = rgb(1, 1, 1, 1),
           border = NA)

      rect(box_x,-100,
           box_x + ww,
           box_y,
           col = rgb(1, 1, 1, 1),
           border = NA)

      rect(-100,
           box_y + wh,
           box_x,
           wh + 100,
           col = rgb(1, 1, 1, 1),
           border = NA)

      rect(
        box_x + ww,
        box_y + wh,
        ww + 100,
        wh + 100,
        col = rgb(1, 1, 1, 1),
        border = NA
      )

      rect(-100,-100,
           box_x,
           box_y,
           col = rgb(1, 1, 1, 1),
           border = NA)

      rect(box_x + ww,-100,
           ww + 100,
           box_y,
           col = rgb(1, 1, 1, 1),
           border = NA)
      
    }, height = wall_height(), width = wall_width())
    
    output$fullTilesCount <- renderText({
      paste("Full tiles:", values$full_tiles)
    })
    
    output$splitTilesCount <- renderText({
      paste("Split tiles:", values$split_tiles)
    })
  })
}
