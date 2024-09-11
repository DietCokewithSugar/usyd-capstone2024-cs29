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
      tw <- tile_width()   # Assume width is longer dimension for horizontal tiles
      ts <- tile_spacing()
      off <- offset()
      
      #尝试
      th<-th+ts*3
      tw<-tw+ts*3
      
      box_x <- values$box_x
      box_y <- values$box_y
      
      plot.new()
      plot.window(xlim = c(0, ww), ylim = c(0, wh))
      
      # base point on top left
      draw_horizontal_tile <- function(x, y) {
        polygon(
          c(x, x + tw*0.2, x + tw*0.2, x),
          c(y - th, y - th, y, y),
          col = "beige",
          border = "black"
        )
        
        polygon(
          c(x + tw*0.2, x + tw*0.4, x + tw*0.4, x + tw*0.2),
          c(y - th, y - th, y, y),
          col = "beige",
          border = "black"
        )
        
        polygon(
          c(x + tw*0.4, x + tw*0.6, x + tw*0.6, x+ tw*0.4),
          c(y - th, y - th, y, y),
          col = "beige",
          border = "black"
        )
        
        polygon(
          c(x + tw*0.6, x + tw*0.8, x + tw*0.8, x+ tw*0.6),
          c(y - th, y - th, y, y),
          col = "beige",
          border = "black"
        )
        
        polygon(
          c(x + tw*0.8, x + tw, x + tw, x+ tw*0.8),
          c(y - th, y - th, y, y),
          col = "beige",
          border = "black"
        )
        
        

      }
      
      #没用
      # base point on top left
      draw_vertical_tile <- function(x, y) {
        polygon(
          c(x,x+th, x+th, x),
          c(y,y,y+0.2*tw,y+0.2*tw),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x,x+th, x+th, x),
          c(y+0.2*tw,y+0.2*tw,y+0.4*tw,y+0.4*tw),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x,x+th, x+th, x),
          c(y+0.4*tw,y+0.4*tw,y+0.6*tw,y+0.6*tw),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x,x+th, x+th, x),
          c(y+0.6*tw,y+0.6*tw,y+0.8*tw,y+0.8*tw),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x,x+th, x+th, x),
          c(y+0.8*tw,y+0.8*tw,y+tw,y+tw),
          col = "brown",
          border = "black"
        )
        
      }
      
      ts1<- ts
      ts2<- ts*2
      ts3<- ts*3
      
      #旧构造方法
      draw_tile <- function(x, y) {
        #右下
        polygon(
          c(x + ts1, x + ts1, x + tw * (1/6)+ts1, x + tw* (1/6)+ts1),
          c(y-ts1, y - th*0.5-ts1, y - th*0.5-ts1, y-ts1),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x + tw * (1/6)+ ts2, x + tw * (1/6)+ ts2, x + tw * (1/3)+ ts2, x + tw* (1/3)+ ts2),
          c(y-ts1, y - th*0.5-ts1, y - th*0.5-ts1, y-ts1),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x + tw * (1/3)+ts3, x + tw * (1/3)+ts3, x + tw * (1/2)+ts3, x + tw* (1/2)+ts3),
          c(y-ts1, y - th*0.5-ts1, y - th*0.5-ts1, y-ts1),
          col = "brown",
          border = "black"
        )
        #右上
        polygon(
          c(x+ ts1, x+ tw * (1/2)+ ts1, x + tw * (1/2)+ ts1, x+ ts1),
          c(y+ ts1, y+ ts1 , y + th*(1/6)+ ts1, y+ th*(1/6)+ ts1),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x+ ts1, x+ tw * (1/2)+ ts1, x + tw * (1/2)+ ts1, x+ ts1),
          c(y+ th*(1/6)+ ts2, y+ th*(1/6)+ ts2 , y + th*(1/3)+ ts2, y+ th*(1/3)+ ts2),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x+ ts1, x+ tw * (1/2)+ ts1, x + tw * (1/2)+ ts1, x+ ts1),
          c(y+ th*(1/3)+ ts3, y+ th*(1/3)+ ts3, y + th*(1/2)+ ts3, y+ th*(1/2)+ ts3),
          col = "brown",
          border = "black"
        )
        #左上
        polygon(
          c(x-ts1, x-ts1, x - tw * (1/6)-ts1, x - tw* (1/6)-ts1),
          c(y+ ts1, y + th*0.5+ ts1, y + th*0.5+ ts1, y+ ts1),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x - tw * (1/6)-ts2, x - tw * (1/6)-ts2, x - tw * (1/3)-ts2, x - tw* (1/3)-ts2),
          c(y+ ts1, y + th*0.5+ ts1, y + th*0.5+ ts1, y+ ts1),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x - tw * (1/3)-ts3, x - tw * (1/3)-ts3, x - tw * (1/2)-ts3, x - tw* (1/2)-ts3),
          c(y+ ts1, y + th*0.5+ ts1, y + th*0.5+ ts1, y+ ts1),
          col = "brown",
          border = "black"
        )
        #左下
        polygon(
          c(x-ts1, x- tw * (1/2)-ts1, x - tw * (1/2)-ts1, x-ts1),
          c(y-ts1, y-ts1 , y - th*(1/6)-ts1, y- th*(1/6)-ts1),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x-ts1, x- tw * (1/2)-ts1, x - tw * (1/2)-ts1, x-ts1),
          c(y- th*(1/6)-ts2, y-th*(1/6)-ts2 , y - th*(1/3)-ts2, y- th*(1/3)-ts2),
          col = "brown",
          border = "black"
        )
        
        polygon(
          c(x-ts1, x- tw * (1/2)-ts1, x - tw * (1/2)-ts1, x-ts1),
          c(y- th*(1/3)-ts3, y- th*(1/3)-ts3 , y-th*(1/2)-ts3, y- th*(1/2)-ts3),
          col = "brown",
          border = "black"
        )
        
      }
      
      #新构造方法
      draw_tile2<- function(x, y) {
        #右下
        polygon(
          c(x + ts1, x + ts1, x + tw * (1/6)+ts1, x + tw* (1/6)+ts1),
          c(y-ts1, y - th*0.5-ts1, y - th*0.5-ts1, y-ts1),
          col = "brown",
          border = "black"
        )
      }
      
      y_position <- -th
      row_counter <- 1
      ts_y<-ts+ts1
      
      while (y_position <= wh + 100) {
        x_position <- ifelse(row_counter %% 2 == 0, -tw + off, -tw)
        
        ts_x<-ts+ts1
        ts_y<-ts_y+ts1
        
        while (x_position <= ww + 100) {
          draw_tile(x_position, y_position)
          x_position <- x_position + tw + ts_x
          
          ts_x<-ts_x+ts1
        }
        y_position <- y_position + th + ts_y
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
