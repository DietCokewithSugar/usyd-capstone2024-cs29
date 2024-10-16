herringbone_server <- function(id, wall_height, wall_width, herringbone_sv, tile_spacing, tile_color, tile_color_2, obstacles, input_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define ns inside the moduleServer function
    values <- reactiveValues(
      box_x = 0,
      box_y = 0,
      offset_x = 0,
      offset_y = 0
    )
    
    
    
    observeEvent(input$up, {
      values$offset_y <- values$offset_y - 1  # Move box up
    })
    
    observeEvent(input$down, {
      values$offset_y <- values$offset_y + 1  # Move box down
    })
    
    observeEvent(input$left, {
      values$offset_x <- values$offset_x + 1  # Move box left
    })
    
    observeEvent(input$right, {
      values$offset_x <- values$offset_x - 1  # Move box right
    })
    
    observeEvent(input$reset, {
      values$offset_x <- 0
      values$offset_y <- 0
    })
    
    
    # 计算长宽比或根据输入的长/宽进行计算
    observe({
      req(herringbone_sv$input_type)
      # 根据 tile_width 和 tile_ratio 计算 tile_height
      if (herringbone_sv$input_type == "tile_width" && herringbone_sv$tile_ratio > 0) {
        herringbone_sv$tile_height <- (herringbone_sv$tile_width - tile_spacing() * (herringbone_sv$tile_ratio - 1)) / herringbone_sv$tile_ratio
        updateNumericInput(input_session, "tile_height_num", value = herringbone_sv$tile_height)
        cat("tile_height = ",herringbone_sv$tile_height,"tile_width = ",herringbone_sv$tile_width,"tile_ratio = ",herringbone_sv$tile_ratio,"\n")
        
        # 根据 tile_height 和 tile_ratio 计算 tile_width
      } else if (herringbone_sv$input_type == "tile_height" && herringbone_sv$tile_ratio > 0) {
        herringbone_sv$tile_width <- (herringbone_sv$tile_height + tile_spacing()) * herringbone_sv$tile_ratio - tile_spacing()
        updateNumericInput(input_session, "tile_width_num", value = herringbone_sv$tile_width)
        cat("tile_height = ",herringbone_sv$tile_height,"tile_width = ",herringbone_sv$tile_width,"tile_ratio = ",herringbone_sv$tile_ratio,"\n")
        
        # 根据 tile_ratio 计算 tile_width，设置 tile_height 为 20
      } else if (herringbone_sv$input_type == "tile_ratio") {
        herringbone_sv$tile_height <- 20
        herringbone_sv$tile_width <- (herringbone_sv$tile_height + tile_spacing()) * herringbone_sv$tile_ratio - tile_spacing()
        updateNumericInput(input_session, "tile_height_num", value = herringbone_sv$tile_height)
        updateNumericInput(input_session, "tile_width_num", value = herringbone_sv$tile_width)
        cat("tile_height = ",herringbone_sv$tile_height,"tile_width = ",herringbone_sv$tile_width,"tile_ratio = ",herringbone_sv$tile_ratio,"\n")
      }
    })
    
    
    output$dynamicWallPlot <- renderUI({
      plotOutput(ns("wallPlot"), height = paste0(wall_height(), "px"), width = paste0(wall_width(), "px"))
    })
    
    output$wallPlot <- renderPlot({
      wh <- wall_height()
      ww <- wall_width()
      th <- herringbone_sv$tile_height  # Assume height is shorter dimension for horizontal tiles
      tw <- herringbone_sv$tile_width   # Assume width is longer dimension for horizontal tiles
      ts <- tile_spacing()
      tc <- tile_color()
      tc2 <- tile_color_2()
      
      box_x <- values$box_x
      box_y <- values$box_y
      
      plot.new()
      plot.window(xlim = c(0, ww), ylim = c(0, wh))
      
      
      # use updateNumericInput update tile_height and tile_width in UI
      updateSliderInput(session, ns("tile_height"), value = values$tile_height)
      updateNumericInput(session, ns("tile_height_num"), value = values$tile_height)
      updateSliderInput(session, ns("tile_width"), value = values$tile_width)
      updateNumericInput(session, ns("tile_width_num"), value = values$tile_width)
      
      
      
      
      # base point on top left
      draw_horizontal_tile <- function(x, y) {
        polygon(
          c(x, x + tw, x + tw, x),
          c(y - th, y - th, y, y),
          col = tc,
          border = "black"
        )
      }
      
      # base point on top left
      draw_vertical_tile <- function(x, y) {
        polygon(
          
          c(x, x + th, x + th, x),
          c(y - tw, y - tw, y, y),
          col = tc2,
          border = "black"
        )
      }
      
      # base point on top left
      draw_unit <- function(x, y) {
        draw_horizontal_tile(x + th + ts, y)
        draw_vertical_tile(x, y)
      }
      
      
      x_position <- -(tw + th + ts) + values$offset_x
      y_position <- wh + tw + th + ts + values$offset_y
      
      
      # generate base line
      # right bottom limit
      while (x_position <= max(wh, ww) + tw + th + ts || y_position >= -(tw + th + ts)) {
        draw_unit(x_position, y_position)
        
        x_position_bottom_left <- x_position - tw - ts
        y_position_bottom_left <- y_position - tw - ts
        
        x_position_top_right <- x_position + tw + ts
        y_position_top_right <- y_position + tw + ts
        
        # draw to bottom left
        while (x_position_bottom_left >= -(tw + th + ts) || y_position_bottom_left >= -(tw + th + ts)) {
          draw_unit(x_position_bottom_left, y_position_bottom_left)
          x_position_bottom_left <- x_position_bottom_left - tw - ts
          y_position_bottom_left <- y_position_bottom_left - tw - ts
        }
        # draw to top right
        while (x_position_top_right <= max(wh, ww) + tw + th + ts || y_position_top_right <= max(wh, ww) + tw + th + ts) {
          draw_unit(x_position_top_right, y_position_top_right)
          x_position_top_right <- x_position_top_right + tw + ts
          y_position_top_right <- y_position_top_right + tw + ts
        }
        x_position <- x_position + th + ts
        y_position <- y_position - th - ts
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






horizontalStack_server <- function(id, wall_height, wall_width, tile_height, tile_width, tile_spacing, offset, tile_color, texture_option, obstacles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    values <- reactiveValues(
      box_x = 0,
      box_y = 0,
      offset_x = 0,
      offset_y = 0,
      full_tiles = 0,
      split_tiles = 0
    )
    
    observeEvent(input$up, {
      values$offset_y <- values$offset_y - 1  # Move box up
    })
    
    observeEvent(input$down, {
      values$offset_y <- values$offset_y + 1  # Move box down
    })
    
    observeEvent(input$left, {
      values$offset_x <- values$offset_x + 1  # Move box left
    })
    
    observeEvent(input$right, {
      values$offset_x <- values$offset_x - 1  # Move box right
    })
    
    observeEvent(input$reset, {
      values$offset_x <- 0
      values$offset_y <- 0
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
    
    # Load texture images
    brick_texture <- png::readPNG("www/brick.png")
    marble_texture <- png::readPNG("www/marble.png")

    output$wallPlot <- renderPlot({
      wh <- wall_height()
      ww <- wall_width()
      th <- tile_height()
      tw <- tile_width()
      ts <- tile_spacing()
      off <- offset()
      tc <- tile_color()
      
      
      box_x <- values$box_x
      box_y <- values$box_y
      
      plot.new()
      plot.window(xlim = c(0, ww), ylim = c(0, wh))
      
      draw_tile <- function(x, y) {
        if (texture_option() != "None") {
          # Use texture
          texture_img <- switch(texture_option(),
                                "Brick" = brick_texture,
                                "Marble" = marble_texture)
          rasterImage(texture_img, x, y, x + tw, y + th)
        } else {
          # Use solid color
          polygon(
            c(x, x, x + tw, x + tw),
            c(y, y + th, y + th, y),
            col = tc,
            border = "black"
          )
        }
      }
      
      full_tiles <- 0
      split_tiles <- 0
      
      y_position <- -th + values$offset_y
      row_counter <- 1
      while (y_position <= wh + 100) {
        x_position <- ifelse(row_counter %% 2 == 0, -tw + off, -tw) + values$offset_x
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



basketweave_server <- function(id, wall_height, wall_width, tile_height, tile_width, tile_spacing, tile_color, tile_color_2, obstacles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define ns inside the moduleServer function
    
    values <- reactiveValues(
      box_x = 0,
      box_y = 0,
      offset_x = 0,
      offset_y = 0
    )
    
    
    
    observeEvent(input$up, {
      values$offset_y <- values$offset_y - 1  # Move box up
    })
    
    observeEvent(input$down, {
      values$offset_y <- values$offset_y + 1  # Move box down
    })
    
    observeEvent(input$left, {
      values$offset_x <- values$offset_x + 1  # Move box left
    })
    
    observeEvent(input$right, {
      values$offset_x <- values$offset_x - 1  # Move box right
    })
    
    observeEvent(input$reset, {
      values$offset_x <- 0
      values$offset_y <- 0
    })
    
    output$dynamicWallPlot <- renderUI({
      plotOutput(ns("wallPlot"), height = paste0(wall_height(), "px"), width = paste0(wall_width(), "px"))
    })
    
    output$wallPlot <- renderPlot({
      wh <- wall_height()
      ww <- wall_width()
      th <- tile_height()  # Assume height is shorter dimension for horizontal tiles
      ts <- tile_spacing()
      tc <- tile_color()
      tc2 <- tile_color_2()
      
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
          col = tc,
          border = "black"
        )
      }
      
      draw_vertical_tile <- function(x, y) {
        # 左下
        polygon(
          c(x, x + th, x + th, x),
          c(y, y, y + tw, y + tw),
          col = tc2,
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
      
      
      y_position <- - uhw*2 - ts*2 + values$offset_y
      while (y_position <= wh + uhw*2 + ts*2) {
        x_position <- - uhw*2 - ts*2 + values$offset_x
        
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




lattice_server <- function(id, wall_height, wall_width, tile_height, tile_spacing, tile_color, tile_color_2, obstacles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define ns inside the moduleServer function
    
    values <- reactiveValues(
      box_x = 0,
      box_y = 0,
      offset_x = 0,
      offset_y = 0
    )
    
    observeEvent(input$up, {
      values$offset_y <- values$offset_y - 1  # Move box up
    })
    
    observeEvent(input$down, {
      values$offset_y <- values$offset_y + 1  # Move box down
    })
    
    observeEvent(input$left, {
      values$offset_x <- values$offset_x + 1  # Move box left
    })
    
    observeEvent(input$right, {
      values$offset_x <- values$offset_x - 1  # Move box right
    })
    
    observeEvent(input$reset, {
      values$offset_x <- 0
      values$offset_y <- 0
    })
    
    
    output$dynamicWallPlot <- renderUI({
      plotOutput(ns("wallPlot"), height = paste0(wall_height(), "px"), width = paste0(wall_width(), "px"))
    })
    
    output$wallPlot <- renderPlot({
      
      wh <- wall_height()
      ww <- wall_width()
      th <- tile_height()
      ts <- tile_spacing()
      tc <- tile_color()
      tc2 <- tile_color_2()
      
      box_x <- values$box_x
      box_y <- values$box_y
      
      
      plot.new()
      plot.window(xlim = c(0, ww), ylim = c(0, wh))
      
      draw_intersection_tile <- function(x, y) {
        polygon(
          c(x, x + th*(sqrt(2)/2), x, x - th*(sqrt(2)/2)),
          c(y, y + th*(sqrt(2)/2), y + th*sqrt(2), y + th*(sqrt(2)/2)),
          col = tc2,
          border = "black"
        )
      }
      
      draw_right_bar_tile <- function(x, y) {
        polygon(
          c(x, x + ts*(sqrt(2)/2), x + (ts-th) * (sqrt(2)/2), x - th*(sqrt(2)/2)),
          c(y, y + ts*(sqrt(2)/2), y + (ts+th) * (sqrt(2)/2), y + th*(sqrt(2)/2)),
          col = tc,
          border = "black"
        )
      }
      
      draw_left_bar_tile <- function(x, y) {
        polygon(
          c(x, x + th*(sqrt(2)/2), x + (th-ts) * (sqrt(2)/2), x - ts*(sqrt(2)/2)),
          c(y, y + th*(sqrt(2)/2), y + (ts+th) * (sqrt(2)/2), y + ts*(sqrt(2)/2)),
          col = tc,
          border = "black"
        )
      }
      
      draw_gap_tile <- function(x, y) {
        polygon(
          c(x, x + ts*(sqrt(2)/2), x, x - ts*(sqrt(2)/2)),
          c(y, y + ts*(sqrt(2)/2), y + ts*sqrt(2), y + ts*(sqrt(2)/2)),
          col = "white",
          border = "black"
        )
      }
      
      # base point on bottom
      draw_unit <- function(x, y) {
        draw_intersection_tile(x, y)
        draw_right_bar_tile(x + th*(sqrt(2)/2), y + th*(sqrt(2)/2))
        draw_left_bar_tile(x - th*(sqrt(2)/2), y + th*(sqrt(2)/2))
        draw_gap_tile(x, y + th*sqrt(2))
      }
      
      # # draw a line
      # abline(v = 100, col = "red", lwd = 2) # 在x=0处画一根纵线，红色，线宽1
      #
      # # draw a line
      # abline(h = 100, col = "blue", lwd = 2) # 在y=0处画一根横线，蓝色，线宽1
      
      y_position <- -(ts + th) + values$offset_y
      row_counter <- 1
      while (y_position <= wh + ts + th) {
        x_position <- ifelse(row_counter %% 2 == 0, -th + (ts+th)*(sqrt(2)/2), -th)  + values$offset_x
        while (x_position <= ww + ts + th) {
          draw_unit(x_position, y_position)
          x_position <- x_position + (ts+th)*sqrt(2)
        }
        y_position <- y_position + (ts+th)*(sqrt(2)/2)
        row_counter <- row_counter + 1
      }
      
      # draw_unit(100, 100)
      
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