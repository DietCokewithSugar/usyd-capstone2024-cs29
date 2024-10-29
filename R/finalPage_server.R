finalPage_server <- function(id, input_data, switch_ui) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- reactiveValues(
      offset_x = 0,
      offset_y = 0,
      full_tiles = 0,
      split_tiles = 0,
      tile_color = "#ADD8E6"
    )

    # Get adjusted dimensions and store in values
    adjusted_dims <- calculate_adjusted_dimensions(input_data, session)
    values$adjusted_wh <- adjusted_dims$adjusted_wh
    values$adjusted_ww <- adjusted_dims$adjusted_ww
    values$adjusted_th <- adjusted_dims$adjusted_th
    values$adjusted_tw <- adjusted_dims$adjusted_tw
    values$scale_factor <- adjusted_dims$scale_factor

    # Set up movement controls
    setup_movement_controls(input, values, function() {
      recalculate_tiles(input_data, values)
    })

    # Observe tile color change
    observeEvent(input$change_color, {
      values$tile_color <- input$tile_color
      draw_tiles_and_box(input_data, values)  # Redraw the tiles with the new color
    })

    # Render tile plot in UI
    output$dynamicWallPlot <- renderUI({
      plotOutput(ns("wallPlot"), height = "60vh", width = "60vw")
    })

    # Render the tile grid and recalculate the tile counts
    output$wallPlot <- renderPlot({
      draw_tiles_and_box(input_data, values)
      recalculate_tiles(input_data, values)
    })

    output$download_plot <- downloadHandler(
      filename = function() {
        paste("wall-plot", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        # 确保正确获取 tile_size_value
        tile_size_value <- switch(
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
          "900x900" = 900,
          50  # 默认值
        )

        # 使用 isolate() 获取 reactiveValues 值
        full_tiles <- isolate(values$full_tiles)
        split_tiles <- isolate(values$split_tiles)
        adjusted_ww <- isolate(values$adjusted_ww)
        adjusted_wh <- isolate(values$adjusted_wh)
        adjusted_tw <- isolate(values$adjusted_tw)
        adjusted_th <- isolate(values$adjusted_th)
        scale_factor <- isolate(values$scale_factor)
        offset_x <- isolate(values$offset_x)
        offset_y <- isolate(values$offset_y)
        tile_color <- isolate(values$tile_color)
        # 从 input_data 中获取必要的数据
        wall_offset <- as.numeric(input_data$wall_offset)
        wall_grout <- as.numeric(input_data$wall_grout)
        obstacles_data <- input_data$obstacles

        # 创建 wall_and_tile_details 数据框
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
            tile_size_value,
            tile_size_value,
            full_tiles,
            split_tiles
          )
        )

        # 准备障碍物数据
        if (!is.null(obstacles_data) && length(obstacles_data) > 0) {
          obstacle_details <- data.frame(
            Parameter = paste0("Obstacle ", seq_along(obstacles_data)),
            Value = sapply(obstacles_data, function(ob)
              paste0(
                "Top: ", ob$top,
                ", Left: ", ob$left,
                ", Width: ", ob$width,
                ", Height: ", ob$height
              ))
          )
          colnames(obstacle_details) <- NULL
          obstacle_table_grob <- gridExtra::tableGrob(obstacle_details, rows = NULL)
        } else {
          obstacle_table_grob <- grid::textGrob("No obstacles", gp = grid::gpar(fontsize = 12, col = "red"))
        }

        # 移除 wall_tile_details 的列名
        colnames(wall_tile_details) <- NULL
        wall_tile_table_grob <- gridExtra::tableGrob(wall_tile_details, rows = NULL)

        # 定义 draw_tiles_and_box() 函数
        # draw_tiles_and_box <- function() {
        #   # 设置绘图窗口
        #   plot.new()
        #   plot.window(
        #     xlim = c(0, adjusted_ww),
        #     ylim = c(0, adjusted_wh),
        #     asp = adjusted_ww / adjusted_wh
        #   )
        #
        #   # 绘制瓷砖
        #   y_position <- offset_y
        #   row_counter <- 1
        #   while (y_position < adjusted_wh + 100) {
        #     x_position <- offset_x + ifelse(row_counter %% 2 == 0, wall_offset * scale_factor, 0)
        #     while (x_position < adjusted_ww + 100) {
        #       # 绘制每块瓷砖
        #       polygon(
        #         c(
        #           x_position,
        #           x_position,
        #           x_position + adjusted_tw,
        #           x_position + adjusted_tw
        #         ),
        #         c(
        #           y_position,
        #           y_position + adjusted_th,
        #           y_position + adjusted_th,
        #           y_position
        #         ),
        #         col = tile_color,
        #         border = "black"
        #       )
        #
        #       # 移动到下一个瓷砖
        #       x_position <- x_position + adjusted_tw + wall_grout * scale_factor
        #     }
        #     y_position <- y_position + adjusted_th + wall_grout * scale_factor
        #     row_counter <- row_counter + 1
        #   }
        #
        #   # 绘制墙壁边框
        #   rect(
        #     0,
        #     0,
        #     adjusted_ww,
        #     adjusted_wh,
        #     border = "red",
        #     lwd = 3
        #   )
        #
        #   # 绘制障碍物
        #   if (!is.null(obstacles_data) && length(obstacles_data) > 0) {
        #     for (obstacle in obstacles_data) {
        #       obstacle_top <- as.numeric(obstacle$top) * scale_factor
        #       obstacle_left <- as.numeric(obstacle$left) * scale_factor
        #       obstacle_width <- as.numeric(obstacle$width) * scale_factor
        #       obstacle_height <- as.numeric(obstacle$height) * scale_factor
        #
        #       rect(
        #         obstacle_left,
        #         adjusted_wh - obstacle_top,
        #         obstacle_left + obstacle_width,
        #         adjusted_wh - obstacle_top - obstacle_height,
        #         col = "orange",
        #         border = "black",
        #         lwd = 2
        #       )
        #     }
        #   }
        # }

        # 定义 wall_plot() 函数
        wall_plot <- function() {
          par(mar = c(4, 4, 4, 4))
          draw_tiles_and_box()
        }

        # 打开图形设备
        png(file, width = 800, height = 1200)

        # 设置布局
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(3, 1)))

        # 加载所需库
        library(grid)
        library(gridBase)
        library(gridExtra)

        # 绘制第一个视图（墙壁绘图）
        vp1 <- viewport(layout.pos.row = 1)
        pushViewport(vp1)
        par(new = TRUE, fig = gridFIG())
        wall_plot()
        popViewport()

        # 绘制第二个视图（墙壁和瓷砖表格）
        vp2 <- viewport(layout.pos.row = 2)
        pushViewport(vp2)
        grid.draw(wall_tile_table_grob)
        popViewport()

        # 绘制第三个视图（障碍物表格或消息）
        vp3 <- viewport(layout.pos.row = 3)
        pushViewport(vp3)
        grid.draw(obstacle_table_grob)
        popViewport()

        # 关闭图形设备
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
    output$tileCostSum <- renderText({
      paste("Tile Cost Summary:", values$tile_cost_sum)
    })
  })
}

# Extracted function to calculate adjusted dimensions
calculate_adjusted_dimensions <- function(input_data, session) {
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

  # # 根据选择的图案设置瓷砖尺寸
  # if (pattern == "Stack") {
  #   th <- tile_size_value
  #   tw <- tile_size_value
  # } else if (pattern == "Herringbone") {
  #   th <- tile_size_value
  #   tw <- tile_size_value * 2
  # } else if (pattern == "Basketweave") {
  #   th <- tile_size_value
  #   tw <- tile_size_value * 3 + wall_grout * 2
  # } else if (pattern == "Lattice") {
  #   th <- tile_size_value
  #   tw <- tile_size_value
  # }

  # th <- th
  # tw <- th

  tw <- th  # Assuming square tiles

  max_height <- 0.9 * session$clientData$output_wallPlot_height
  max_width <- 0.9 * session$clientData$output_wallPlot_width
  scale_factor <- min(max_height / wh, max_width / ww, 1)

  adjusted_wh <- wh * scale_factor
  adjusted_ww <- ww * scale_factor
  adjusted_th <- th * scale_factor
  adjusted_tw <- tw * scale_factor

  list(
    adjusted_wh = adjusted_wh,
    adjusted_ww = adjusted_ww,
    adjusted_th = adjusted_th,
    adjusted_tw = adjusted_tw,
    scale_factor = scale_factor
  )
}

# Extracted function to set up movement controls
setup_movement_controls <- function(input, values, recalculate_tiles) {
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
}

# Extracted function to recalculate tile counts
recalculate_tiles <- function(input_data, values) {
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

# Extracted function to draw tiles and wall boundary
draw_tiles_and_box <- function(input_data, values) {
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
  pattern_result <- horizontalStack_server(
    id = "horizontalStack",
    wall_height = values$adjusted_wh,
    wall_width = values$adjusted_ww,
    tile_height = values$adjusted_th,
    tile_width = values$adjusted_tw,
    tile_spacing = (wall_grout*values$scale_factor),
    offset = (wall_offset*values$scale_factor),
    tile_color = values$tile_color,
    offset_x = values$offset_x,
    offset_y = values$offset_y
  )

  # Draw the tiles across the wall area with offsets
  # {
  #   y_position <- values$offset_y
  #   row_counter <- 1
  #   while (y_position < values$adjusted_wh + 100) {
  #     x_position <- values$offset_x + ifelse(row_counter %% 2 == 0,
  #                                            wall_offset * values$scale_factor,
  #                                            0)
  #     while (x_position < values$adjusted_ww + 100) {
  #       # Draw each tile
  #       polygon(
  #         c(
  #           x_position,
  #           x_position,
  #           x_position + values$adjusted_tw,
  #           x_position + values$adjusted_tw
  #         ),
  #         c(
  #           y_position,
  #           y_position + values$adjusted_th,
  #           y_position + values$adjusted_th,
  #           y_position
  #         ),
  #         col = values$tile_color,
  #         border = "black"
  #       )
  #
  #       # Move to the next tile horizontally
  #       x_position <- x_position + values$adjusted_tw + wall_grout * values$scale_factor
  #     }
  #     # Move to the next row vertically
  #     y_position <- y_position + values$adjusted_th + wall_grout * values$scale_factor
  #     row_counter <- row_counter + 1
  #   }
  # }

  # if (userInput_server_return_values$pattern_dropdown() == "Stack") {
  #   horizontalStack_server(
  #     id = "horizontalStack",
  #     wall_height = values$adjusted_wh,
  #     wall_width = values$adjusted_ww,
  #     tile_height = values$adjusted_th,
  #     tile_width = values$adjusted_tw,
  #     tile_spacing = wall_grout,
  #     offset = wall_offset * values$scale_factor,
  #     tile_color = values$tile_color,
  #     offset_x = values$offset_x,
  #     offset_y = values$offset_y,
  #   )
  # } else if (userInput_server_return_values$pattern_dropdown() == "Herringbone") {
  #   herringbone_server(
  #     id = "herringbone",
  #     wall_height = userInput_server_return_values$wall_height,
  #     wall_width = userInput_server_return_values$wall_width,
  #     tile_height = userInput_server_return_values$tile_height,
  #     tile_width = userInput_server_return_values$tile_width,
  #     tile_spacing = userInput_server_return_values$tile_spacing,
  #     tile_color = userInput_server_return_values$tile_color,
  #     tile_color_2 = userInput_server_return_values$tile_color_2,
  #   )
  # } else if (userInput_server_return_values$pattern_dropdown() == "Basketweave") {
  #   basketweave_server(
  #     id = "basketweave",
  #     wall_height = userInput_server_return_values$wall_height,
  #     wall_width = userInput_server_return_values$wall_width,
  #     tile_height = userInput_server_return_values$tile_height,
  #     tile_width = userInput_server_return_values$tile_width,
  #     tile_spacing = userInput_server_return_values$tile_spacing,
  #     tile_color = userInput_server_return_values$tile_color,
  #     tile_color_2 = userInput_server_return_values$tile_color_2,
  #   )
  # } else if (userInput_server_return_values$pattern_dropdown() == "Lattice") {
  #   lattice_server(
  #     id = "lattice",
  #     wall_height = userInput_server_return_values$wall_height,
  #     wall_width = userInput_server_return_values$wall_width,
  #     tile_height = userInput_server_return_values$tile_height,
  #     tile_spacing = userInput_server_return_values$tile_spacing,
  #     tile_color = userInput_server_return_values$tile_color,
  #     tile_color_2 = userInput_server_return_values$tile_color_2,
  #   )
  # }




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
