lattice_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      column(
        12,
        div(
          style = "display: flex; justify-content: center; align-items: center; flex-direction: column; height: 100%;",

          uiOutput(ns("dynamicWallPlot")),

          div(
            style = "margin-top: 20px; text-align: center; display: flex; justify-content: center;",
            div(style = "margin-right: 20px;", textOutput(ns("fullTiles1Count"))),
            div(style = "margin-left: 20px;", textOutput(ns("splitTiles1Count"))),
            div(style = "margin-right: 20px;", textOutput(ns("fullTiles2Count"))),
            div(style = "margin-left: 20px;", textOutput(ns("splitTiles2Count"))),
            div(style = "margin-left: 20px;", textOutput(ns("tileCostSum"))),
          ),
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
        style = "display: flex; justify-content: space-between; align-items: center; width: 320px;",  # Adjust width as needed
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
}

lattice_server <- function(id, wall_height, wall_width, tile_height, tile_spacing, tile_color, tile_color_2, obstacles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define ns inside the moduleServer function

    values <- reactiveValues(
      box_x = 0,
      box_y = 0,
      offset_x = 0,
      offset_y = 0,
      full_tiles_1 = 0,
      split_tiles_1 = 0,
      full_tiles_2 = 0,
      split_tiles_2 = 0,
      tile_cost_sum = 0
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

      tile_1_list <- list()
      tile_2_list <- list()
      full_tiles_1 <- 0
      split_tiles_1 <- 0
      full_tiles_2 <- 0
      split_tiles_2 <- 0

      draw_intersection_tile <- function(x, y) {
        polygon(
          c(x, x + th*(sqrt(2)/2), x, x - th*(sqrt(2)/2)),
          c(y, y + th*(sqrt(2)/2), y + th*sqrt(2), y + th*(sqrt(2)/2)),
          col = tc2,
          border = "black"
        )
        group_data <- list(c(x, y), c(x + th*(sqrt(2)/2), y + th*(sqrt(2)/2)), c(x, y + th*sqrt(2)), c(x - th*(sqrt(2)/2), y + th*(sqrt(2)/2)))
        tile_2_list <<- c(tile_2_list, list(group_data))
      }

      draw_right_bar_tile <- function(x, y) {
        polygon(
          c(x, x + ts*(sqrt(2)/2), x + (ts-th) * (sqrt(2)/2), x - th*(sqrt(2)/2)),
          c(y, y + ts*(sqrt(2)/2), y + (ts+th) * (sqrt(2)/2), y + th*(sqrt(2)/2)),
          col = tc,
          border = "black"
        )
        group_data <- list(c(x, y), c(x + ts*(sqrt(2)/2), y + ts*(sqrt(2)/2)), c(x + (ts-th) * (sqrt(2)/2), y + (ts+th) * (sqrt(2)/2)), c(x - th*(sqrt(2)/2), y + th*(sqrt(2)/2)))
        tile_1_list <<- c(tile_1_list, list(group_data))
      }

      draw_left_bar_tile <- function(x, y) {
        polygon(
          c(x, x + th*(sqrt(2)/2), x + (th-ts) * (sqrt(2)/2), x - ts*(sqrt(2)/2)),
          c(y, y + th*(sqrt(2)/2), y + (ts+th) * (sqrt(2)/2), y + ts*(sqrt(2)/2)),
          col = tc,
          border = "black"
        )
        group_data <- list(c(x, y), c(x + th*(sqrt(2)/2), y + th*(sqrt(2)/2)), c(x + (th-ts) * (sqrt(2)/2), y + (ts+th) * (sqrt(2)/2)), c(x - ts*(sqrt(2)/2), y + ts*(sqrt(2)/2)))
        tile_1_list <<- c(tile_1_list, list(group_data))
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

      observe({
        # 调用 tileCountAndCost 模块
        tile_count_result <- tileCountAndCost(
          box_x = box_x,
          box_y = box_y,
          ww = ww,
          wh = wh,
          tile_1_list = tile_1_list,  # 传递你的数据
          tile_1_cost = 10,
          tile_2_list = tile_2_list,
          tile_2_cost = 5,
          tile_3_list = NULL,
          tile_3_cost = NULL,
          tile_4_list = NULL,
          tile_4_cost = NULL
        )

        # 将结果存入 reactiveValues
        values$full_tiles_1 <- tile_count_result$full_tiles_1
        values$split_tiles_1 <- tile_count_result$split_tiles_1
        values$full_tiles_2 <- tile_count_result$full_tiles_2
        values$split_tiles_2 <- tile_count_result$split_tiles_2
        values$tile_cost_sum <- tile_count_result$tile_cost_sum
      })


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

    output$fullTiles1Count <- renderText({
      paste("Full tiles 1:", values$full_tiles_1)
    })

    output$splitTiles1Count <- renderText({
      paste("Split tiles 1:", values$split_tiles_1)
    })

    output$fullTiles2Count <- renderText({
      paste("Full tiles 2:", values$full_tiles_2)
    })

    output$splitTiles2Count <- renderText({
      paste("Split tiles 2:", values$split_tiles_2)
    })

    output$tileCostSum <- renderText({
      paste("Tile Cost Summary:", values$tile_cost_sum)
    })
  })
}
