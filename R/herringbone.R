herringbone_ui <- function(id) {
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
        ),
        actionButton(
          ns("reset"),
          "",
          icon = icon("refresh"),
          style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
        )
      )
    ),
    fluidPage(
      # tags$head(
      #   tags$style(HTML("
      #     #right_slider .jslider {
      #       transform: rotate(270deg);
      #       height: 200px;
      #     }
      #     #right_slider .jslider-pointer {
      #       transform: translate(-10px, 0) !important;
      #     }
      #   "))
      # ),
      fluidRow(

      )
    )
  )
}

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
