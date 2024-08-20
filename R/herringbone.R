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
        )
      )
    )
  )
}

herringbone_server <- function(id, wall_height, wall_width, tile_height, tile_width, tile_spacing, offset, obstacles) {
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
      # off <- offset()

      box_x <- values$box_x
      box_y <- values$box_y

      plot.new()
      plot.window(xlim = c(0, ww), ylim = c(0, wh))

      # base point on top left
      draw_horizontal_tile <- function(x, y) {
        polygon(
          c(x, x + tw, x + tw, x),
          c(y - th, y - th, y, y),
          col = "beige",
          border = "black"
        )
      }

      # base point on top left
      draw_vertical_tile <- function(x, y) {
        polygon(

          c(x, x + th, x + th, x),
          c(y - tw, y - tw, y, y),
          col = "brown",
          border = "black"
        )
      }

      # base point on top left
      draw_unit <- function(x, y) {
        draw_horizontal_tile(x + th + ts, y)
        draw_vertical_tile(x, y)
      }


      x_position <- -(tw + th + ts)
      y_position <- wh + tw + th + ts


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
    }, height = wall_height(), width = wall_width())
  })
}










# generators[['Herringbone']] <- \(area, intGrout, pattern=1, scale) {
#   x <- area[['x']][[1]]
#   y <- area[['y']][[1]]
#   # Based on vertical tile
#   height <- 1 + intGrout
#   width <- 0.5
#   horiTile <- makeTile(height,width,scale)
#   vertTile <- makeTile(width,height,scale)
#   while (y < area[['y']][[2]]) {
#     while (x < area[['x']][[2]]) {
#       if (pattern == 1) {
#         # Vertical, Skip, Horizontal
#         checkDrawTile(vertTile,area,x,y,scale)
#         x <- x + (width*2) + (intGrout*2)
#         checkDrawTile(horiTile,area,x,y,scale)
#         x <- x + height + intGrout
#       } else if (pattern == 2) {
#         # Skip, Horizontal, Vertical
#         x <- x + width + intGrout
#         checkDrawTile(horiTile,area,x,y,scale)
#         x <- x + height + intGrout
#         checkDrawTile(vertTile,area,x,y,scale)
#         x <- x + width + intGrout
#       } else if (pattern == 3) {
#         # Horizontal, Vertical, Skip
#         checkDrawTile(horiTile,area,x,y,scale)
#         x <- x + height + intGrout
#         checkDrawTile(vertTile,area,x,y,scale)
#         x <- x + (width*2) + (intGrout*2)
#       } else { # pattern == 4
#         # Skip, Vertical, Skip, Horizontal
#         x <- x + width + intGrout
#         checkDrawTile(vertTile,area,x,y,scale)
#         x <- x + (width*2) + (intGrout*2)
#         checkDrawTile(horiTile,area,x,y,scale)
#         x <- x + (height-width)
#       }
#     }
#     # Go up a level
#     y <- y + width + intGrout
#     x <- area[['x']][[1]]
#     pattern <- pattern + 1
#     if (pattern == 5) {
#       pattern <- 1
#     }
#   }
# }