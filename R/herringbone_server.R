herringbone_server <- function(
  id,
  wall_height,
  wall_width,
  tile_height,
  tile_width,
  tile_spacing,
  tile_color,
  offset_x,
  offset_y) {

  values <- reactiveValues(
    box_x = 0,
    box_y = 0,
    # offset_x = 0,
    # offset_y = 0
  )



  wh <- wall_height
  ww <- wall_width
  th <- tile_height  # Assume height is shorter dimension for horizontal tiles
  tw <- tile_width   # Assume width is longer dimension for horizontal tiles
  ts <- tile_spacing
  tc <- tile_color
  tc2 <- tile_color

  box_x <- values$box_x
  box_y <- values$box_y

  plot.new()
  plot.window(xlim = c(0, ww), ylim = c(0, wh))


  tile_list <- list()


  # base point on top left
  draw_horizontal_tile <- function(x, y) {
    polygon(
      c(x, x + tw, x + tw, x),
      c(y - th, y - th, y, y),
      col = tc,
      border = "black"
    )
    group_data <- list(c(x, y - th), c(x + tw, y - th), c(x + tw, y), c(x, y))
    tile_list <<- c(tile_list, list(group_data))
  }

  # base point on top left
  draw_vertical_tile <- function(x, y) {
    polygon(

      c(x, x + th, x + th, x),
      c(y - tw, y - tw, y, y),
      col = tc2,
      border = "black"
    )
    group_data <- list(c(x, y - tw), c(x + th, y - tw), c(x + th, y), c(x, y))
    tile_list <<- c(tile_list, list(group_data))
  }

  # base point on top left
  draw_unit <- function(x, y) {
    draw_horizontal_tile(x + th + ts, y)
    draw_vertical_tile(x, y)
  }


  x_position <- -(tw + th + ts) + offset_x
  y_position <- wh + tw + th + ts + offset_y


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

  # 调用 tileCountAndCost 模块
  tile_count_result <- tileCountAndCost(
    box_x = box_x,
    box_y = box_y,
    ww = ww,
    wh = wh,
    tile_1_list = tile_list,  # 传递你的数据
    tile_1_cost = 10,
    tile_2_list = NULL,
    tile_2_cost = NULL,    # 或者传递正确的值
    tile_3_list = NULL,
    tile_3_cost = NULL,
    tile_4_list = NULL,
    tile_4_cost = NULL
  )



  # 将结果存入 reactiveValues
  values$full_tiles <- tile_count_result$full_tiles_1
  values$split_tiles <- tile_count_result$split_tiles_1
  values$tile_cost_sum <- tile_count_result$tile_cost_sum


  return(
    list(
      full_tiles = values$full_tiles,
      split_tiles = values$split_tiles,
      tile_cost_sum = values$tile_cost_sum
    )
  )
}