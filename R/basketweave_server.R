basketweave_server <- function(
  id,
  wall_height,
  wall_width,
  tile_height,
  tile_spacing,
  tile_color,
  offset_x,
  offset_y) {

  values <- reactiveValues(
    box_x = 0,
    box_y = 0,
    # offset_x = 0,
    # offset_y = 0,
    full_tiles = 0,
    split_tiles = 0,
    tile_cost_sum = 0
  )



  wh <- wall_height
  ww <- wall_width
  th <- tile_height  # Assume height is shorter dimension for horizontal tiles
  ts <- tile_spacing
  tc <- tile_color
  tc2 <- tile_color

  #尝试
  tw <- th*3 + ts*2

  box_x <- values$box_x
  box_y <- values$box_y



  tile_list <- list()

  # base point on top left
  draw_horizontal_tile <- function(x, y) {
    # 左下
    polygon(
      c(x, x + tw, x + tw, x),
      c(y, y, y + th, y + th),
      col = tc,
      border = "black"
    )
    group_data <- list(c(x, y), c(x + tw, y), c(x + tw, y + th), c(x, y + th))
    tile_list <<- c(tile_list, list(group_data))
  }

  draw_vertical_tile <- function(x, y) {
    # 左下
    polygon(
      c(x, x + th, x + th, x),
      c(y, y, y + tw, y + tw),
      col = tc2,
      border = "black"
    )
    group_data <- list(c(x, y), c(x + th, y), c(x + th, y + tw), c(x, y + tw))
    tile_list <<- c(tile_list, list(group_data))
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


  y_position <- - uhw*2 - ts*2 + offset_y
  while (y_position <= wh + uhw*2 + ts*2) {
    x_position <- - uhw*2 - ts*2 + offset_x

    while (x_position <= ww + uhw*2 + ts*2) {
      draw_unit(x_position, y_position)
      x_position <- x_position + uhw + ts
    }

    y_position <- y_position + uhw + ts
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
