horizontalStack_server <- function(
  id,
  wall_height,
  wall_width,
  tile_height,
  tile_width,
  tile_spacing,
  offset,
  tile_color,
  offset_x,
  offset_y) {

  values <- reactiveValues(
    # box_x = 0,
    # box_y = 0,
    # offset_x = 0,
    # offset_y = 0,
    full_tiles = 0,
    split_tiles = 0
  )

  wh <- wall_height
  ww <- wall_width
  th <- tile_height
  tw <- tile_width
  ts <- tile_spacing
  off <- offset
  tc <- tile_color

  # box_x <- box_x
  # box_y <- box_y

  plot.new()
  plot.window(xlim = c(0, ww), ylim = c(0, wh))

  tile_list <- list()
  full_tiles <- 0
  split_tiles <- 0

  draw_tile <- function(x, y) {
      polygon(
        c(x, x, x + tw, x + tw),
        c(y, y + th, y + th, y),
        col = tc,
        border = "black"
      )
    group_data <- list(c(x, y), c(x, y + th), c(x + tw, y + th), c(x + tw, y))
    tile_list <<- c(tile_list, list(group_data))
  }



  y_position <- -th + offset_y
  row_counter <- 1
  while (y_position <= wh + 100) {
    x_position <- ifelse(row_counter %% 2 == 0, -tw + off, -tw) + offset_x
    while (x_position <= ww + 100) {
      draw_tile(x_position, y_position)
      x_position <- x_position + tw + ts
    }
    y_position <- y_position + th + ts
    row_counter <- row_counter + 1
  }



  # 调用 tileCountAndCost 模块
  tile_count_result <- tileCountAndCost(
    box_x = offset_x,
    box_y = offset_y,
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
  full_tiles <- tile_count_result$full_tiles_1
  split_tiles <- tile_count_result$split_tiles_1
  tile_cost_sum <- tile_count_result$tile_cost_sum



  return(
    list(
      full_tiles = full_tiles,
      split_tiles = split_tiles,
      tile_cost_sum = tile_cost_sum
    )
  )
}