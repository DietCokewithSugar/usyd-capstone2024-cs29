lattice_server <- function(
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
    # offset_y = 0,
    full_tiles_1 = 0,
    split_tiles_1 = 0,
    full_tiles_2 = 0,
    split_tiles_2 = 0,
    tile_cost_sum = 0
  )


  wh <- wall_height
  ww <- wall_width
  th <- tile_height
  ts <- tile_spacing
  tc <- tile_color
  tc2 <- tile_color

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

  y_position <- -(ts + th) + offset_y
  row_counter <- 1
  while (y_position <= wh + ts + th) {
    x_position <- ifelse(row_counter %% 2 == 0, -th + (ts+th)*(sqrt(2)/2), -th)  + offset_x
    while (x_position <= ww + ts + th) {
      draw_unit(x_position, y_position)
      x_position <- x_position + (ts+th)*sqrt(2)
    }
    y_position <- y_position + (ts+th)*(sqrt(2)/2)
    row_counter <- row_counter + 1
  }

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

  return(
    list(
      full_tiles = values$full_tiles_1 + values$full_tiles_2,
      split_tiles = values$split_tiles_1 + values$split_tiles_2,
      tile_cost_sum = values$tile_cost_sum
    )
  )
}
