library(sf)

# 函数确保多边形闭合
close_polygon <- function(point_list) {
  if (!identical(point_list[[1]], point_list[[length(point_list)]])) {
    point_list <- append(point_list, list(point_list[[1]]))
  }
  return(point_list)
}

# 批量生成瓷砖多边形
generate_tile_polygons <- function(tile_list) {
  tile_polygons <- lapply(tile_list, function(point_list) {
    point_list <- close_polygon(point_list)
    return(st_polygon(list(matrix(unlist(point_list), ncol = 2, byrow = TRUE))))
  })
  return(st_sfc(tile_polygons))
}

tileCountAndCost <- function(box_x, box_y, ww, wh,
                             tile_1_list, tile_1_cost,
                             tile_2_list, tile_2_cost,
                             tile_3_list, tile_3_cost,
                             tile_4_list, tile_4_cost) {

  # 初始化变量
  full_tiles_1 <- 0
  split_tiles_1 <- 0
  full_tiles_2 <- 0
  split_tiles_2 <- 0
  full_tiles_3 <- 0
  split_tiles_3 <- 0
  full_tiles_4 <- 0
  split_tiles_4 <- 0
  tile_cost_sum <- 0  # 初始化tile_cost_sum

  # 定义墙的多边形
  wall_polygon <- st_polygon(list(matrix(c(box_x, box_y, box_x + ww, box_y, box_x + ww, box_y + wh, box_x, box_y + wh, box_x, box_y), ncol = 2, byrow = TRUE)))
  wall_polygon <- st_sfc(wall_polygon)

  # 通用的瓷砖检测函数
  calculate_tiles <- function(tile_list, full_tiles, split_tiles) {
    if (length(tile_list) == 0) return(list(full_tiles = 0, split_tiles = 0))

    # 批量生成瓷砖多边形
    tile_polygons <- generate_tile_polygons(tile_list)

    # 使用空间操作判断
    contains <- st_contains(wall_polygon, tile_polygons, sparse = FALSE)
    intersects <- st_intersects(tile_polygons, wall_polygon, sparse = FALSE)

    # 对每块瓷砖进行判断
    for (i in 1:length(tile_list)) {
      if (contains[1, i]) {
        # 如果完全包含在墙内
        full_tiles <- full_tiles + 1
      } else if (intersects[i, 1]) {
        # 如果部分相交但不完全包含
        split_tiles <- split_tiles + 1
      }
    }

    return(list(full_tiles = full_tiles, split_tiles = split_tiles))
  }

  # 计算 tile_1_list
  if (!is.null(tile_1_list)) {
    result <- calculate_tiles(tile_1_list, full_tiles_1, split_tiles_1)
    full_tiles_1 <- result$full_tiles
    split_tiles_1 <- result$split_tiles
  }

  # 计算 tile_2_list
  if (!is.null(tile_2_list)) {
    result <- calculate_tiles(tile_2_list, full_tiles_2, split_tiles_2)
    full_tiles_2 <- result$full_tiles
    split_tiles_2 <- result$split_tiles
  }

  # 计算 tile_3_list
  if (!is.null(tile_3_list)) {
    result <- calculate_tiles(tile_3_list, full_tiles_3, split_tiles_3)
    full_tiles_3 <- result$full_tiles
    split_tiles_3 <- result$split_tiles
  }

  # 计算 tile_4_list
  if (!is.null(tile_4_list)) {
    result <- calculate_tiles(tile_4_list, full_tiles_4, split_tiles_4)
    full_tiles_4 <- result$full_tiles
    split_tiles_4 <- result$split_tiles
  }

  # 计算 tile_cost_sum 总价
  tile_cost_sum <- (full_tiles_1 + split_tiles_1) * (tile_1_cost %||% 0) +
    (full_tiles_2 + split_tiles_2) * (tile_2_cost %||% 0) +
    (full_tiles_3 + split_tiles_3) * (tile_3_cost %||% 0) +
    (full_tiles_4 + split_tiles_4) * (tile_4_cost %||% 0)

  cat("tile_cost_sum = ", tile_cost_sum)

  return(
    list(
      full_tiles_1 = full_tiles_1,
      split_tiles_1 = split_tiles_1,
      full_tiles_2 = full_tiles_2,
      split_tiles_2 = split_tiles_2,
      full_tiles_3 = full_tiles_3,
      split_tiles_3 = split_tiles_3,
      full_tiles_4 = full_tiles_4,
      split_tiles_4 = split_tiles_4,
      tile_cost_sum = tile_cost_sum
    )
  )
}



tileCostComparison <- function(ww, wh, unit_area, unit_cost) {

  estimated_tile_cost <- 0

  estimated_tile_cost <- ww * wh / unit_area * unit_cost




  return(
    list(
      estimated_tile_cost = estimated_tile_cost
    )
  )
}
