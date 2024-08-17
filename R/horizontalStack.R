horizontalStack_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        plotOutput(ns("wallPlot"), height = "500px"),
        verbatimTextOutput(ns("info"))
      )
    ),
    
    absolutePanel(
      top = "calc(50% - 50px)",
      left = "calc(50% - 50px)",
      width = 100,
      height = 100,
      div(
        style = "position: relative;",
        actionButton(
          ns("up"),
          "^",
          icon = icon("arrow-up"),
          width = "50px",
          style = "position: absolute; top: 0px; left: 25px;"
        ),
        actionButton(
          ns("left"),
          "<",
          icon = icon("arrow-left"),
          width = "50px",
          style = "position: absolute; top: 50px; left: 0px;"
        ),
        actionButton(
          ns("right"),
          ">",
          icon = icon("arrow-right"),
          width = "50px",
          style = "position: absolute; top: 50px; left: 50px;"
        ),
        actionButton(
          ns("down"),
          "v",
          icon = icon("arrow-down"),
          width = "50px",
          style = "position: absolute; top: 100px; left: 25px;"
        )
      )
    )
  )
}


horizontalStack_server <- function(id, wall_height, wall_width, tile_height, tile_width, tile_spacing, offset) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for wall dimensions and position
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
    
    output$wallPlot <- renderPlot({
    
      wh <- wall_height()
      ww <- wall_width()
      th <- tile_height()
      tw <- tile_width()
      ts <- tile_spacing()
      off <- offset()
      
      # Set up the plotting area
      plot.new()
      plot.window(xlim = c(0, ww), ylim = c(0, wh))
      
      # Function to draw a tile at a specific position
      draw_tile <- function(x, y) {
        polygon(
          c(x, x, x + tw, x + tw),
          c(y, y + th, y + th, y),
          col = "lightblue",
          border = "black"
        )
      }
      
      # Loop to fill the wall with tiles in the background, including edges
      y_position <- -th  # Start slightly above the top to allow for sliced tiles
      row_counter <- 1  # To keep track of the row number
      while (y_position <= wh) {
        x_position <- ifelse(row_counter %% 2 == 0, -tw + off, -tw)  # Start slightly left to allow for sliced tiles and apply offset for alternate rows
        while (x_position <= ww) {
          draw_tile(x_position, y_position)
          x_position <- x_position + tw + ts
        }
        y_position <- y_position + th + ts
        row_counter <- row_counter + 1
      }
      
      # Draw the box representing the wall
      polygon(
        c(
          values$box_x,
          values$box_x,
          values$box_x + ww,
          values$box_x + ww
        ),
        c(
          values$box_y,
          values$box_y + wh,
          values$box_y + wh,
          values$box_y
        ),
        border = "red",
        lwd = 3,
        col = NA
      )
    })
    
    output$info <- renderText({
      paste("Box position: X =", round(values$box_x), "Y =", round(values$box_y))
    })
  })
}