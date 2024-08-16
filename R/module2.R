# Example module 2


module2_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # numericInput(ns("numBoxes2"), "Enter number of boxes:", value = 1, min = 1),
    uiOutput(ns("boxes"))  # Placeholder for the dynamic boxes
    
  )
}

module2_server <- function(id, wallHeight, wallWidth, tileWidth, tileHeight)  {
  
  
  
  moduleServer(id, function(input, output, session) {
    
    boxesBox <- reactive({
      height <- as.numeric(wallHeight())  # Ensure wallHeight is numeric
      width <- as.numeric(wallWidth())
      tileWidth <- as.numeric(tileWidth())  # Ensure wallHeight is numeric
      tileHeight <- as.numeric(tileHeight()) # Ensure wallWidth is numeric
      
      # Set a fixed size for each tile
      fixedTileHeight <- tileWidth  # Example fixed height for each tile (in pixels)sidecar
      fixedTileWidth <- tileHeight   # Example fixed width for each tile (in pixels)
      
      # Handle the case where height or width is missing or not valid
      if (is.na(height) || height <= 0 || is.na(width) || width <= 0) {
        return(NULL)  # Return NULL if any of the values are invalid
      }
      
      # Calculate the number of tiles that can fit in each dimension
      numCols <- floor(width / fixedTileWidth)  # Number of columns
      numRows <- floor(height / fixedTileHeight)  # Number of rows
      
      # Calculate the total number of tiles
      n <- numCols * numRows
      
      # Generate the tiles
      lapply(1:n, function(i) {
        div(style = paste0(
          "border: 1px solid blue; ",
          "padding: 5px; ",
          "margin: 2px; ",
          "display: inline-block; ",
          "width: ", fixedTileWidth - 10, "px; ",  # Adjust for padding
          "height: ", fixedTileHeight - 10, "px; ",  # Adjust for padding
          "box-sizing: border-box; ",
          "background-color: lightgray;"
        ),
        paste("Tile", i)
        )
      })
    })
    
    output$boxes <- renderUI({
      width <- as.numeric(wallWidth())
      height <- as.numeric(wallHeight())
      
      # Ensure that width and height are valid before rendering the wall
      if (is.na(width) || width <= 0 || is.na(height) || height <= 0) {
        return(NULL)  # Return NULL if the wall dimensions are invalid
      }
      
      div(style = paste0("width: ", width, "px; height: ", height, "px; border: 2px solid black; padding: 5px;"),
          do.call(tagList, boxesBox())
      )
    })
    
    # Render the UI for the boxes
    # output$boxes <- renderUI({
    #   do.call(tagList, boxesBox())  # Combine the list of boxes into a tagList for rendering
    # })
    
  })
}

