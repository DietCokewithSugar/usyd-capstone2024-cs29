# Example module 1

source("~/Desktop/ShinyTest/R/tileGenerator.R")

module1_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # actionButton(inputId = ns("my_button"), label = "Click Me"),
    # # imageOutput("tile_image")
    
    selectInput(ns("TilePatternDropdown"), "Choose a Tile Pattern:",
                choices = c("None", "Tile Pattern 1", "Tile Pattern 2", "Tile Pattern 3")),

    conditionalPanel(
      condition = paste0("input['", ns("TilePatternDropdown"), "'] != 'None'"),
      numericInput(ns("wallHeight"), "Height of Wall", value = NULL, min = 0),
      numericInput(ns("wallWidth"), "Width of Wall", value = NULL, min = 0),
      sliderInput(ns("tileHeight"), "Height of Tile", min = 0, max = 100, value = 50),
      sliderInput(ns("tileWidth"), "Width of Tile", min = 0, max = 100, value = 50),
      selectInput(ns("ObstacleDropdown"), "Does it have any obstacle?",
                  choices = c("None", "Yes", "No")),

      conditionalPanel(
        condition = paste0("input['", ns("ObstacleDropdown"), "'] == 'Yes'"),
        textInput(ns("obstacleHeight"), "Height of Obstacle"),
        textInput(ns("obstacleWidth"), "Width of Obstacle")
      ),
    
    numericInput(ns("numBoxes"), "Enter number of boxes:", value = 1, min = 1)
    
    )
    
  )
}

module1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # message_reactive <- eventReactive(input$text, {
    #   generate_message(input$text)
    # })
    # 
    # output$mod1output <- renderText({
    #   message_reactive()
    # })
    
    # observeEvent(input$my_button, {
    #   print("button clicked")
    #   generate_tile()
    #   # output$tile_image <- renderImage({
    #   #   # Call the function to generate the tile and save it as a PNG
    #   #   file_path <- generate_tile()  # This will return the file path to the PNG
    #   #   
    #   #   # Return a list with the file path to be displayed
    #   #   list(src = file_path,
    #   #        contentType = 'image/png',
    #   #        alt = "Generated Tile")
    #   # }, deleteFile = TRUE)  # Set to TRUE if you want to delete the file after rendering
    # })
    
    # Reactive expressions for wall height and width
    wallHeight <- reactive({ input$wallHeight })
    wallWidth <- reactive({ input$wallWidth })
    tileHeight <- reactive({ input$tileHeight })
    tileWidth <- reactive({ input$tileWidth })
    # numBoxes <- reactive({ input$numBoxes })

    return(list(
      # message = message_reactive,

      wallHeight = wallHeight,
      wallWidth = wallWidth,
      tileWidth = tileWidth,
      tileHeight = tileHeight
      # numBoxes = numBoxes
    ))
    
  })
}