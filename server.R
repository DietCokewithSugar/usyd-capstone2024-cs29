library(shiny)

source("R/userInput.R")
source("R/horizontalStack.R")

server <- function(input, output, session) {
  
  userInput_server_return_values <- userInput_server("userInput")
  horizontalStack_server(
    id = "horizontalStack",
    wall_height = userInput_server_return_values$wall_height,
    wall_width = userInput_server_return_values$wall_width,
    tile_height = userInput_server_return_values$tile_height,
    tile_width = userInput_server_return_values$tile_width,
    tile_spacing = userInput_server_return_values$tile_spacing,
    offset = userInput_server_return_values$offset
  )
  
}
