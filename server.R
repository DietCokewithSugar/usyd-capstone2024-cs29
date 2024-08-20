library(shiny)

source("R/userInput.R")
source("R/horizontalStack.R")
<<<<<<< Updated upstream
=======
source("R/BasketWeave.R")
>>>>>>> Stashed changes
source("R/obstacles.R")

server <- function(input, output, session) {
  
  userInput_server_return_values <- userInput_server("userInput")
  obstaclesServer_return_values <- obstaclesServer("obstacles")
  observe({
    print(obstaclesServer_return_values())
  })
  
  output$dynamicUI <- renderUI({
    # Assuming there is a variable `someVar` from the user input that determines which UI to show
    if (userInput_server_return_values$pattern_dropdown() == "Stack") {
      horizontalStack_ui("horizontalStack")
<<<<<<< Updated upstream
    } else {
=======
    }else if(userInput_server_return_values$pattern_dropdown() == "Basketweave") {
      horizontalStack_ui("BasketWeave")
    }else {
>>>>>>> Stashed changes
      # other_module_ui("otherModule")  # Assuming another module UI is defined
    }
  })
  
  observe({
    if (userInput_server_return_values$pattern_dropdown() == "Stack") {
      horizontalStack_server(
        id = "horizontalStack",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_width = userInput_server_return_values$tile_width,
        tile_spacing = userInput_server_return_values$tile_spacing,
        offset = userInput_server_return_values$offset,
        obstacles = obstaclesServer_return_values
      )
<<<<<<< Updated upstream
    } else {
=======
    } else if (userInput_server_return_values$pattern_dropdown() == "Basketweave") {
      horizontalStack_server(
        id = "BasketWeave",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_width = userInput_server_return_values$tile_width,
        tile_spacing = userInput_server_return_values$tile_spacing,
        offset = userInput_server_return_values$offset,
        obstacles = obstaclesServer_return_values
      )
    } 
    
    else {
>>>>>>> Stashed changes
      # other_module_server("otherModule", ...)  # Assuming other module server is defined
    }
  })
  
  
  
}
