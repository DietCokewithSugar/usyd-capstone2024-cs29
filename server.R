library(shiny)

source("R/userInput.R")
source("R/horizontalStack.R")
source("R/herringbone.R")
source("R/BasketWeave.R")
source("R/lattice.R")
source("R/obstacles.R")

server <- function(input, output, session) {
  herringbone_sv <- reactiveValues(tile_width = 40, tile_height = 20, tile_ratio = 2, input_type = NULL)
  userInput_server_return_values <- userInput_server("userInput",herringbone_sv)
  obstaclesServer_return_values <- obstaclesServer("obstacles")
  observe({
    print(obstaclesServer_return_values())
  })

  output$dynamicUI <- renderUI({
    # Assuming there is a variable `someVar` from the user input that determines which UI to show
    if (userInput_server_return_values$pattern_dropdown() == "Stack") {
      horizontalStack_ui("horizontalStack")
    } else if (userInput_server_return_values$pattern_dropdown() == "Herringbone") {
      herringbone_ui("herringbone")
    } else if (userInput_server_return_values$pattern_dropdown() == "Basketweave") {
      horizontalStack_ui("basketweave")
    } else if (userInput_server_return_values$pattern_dropdown() == "Lattice") {
      lattice_ui("lattice")
    } else {
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
    } else if (userInput_server_return_values$pattern_dropdown() == "Herringbone") {
      herringbone_server(
        id = "herringbone",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        herringbone_sv = herringbone_sv,
        tile_spacing = userInput_server_return_values$tile_spacing,
        tile_color = userInput_server_return_values$tile_color,
        tile_two_color = userInput_server_return_values$tile_two_color,
        obstacles = obstaclesServer_return_values,
        input_session = userInput_server_return_values$session
      )
    } else if (userInput_server_return_values$pattern_dropdown() == "Basketweave") {
      horizontalStack_server(
        id = "basketWeave",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_width = userInput_server_return_values$tile_width,
        tile_spacing = userInput_server_return_values$tile_spacing,
        offset = userInput_server_return_values$offset,
        obstacles = obstaclesServer_return_values
      )
    } else if (userInput_server_return_values$pattern_dropdown() == "Lattice") {
      lattice_server(
        id = "lattice",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_width = userInput_server_return_values$tile_width,
        tile_spacing = userInput_server_return_values$tile_spacing,
        tile_color = userInput_server_return_values$tile_color,
        tile_two_color = userInput_server_return_values$tile_two_color,
        obstacles = obstaclesServer_return_values
      )
    } else {
      # other_module_server("otherModule", ...)  # Assuming other module server is defined
    }
  })



}