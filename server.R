library(shiny)


source("R/controller.R")
source("R/designUI.R")

# 导入函数
source("functions/horizontalStack_server.R")
source("functions/herringbone_server.R")
source("functions/basketweave_server.R")
source("functions/lattice_server.R")
source("functions/tileCountAndCost.R")
# ... 导入其他函数 ...

server <- function(input, output, session) {
  userInput_server_return_values <- userInput_server("userInput")
  obstaclesServer_return_values <- obstaclesServer("obstacles", userInput = userInput_server_return_values)
  
  
  
  #dynamic page rendering starts
  
  current_ui <- reactiveVal("secondPage")
  page_data <- reactiveVal(NULL)
  
  switch_ui <- function(new_ui, values = NULL) {
    if (!is.null(values)) {
      page_data(values)
    }
    current_ui(new_ui)
  }
  
  observe({
    ui <- current_ui()
    cat("Switching to UI:", ui, "\n")  # Debugging log
    
    isolate({
      if (ui == "landingPage") {
        output$new_ui_container <- renderUI({
          landingPage_ui("landing_page")
        })
        landingPage_server("landing_page", selected_values = page_data(), switch_ui = switch_ui)
        
      } else if (ui == "secondPage") {
        output$new_ui_container <- renderUI({
          secondPage_ui("second_page")  # Pass stored values
        })
        secondPage_server("second_page", switch_ui = switch_ui)
        
      } else if (ui == "finalPage") {
        output$new_ui_container <- renderUI({
          finalPage_ui("final_page")
        })
        finalPage_server("final_page", input_data = page_data(), switch_ui = switch_ui)
      }
    })
  })
  
  #dynamic page rendering ends
  
  output$dynamicUI <- renderUI({
    # Assuming there is a variable `someVar` from the user input that determines which UI to show
    if (userInput_server_return_values$pattern_dropdown() == "Stack") {
      design_ui("horizontalStack")
    } else if (userInput_server_return_values$pattern_dropdown() == "Herringbone") {
      design_ui("herringbone")
    } else if (userInput_server_return_values$pattern_dropdown() == "Basketweave") {
      design_ui("basketweave")
    } else if (userInput_server_return_values$pattern_dropdown() == "Lattice") {
      design_ui("lattice")
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
        tile_color = userInput_server_return_values$tile_color,
        texture_option = userInput_server_return_values$texture_option,
        obstacles = obstaclesServer_return_values
      )
    } else if (userInput_server_return_values$pattern_dropdown() == "Herringbone") {
      herringbone_server(
        id = "herringbone",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_width = userInput_server_return_values$tile_width,
        tile_spacing = userInput_server_return_values$tile_spacing,
        tile_color = userInput_server_return_values$tile_color,
        tile_color_2 = userInput_server_return_values$tile_color_2,
        obstacles = obstaclesServer_return_values,
        input_session = userInput_server_return_values$session
      )
    } else if (userInput_server_return_values$pattern_dropdown() == "Basketweave") {
      basketweave_server(
        id = "basketweave",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_width = userInput_server_return_values$tile_width,
        tile_spacing = userInput_server_return_values$tile_spacing,
        tile_color = userInput_server_return_values$tile_color,
        tile_color_2 = userInput_server_return_values$tile_color_2,
        obstacles = obstaclesServer_return_values
      )
    } else if (userInput_server_return_values$pattern_dropdown() == "Lattice") {
      lattice_server(
        id = "lattice",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_spacing = userInput_server_return_values$tile_spacing,
        tile_color = userInput_server_return_values$tile_color,
        tile_color_2 = userInput_server_return_values$tile_color_2,
        obstacles = obstaclesServer_return_values
      )
    } else {
      # other_module_server("otherModule", ...)  # Assuming other module server is defined
    }
  })



}
